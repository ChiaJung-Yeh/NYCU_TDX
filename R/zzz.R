library(dplyr)
library(jsonlite)

.package_state <- new.env(parent = emptyenv())
.package_state$update_checked <- FALSE


.onAttach <- function(libname, pkgname) {
  if (
    interactive() &&
    isTRUE(getOption(paste0(pkgname, ".check_updates"), TRUE)) &&
    !isTRUE(.package_state$update_checked)
  ) {
    .package_state$update_checked <- TRUE

    check_github_version(
      pkgname = pkgname,
      libname = libname,
      owner = "ChiaJung-Yeh",
      repo = "NYCU_TDX"
    )
  }
}

check_github_version <- function(
    pkgname,
    libname,
    owner,
    repo
) {
  current_version <- utils::packageVersion(
    pkgname,
    lib.loc = libname
  )

  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/latest",
    owner,
    repo
  )

  response <- tryCatch(
    {
      con <- url(
        api_url,
        headers = c(
          Accept = "application/vnd.github+json",
          `User-Agent` = paste0(pkgname, "-version-check")
        )
      )

      on.exit(close(con), add = TRUE)

      paste(
        readLines(con, warn = FALSE),
        collapse = "\n"
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Fail silently when offline or when GitHub is unavailable.
  if (is.null(response)) {
    return(invisible(FALSE))
  }

  release <- tryCatch(
    jsonlite::fromJSON(response),
    error = function(e) NULL
  )

  if (
    is.null(release) ||
    is.null(release$tag_name)
  ) {
    return(invisible(FALSE))
  }

  # Converts tags such as "v1.2.3" into "1.2.3".
  latest_version_text <- sub(
    "^v",
    "",
    release$tag_name,
    ignore.case = TRUE
  )

  latest_version <- tryCatch(
    package_version(latest_version_text),
    error = function(e) NULL
  )

  if (is.null(latest_version)) {
    return(invisible(FALSE))
  }

  if (latest_version > current_version) {
    install_command <- sprintf(
      'remotes::install_github("%s/%s")',
      owner,
      repo
    )

    packageStartupMessage(
      sprintf(
        paste0(
          "A newer version of %s is available on GitHub: %s ",
          "(installed: %s).\n",
          "Update it with:\n  %s"
        ),
        pkgname,
        latest_version,
        current_version,
        install_command
      )
    )

    return(invisible(TRUE))
  }

  invisible(FALSE)
}


