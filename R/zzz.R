library(dplyr)
library(jsonlite)

# Internal package environment.
# This prevents repeated version checks within the same R session.
.tdx_state <- new.env(parent = emptyenv())

.tdx_state$version_checked <- FALSE


# Package startup hook -----------------------------------------------------

.onAttach <- function(libname, pkgname) {

  # Show the welcome message whenever TDX is attached.
  packageStartupMessage(
    paste0(
      "\n",
      "=====================================================\n",
      "              Welcome to TDX 🚍\n",
      "-----------------------------------------------------\n",
      "TDX: A toolkit for accessing and processing\n",
      "Taiwan MOTC TDX transportation data.\n",
      "\n",
      "Documentation:\n",
      "  https://github.com/ChiaJung-Yeh/NYCU_TDX\n",
      "\n",
      "Report issues:\n",
      "  https://github.com/ChiaJung-Yeh/NYCU_TDX/issues\n",
      "=====================================================\n"
    )
  )

  # Users can disable update checking with:
  #
  # options(TDX.check_updates = FALSE)
  check_enabled <- isTRUE(
    getOption("TDX.check_updates", TRUE)
  )

  # Only check for updates:
  # 1. in an interactive R session;
  # 2. when update checking is enabled;
  # 3. once per R session.
  if (
    interactive() &&
    check_enabled &&
    !isTRUE(.tdx_state$version_checked)
  ) {
    .tdx_state$version_checked <- TRUE

    check_tdx_version(
      libname = libname,
      pkgname = pkgname
    )
  }

  invisible(NULL)
}


# GitHub version checker ---------------------------------------------------

check_tdx_version <- function(
    libname = NULL,
    pkgname = "TDX"
) {

  github_owner <- "ChiaJung-Yeh"
  github_repo <- "NYCU_TDX"

  # Get the currently installed package version.
  current_version <- tryCatch(
    {
      if (is.null(libname)) {
        utils::packageVersion(pkgname)
      } else {
        utils::packageVersion(
          pkgname,
          lib.loc = libname
        )
      }
    },
    error = function(e) NULL
  )

  if (is.null(current_version)) {
    return(invisible(FALSE))
  }

  # GitHub API endpoint for the latest published release.
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/latest",
    github_owner,
    github_repo
  )

  # Retrieve the latest release information.
  response_text <- tryCatch(
    {
      connection <- url(
        api_url,
        open = "rb",
        headers = c(
          Accept = "application/vnd.github+json",
          `User-Agent` = "TDX-R-package-version-check",
          `X-GitHub-Api-Version` = "2022-11-28"
        )
      )

      on.exit(
        try(close(connection), silent = TRUE),
        add = TRUE
      )

      paste(
        readLines(
          connection,
          warn = FALSE,
          encoding = "UTF-8"
        ),
        collapse = "\n"
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Fail silently if there is no internet connection
  # or GitHub is temporarily unavailable.
  if (
    is.null(response_text) ||
    !nzchar(response_text)
  ) {
    return(invisible(FALSE))
  }

  # Parse the GitHub API response.
  release_information <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) NULL
  )

  if (
    is.null(release_information) ||
    is.null(release_information$tag_name) ||
    !nzchar(release_information$tag_name)
  ) {
    return(invisible(FALSE))
  }

  release_tag <- release_information$tag_name

  # Convert tags such as v1.2.0 or V1.2.0 to 1.2.0.
  latest_version_text <- sub(
    pattern = "^[vV]",
    replacement = "",
    x = release_tag
  )

  latest_version <- tryCatch(
    package_version(latest_version_text),
    error = function(e) NULL
  )

  if (is.null(latest_version)) {
    return(invisible(FALSE))
  }

  update_available <- latest_version > current_version

  if (update_available) {
    packageStartupMessage(
      sprintf(
        paste0(
          "\n",
          "A newer version of TDX is available on GitHub.\n",
          "\n",
          "  Installed version: %s\n",
          "  Latest version:    %s\n",
          "\n",
          "Update it with:\n",
          "  remotes::install_github(",
          "\"ChiaJung-Yeh/NYCU_TDX\"",
          ")\n"
        ),
        as.character(current_version),
        as.character(latest_version)
      )
    )
  }

  invisible(update_available)
}
