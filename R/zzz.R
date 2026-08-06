library(dplyr)
library(jsonlite)

# Internal package environment.
# This prevents repeated checks within the same R session.
.tdx_state <- new.env(parent = emptyenv())

.tdx_state$version_checked <- FALSE


#' Package startup hook
#'
#' This function runs automatically when users call library(TDX).
#'
#' @param libname Path to the R library containing the package.
#' @param pkgname Package name.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {

  # Users can disable the version check with:
  # options(TDX.check_updates = FALSE)
  check_enabled <- isTRUE(
    getOption("TDX.check_updates", TRUE)
  )

  # Do not make internet requests in non-interactive sessions,
  # during automated checks, or more than once per R session.
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
}


#' Check for a newer TDX version on GitHub
#'
#' This internal function compares the installed package version with
#' the latest published GitHub Release.
#'
#' @param libname Path to the R library containing the package.
#' @param pkgname Package name.
#'
#' @return Invisibly returns TRUE when an update is available;
#' otherwise returns FALSE.
#'
#' @keywords internal
check_tdx_version <- function(
    libname = NULL,
    pkgname = "TDX"
) {

  github_owner <- "ChiaJung-Yeh"
  github_repo  <- "NYCU_TDX"

  # Obtain the currently installed version.
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

  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/latest",
    github_owner,
    github_repo
  )

  # Retrieve information about the latest published release.
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

  # Silently stop when offline or GitHub is unavailable.
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

  # Convert tags such as:
  # v1.2.0
  # V1.2.0
  # 1.2.0
  #
  # into:
  # 1.2.0
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
          "A newer version of TDX is available on GitHub.\n",
          "  Installed version: %s\n",
          "  Latest version:    %s\n",
          "Update it with:\n",
          "  remotes::install_github(",
          "\"ChiaJung-Yeh/NYCU_TDX\"",
          ")"
        ),
        as.character(current_version),
        as.character(latest_version)
      )
    )
  }

  invisible(update_available)
}


