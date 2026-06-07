#' Null-coalescing operator
#'
#' Returns \code{x} if it is not \code{NULL}, not length-zero, and not an empty
#' string; otherwise returns \code{y}.
#'
#' @param x First value.
#' @param y Fallback value.
#' @return \code{x} if it is "present", else \code{y}.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && identical(x, ""))) {
    y
  } else {
    x
  }
}

#' Download a binary file from a URL
#'
#' Downloads a file to the specified local path, creating parent directories as
#' needed.
#'
#' @param url URL to download from.
#' @param dest_path Local file path to save to.
#' @param timeout Timeout in seconds (default: 300).
#' @return The \code{dest_path}, invisibly.
#' @keywords internal
download_binary_file <- function(url, dest_path, timeout = 300) {
  dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)

  response <- httr::GET(
    url,
    httr::timeout(timeout),
    httr::write_disk(dest_path, overwrite = TRUE)
  )

  if (httr::http_error(response)) {
    stop("Failed to download file from ", url, " (status ", httr::status_code(response), ")")
  }

  invisible(dest_path)
}

#' Guess file extension from URL
#'
#' Extracts the file extension from a URL path. If no extension is found,
#' returns the fallback.
#'
#' @param url A URL string.
#' @param fallback Fallback extension (default: "jpg").
#' @return File extension string without a leading dot.
#' @keywords internal
guess_extension_from_url <- function(url, fallback = "jpg") {
  url_no_query <- sub("\\?.*$", "", url)
  extension <- tools::file_ext(url_no_query)
  if (!nzchar(extension)) fallback else extension
}

#' Resolve PaddleOCR API token
#'
#' Checks the explicit \code{token} parameter first; if empty, falls back to
#' the \code{PADDLE_OCR_TOKEN} environment variable.
#'
#' @param token Explicit token string (may be \code{""}).
#' @return The resolved token string.
#' @keywords internal
resolve_token <- function(token) {
  if (!nzchar(token)) {
    token <- Sys.getenv("PADDLE_OCR_TOKEN")
  }
  if (!nzchar(token)) {
    stop(
      "PaddleOCR API token is required. ",
      "Set the PADDLE_OCR_TOKEN environment variable or pass `token` explicitly.",
      call. = FALSE
    )
  }
  token
}

#' Resolve PaddleOCR job URL
#'
#' @param job_url Explicit URL or empty string.
#' @return The resolved job URL.
#' @keywords internal
resolve_job_url <- function(job_url) {
  if (!nzchar(job_url)) PADDLE_JOB_URL else job_url
}

#' Resolve PaddleOCR model name
#'
#' @param model Explicit model name or empty string.
#' @return The resolved model name.
#' @keywords internal
resolve_model <- function(model) {
  if (!nzchar(model)) PADDLE_DEFAULT_MODEL else model
}
