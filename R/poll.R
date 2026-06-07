#' Poll a PaddleOCR job until completion
#'
#' Repeatedly queries the job status endpoint until the job reaches
#' \code{"done"} or \code{"failed"} state, or the maximum wait time is
#' exceeded.
#'
#' @param job_id The job ID returned by \code{\link{submit_paddle_job}}.
#' @param token PaddleOCR API bearer token. If empty, reads from the
#'   \code{PADDLE_OCR_TOKEN} environment variable.
#' @param job_url PaddleOCR API endpoint. Defaults to the official cloud URL.
#' @param poll_interval Seconds between status checks (default: 5).
#' @param max_wait_seconds Maximum total wait time in seconds (default: 3600).
#' @return A character string containing the JSONL result URL.
#' @export
#' @examples
#' \dontrun{
#' job_id <- submit_paddle_job("document.png", token = "your_token")
#' jsonl_url <- poll_paddle_job(job_id, token = "your_token")
#' }
poll_paddle_job <- function(job_id,
                            token = "",
                            job_url = "",
                            poll_interval = 5,
                            max_wait_seconds = 3600) {
  token <- resolve_token(token)
  job_url <- resolve_job_url(job_url)

  auth_header <- httr::add_headers(Authorization = paste("bearer", token))
  endpoint <- paste0(job_url, "/", utils::URLencode(as.character(job_id), reserved = TRUE))

  started <- Sys.time()
  jsonl_url <- ""

  repeat {
    response <- httr::GET(endpoint, auth_header, httr::timeout(120))
    if (httr::http_error(response)) {
      stop(
        "Failed to query job status (status ",
        httr::status_code(response),
        "): ",
        httr::content(response, as = "text", encoding = "UTF-8"),
        call. = FALSE
      )
    }

    parsed <- jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    data <- parsed$data %||% list()
    state <- data$state %||% "unknown"

    if (identical(state, "pending")) {
      message("Job status: pending")
    } else if (identical(state, "running")) {
      progress <- data$extractProgress %||% list()
      total_pages <- progress$totalPages
      extracted_pages <- progress$extractedPages
      if (!is.null(total_pages) && !is.null(extracted_pages)) {
        message(sprintf(
          "Job status: running (total pages: %s, extracted: %s)",
          total_pages, extracted_pages
        ))
      } else {
        message("Job status: running...")
      }
    } else if (identical(state, "done")) {
      progress <- data$extractProgress %||% list()
      message(sprintf(
        "Job completed. Extracted pages: %s, start: %s, end: %s",
        progress$extractedPages %||% "NA",
        progress$startTime %||% "NA",
        progress$endTime %||% "NA"
      ))
      jsonl_url <- data$resultUrl$jsonUrl %||% ""
      break
    } else if (identical(state, "failed")) {
      stop("Job failed. Reason: ", data$errorMsg %||% "(none)", call. = FALSE)
    } else {
      message("Unknown job state: ", state)
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed > max_wait_seconds) {
      stop("Timed out waiting for job ", job_id, " to finish.", call. = FALSE)
    }

    Sys.sleep(poll_interval)
  }

  if (!nzchar(jsonl_url)) {
    stop("Job completed but no result JSONL URL was returned.", call. = FALSE)
  }

  jsonl_url
}
