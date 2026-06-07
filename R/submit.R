#' Submit an OCR job to the PaddleOCR cloud API
#'
#' Submits a local file or a URL for OCR processing. The function detects
#' whether the input is a URL or a local file path and uses the appropriate
#' API call method (JSON body vs. multipart upload).
#'
#' @param file_path A local file path or a URL (\code{http://} or \code{https://}).
#' @param token PaddleOCR API bearer token. If empty, reads from the
#'   \code{PADDLE_OCR_TOKEN} environment variable.
#' @param job_url PaddleOCR API endpoint. Defaults to the official cloud URL.
#' @param model Model name to use. Defaults to \code{"PaddleOCR-VL-1.6"}.
#' @param use_doc_orientation_classify Logical; enable document orientation
#'   classification.
#' @param use_doc_unwarping Logical; enable document unwarping.
#' @param use_chart_recognition Logical; enable chart/table recognition.
#' @param timeout HTTP request timeout in seconds (default: 600).
#' @return A character string containing the job ID.
#' @export
#' @examples
#' \dontrun{
#' # Submit a local image
#' job_id <- submit_paddle_job(
#'   file_path = "document.png",
#'   token = "your_token_here"
#' )
#'
#' # Submit a URL
#' job_id <- submit_paddle_job(
#'   file_path = "https://example.com/document.jpg",
#'   token = "your_token_here"
#' )
#' }
submit_paddle_job <- function(file_path,
                              token = "",
                              job_url = "",
                              model = "",
                              use_doc_orientation_classify = FALSE,
                              use_doc_unwarping = FALSE,
                              use_chart_recognition = FALSE,
                              timeout = 600) {
  token <- resolve_token(token)
  job_url <- resolve_job_url(job_url)
  model <- resolve_model(model)

  optional_payload <- list(
    useDocOrientationClassify = isTRUE(use_doc_orientation_classify),
    useDocUnwarping = isTRUE(use_doc_unwarping),
    useChartRecognition = isTRUE(use_chart_recognition)
  )

  auth_header <- httr::add_headers(Authorization = paste("bearer", token))

  is_url <- grepl("^https?://", file_path, ignore.case = TRUE)

  if (is_url) {
    payload <- list(
      fileUrl = file_path,
      model = model,
      optionalPayload = optional_payload
    )
    response <- httr::POST(
      url = job_url,
      auth_header,
      httr::content_type_json(),
      httr::timeout(timeout),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      encode = "raw"
    )
  } else {
    if (!file.exists(file_path)) {
      stop("Input file does not exist: ", file_path, call. = FALSE)
    }

    response <- httr::POST(
      url = job_url,
      auth_header,
      httr::timeout(timeout),
      body = list(
        model = model,
        optionalPayload = jsonlite::toJSON(optional_payload, auto_unbox = TRUE),
        file = httr::upload_file(file_path)
      ),
      encode = "multipart"
    )
  }

  response_text <- httr::content(response, as = "text", encoding = "UTF-8")

  if (httr::http_error(response)) {
    stop(
      "PaddleOCR job submission failed (status ",
      httr::status_code(response),
      "): ",
      response_text,
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  job_id <- parsed$data$jobId
  if (is.null(job_id) || !nzchar(as.character(job_id))) {
    stop("Unexpected response: missing `jobId`. Body: ", response_text, call. = FALSE)
  }

  message("Job submitted successfully. Job ID: ", job_id)
  job_id
}
