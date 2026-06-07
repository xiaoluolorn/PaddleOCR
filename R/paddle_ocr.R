#' Perform OCR on an image, PDF, or URL using PaddleOCR
#'
#' This is the main entry point for the PaddleOCR cloud API. Submit a local
#' file (image or document) or a URL, and the function will handle job
#' submission, polling, result retrieval, and file saving automatically.
#'
#' @param file_path A local file path or a URL (\code{http://} or
#'   \code{https://}).
#' @param output_dir Directory to save output Markdown and images
#'   (default: \code{"output"}).
#' @param token PaddleOCR API bearer token. If empty, reads from the
#'   \code{PADDLE_OCR_TOKEN} environment variable.
#' @param job_url PaddleOCR API endpoint. Defaults to the official cloud URL.
#' @param model Model name to use. Defaults to \code{"PaddleOCR-VL-1.6"}.
#' @param use_doc_orientation_classify Logical; enable document orientation
#'   classification.
#' @param use_doc_unwarping Logical; enable document unwarping.
#' @param use_chart_recognition Logical; enable chart/table recognition.
#' @param poll_interval Seconds between status checks (default: 5).
#' @param max_wait_seconds Maximum wait time for job completion (default:
#'   3600).
#' @param timeout HTTP request timeout in seconds (default: 600).
#' @return A list with elements:
#'   \describe{
#'     \item{file_path}{Input file path or URL.}
#'     \item{job_id}{The submitted job ID.}
#'     \item{output_dir}{Output directory path.}
#'     \item{markdown_files}{Paths to saved Markdown files.}
#'     \item{page_count}{Number of pages processed.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # OCR a local image
#' result <- paddle_ocr("document.png")
#'
#' # OCR a URL
#' result <- paddle_ocr("https://example.com/document.jpg")
#'
#' # With custom options
#' result <- paddle_ocr(
#'   file_path = "table.png",
#'   use_chart_recognition = TRUE,
#'   output_dir = "my_output"
#' )
#' }
paddle_ocr <- function(file_path,
                       output_dir = "output",
                       token = "",
                       job_url = "",
                       model = "",
                       use_doc_orientation_classify = FALSE,
                       use_doc_unwarping = FALSE,
                       use_chart_recognition = FALSE,
                       poll_interval = 5,
                       max_wait_seconds = 3600,
                       timeout = 600) {
  token <- resolve_token(token)
  job_url <- resolve_job_url(job_url)
  model <- resolve_model(model)

  is_url <- grepl("^https?://", file_path, ignore.case = TRUE)

  if (!is_url && !file.exists(file_path)) {
    stop("Input file does not exist: ", file_path, call. = FALSE)
  }

  message("Processing file: ", file_path)

  # 1. Submit job
  job_id <- submit_paddle_job(
    file_path = file_path,
    token = token,
    job_url = job_url,
    model = model,
    use_doc_orientation_classify = use_doc_orientation_classify,
    use_doc_unwarping = use_doc_unwarping,
    use_chart_recognition = use_chart_recognition,
    timeout = timeout
  )

  # 2. Poll until done
  jsonl_url <- poll_paddle_job(
    job_id = job_id,
    token = token,
    job_url = job_url,
    poll_interval = poll_interval,
    max_wait_seconds = max_wait_seconds
  )

  # 3. Download and parse JSONL result
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  jsonl_response <- httr::GET(jsonl_url, httr::timeout(600))
  if (httr::http_error(jsonl_response)) {
    stop(
      "Failed to download JSONL result (status ",
      httr::status_code(jsonl_response),
      ")",
      call. = FALSE
    )
  }
  jsonl_text <- httr::content(jsonl_response, as = "text", encoding = "UTF-8")

  lines <- strsplit(trimws(jsonl_text), "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(lines)]

  markdown_files <- character(0)
  page_num <- 0L

  for (line in lines) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    result <- parsed$result %||% list()
    layout_results <- result$layoutParsingResults %||% list()

    for (res in layout_results) {
      # Save markdown as doc_{page_num}.md
      md_filename <- file.path(output_dir, sprintf("doc_%d.md", page_num))
      md_text <- res$markdown$text %||% ""
      writeLines(md_text, md_filename, useBytes = TRUE)
      message("Markdown saved: ", md_filename)
      markdown_files <- c(markdown_files, md_filename)

      # Download markdown inline images (preserving relative paths)
      markdown_images <- res$markdown$images
      if (!is.null(markdown_images) && length(markdown_images) > 0) {
        for (img_path in names(markdown_images)) {
          img_url <- markdown_images[[img_path]]
          if (is.null(img_url) || !nzchar(img_url)) next
          full_img_path <- file.path(output_dir, img_path)
          try(download_binary_file(url = img_url, dest_path = full_img_path), silent = TRUE)
          message("Image saved: ", full_img_path)
        }
      }

      # Download output images as {img_name}_{page_num}.jpg
      output_images <- res$outputImages
      if (!is.null(output_images) && length(output_images) > 0) {
        for (img_name in names(output_images)) {
          img_url <- output_images[[img_name]]
          if (is.null(img_url) || !nzchar(img_url)) next
          filename <- file.path(output_dir, sprintf("%s_%d.jpg", img_name, page_num))
          try(download_binary_file(url = img_url, dest_path = filename), silent = TRUE)
          message("Image saved: ", filename)
        }
      }

      page_num <- page_num + 1L
    }
  }

  invisible(list(
    file_path = file_path,
    job_id = job_id,
    output_dir = output_dir,
    markdown_files = markdown_files,
    page_count = page_num
  ))
}
