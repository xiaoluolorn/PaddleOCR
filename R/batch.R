#' Batch convert PDFs to Markdown via PaddleOCR
#'
#' Processes all PDF files in a directory, converting each to Markdown using
#' \code{\link{pdf_to_markdown_with_paddle}}. Requires the \pkg{pdftools}
#' package.
#'
#' @param pdf_dir Directory containing PDF files (default: current directory).
#' @param output_root Root directory for output (default:
#'   \code{"<pdf_dir>/paddle_output"}).
#' @param dpi Image resolution for rendering (default: 300).
#' @param batch_trigger Number of pages to render before starting OCR
#'   (default: 3).
#' @param token PaddleOCR API token.
#' @param job_url PaddleOCR API endpoint.
#' @param model Model name.
#' @param poll_interval Polling interval in seconds.
#' @param max_wait_seconds Maximum wait time per job.
#' @param timeout HTTP timeout.
#' @return A named list of results (one per PDF). Failed conversions return a
#'   list with an \code{error} element.
#' @export
#' @examples
#' \dontrun{
#' results <- batch_pdf_to_markdown_with_paddle(
#'   pdf_dir = "papers",
#'   output_root = "papers/paddle_output"
#' )
#' }
batch_pdf_to_markdown_with_paddle <- function(pdf_dir = ".",
                                              output_root = file.path(pdf_dir, "paddle_output"),
                                              dpi = 300,
                                              batch_trigger = 3,
                                              token = "",
                                              job_url = "",
                                              model = "",
                                              poll_interval = 5,
                                              max_wait_seconds = 1800,
                                              timeout = 600) {
  pdf_files <- list.files(
    pdf_dir,
    pattern = "\\.pdf$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(pdf_files) == 0) {
    stop("No PDF files found in: ", pdf_dir, call. = FALSE)
  }

  dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
  results <- vector("list", length(pdf_files))
  names(results) <- tools::file_path_sans_ext(basename(pdf_files))

  for (i in seq_along(pdf_files)) {
    pdf_path <- pdf_files[i]
    base_name <- tools::file_path_sans_ext(basename(pdf_path))
    output_dir <- file.path(output_root, base_name)

    message(sprintf("[%d/%d] Processing %s", i, length(pdf_files), basename(pdf_path)))

    results[[base_name]] <- tryCatch(
      pdf_to_markdown_with_paddle(
        pdf_path = pdf_path,
        output_dir = output_dir,
        dpi = dpi,
        batch_trigger = batch_trigger,
        token = token,
        job_url = job_url,
        model = model,
        poll_interval = poll_interval,
        max_wait_seconds = max_wait_seconds,
        timeout = timeout
      ),
      error = function(e) {
        message("  -> Failed: ", conditionMessage(e))
        list(error = conditionMessage(e))
      }
    )
  }

  invisible(results)
}
