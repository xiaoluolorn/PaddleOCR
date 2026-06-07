#' Convert all pages of a PDF to images
#'
#' Renders every page of a PDF file to PNG images at the specified DPI.
#' Requires the \pkg{pdftools} package.
#'
#' @param pdf_path Path to the PDF file.
#' @param image_dir Directory to save PNG images into.
#' @param dpi Resolution in dots per inch (default: 300).
#' @return A character vector of image file paths.
#' @export
#' @examples
#' \dontrun{
#' images <- pdf_to_images("document.pdf", "images", dpi = 300)
#' }
pdf_to_images <- function(pdf_path, image_dir, dpi = 300) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF conversion. Install it with install.packages('pdftools').", call. = FALSE)
  }
  if (!file.exists(pdf_path)) {
    stop("PDF file does not exist: ", pdf_path, call. = FALSE)
  }

  dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)

  page_count <- pdftools::pdf_info(pdf_path)$pages
  base_name <- tools::file_path_sans_ext(basename(pdf_path))
  image_paths <- file.path(
    image_dir,
    sprintf("%s_%03d.png", base_name, seq_len(page_count))
  )

  pdftools::pdf_convert(
    pdf = pdf_path,
    format = "png",
    dpi = dpi,
    filenames = image_paths,
    verbose = FALSE
  )

  image_paths
}

#' Convert a single PDF page to an image
#'
#' Renders one page of a PDF to a PNG file. Uses a temporary directory
#' internally to avoid \pkg{pdftools} filename template issues.
#'
#' @param pdf_path Path to the PDF file.
#' @param page_index Page number to render (1-indexed).
#' @param image_dir Directory to save the PNG image.
#' @param dpi Resolution in dots per inch (default: 300).
#' @return Path to the saved PNG image.
#' @export
#' @examples
#' \dontrun{
#' img <- pdf_page_to_image("document.pdf", page_index = 1, image_dir = "pages")
#' }
pdf_page_to_image <- function(pdf_path, page_index, image_dir, dpi = 300) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF conversion. Install it with install.packages('pdftools').", call. = FALSE)
  }
  dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)

  base_name <- tools::file_path_sans_ext(basename(pdf_path))
  dest_path <- file.path(image_dir, sprintf("%s_%03d.png", base_name, page_index))

  # Render into a unique temp directory to avoid pdftools sprintf template issues
  tmp_dir <- file.path(tempdir(), paste0("PaddleOCR_", as.integer(Sys.time()), "_", page_index))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  produced <- pdftools::pdf_convert(
    pdf = pdf_path,
    format = "png",
    pages = page_index,
    dpi = dpi,
    filenames = file.path(tmp_dir, "page_%d.png"),
    verbose = FALSE
  )

  if (length(produced) != 1 || !file.exists(produced[[1]])) {
    stop("Failed to render page ", page_index, " of ", pdf_path, call. = FALSE)
  }

  if (file.exists(dest_path)) file.remove(dest_path)
  ok <- file.rename(produced[[1]], dest_path)
  if (!isTRUE(ok)) {
    file.copy(produced[[1]], dest_path, overwrite = TRUE)
  }

  dest_path
}

#' OCR a single image file via PaddleOCR
#'
#' Submits an image for OCR, polls for completion, and saves Markdown output.
#'
#' @param image_path Path to the image file.
#' @param output_dir Directory for OCR output.
#' @param page_index Page index for naming output files (1-indexed).
#' @param token PaddleOCR API token.
#' @param job_url PaddleOCR API endpoint.
#' @param model Model name.
#' @param use_doc_orientation_classify Logical; enable orientation classification.
#' @param use_doc_unwarping Logical; enable document unwarping.
#' @param use_chart_recognition Logical; enable chart recognition.
#' @param poll_interval Polling interval in seconds.
#' @param max_wait_seconds Maximum wait time.
#' @param timeout HTTP timeout.
#' @return A list with \code{markdown_paths}, \code{markdown_texts}, and
#'   \code{job_id}.
#' @export
#' @examples
#' \dontrun{
#' result <- image_to_markdown("page_1.png", output_dir = "output", page_index = 1)
#' }
image_to_markdown <- function(image_path,
                              output_dir,
                              page_index,
                              token = "",
                              job_url = "",
                              model = "",
                              use_doc_orientation_classify = FALSE,
                              use_doc_unwarping = FALSE,
                              use_chart_recognition = FALSE,
                              poll_interval = 5,
                              max_wait_seconds = 1800,
                              timeout = 600) {
  token <- resolve_token(token)
  job_url <- resolve_job_url(job_url)
  model <- resolve_model(model)

  message("Processing file: ", image_path)

  job_id <- submit_paddle_job(
    file_path = image_path,
    token = token,
    job_url = job_url,
    model = model,
    use_doc_orientation_classify = use_doc_orientation_classify,
    use_doc_unwarping = use_doc_unwarping,
    use_chart_recognition = use_chart_recognition,
    timeout = timeout
  )

  jsonl_url <- poll_paddle_job(
    job_id = job_id,
    token = token,
    job_url = job_url,
    poll_interval = poll_interval,
    max_wait_seconds = max_wait_seconds
  )

  parsed_result <- process_paddle_jsonl_result(
    jsonl_url = jsonl_url,
    output_dir = output_dir,
    starting_doc_index = page_index - 1L
  )

  list(
    markdown_paths = parsed_result$markdown_files,
    markdown_texts = parsed_result$markdown_texts,
    job_id = job_id
  )
}

#' Convert a PDF to Markdown via PaddleOCR
#'
#' Renders PDF pages to images one by one, then submits them for OCR using
#' the PaddleOCR cloud API. Pages are processed in streaming batches:
#' once \code{batch_trigger} pages are rendered, OCR begins while rendering
#' continues.
#'
#' Requires the \pkg{pdftools} package.
#'
#' @param pdf_path Path to the PDF file.
#' @param output_dir Output directory. If \code{NULL}, defaults to
#'   \code{"<pdf_name>_paddle_output"} next to the PDF.
#' @param combined_markdown Logical; if \code{TRUE} (default), combine all
#'   page Markdown into a single file.
#' @param dpi Image resolution for rendering (default: 300).
#' @param batch_trigger Number of pages to render before starting OCR
#'   (default: 3).
#' @param token PaddleOCR API token.
#' @param job_url PaddleOCR API endpoint.
#' @param model Model name.
#' @param use_doc_orientation_classify Logical; enable orientation classification.
#' @param use_doc_unwarping Logical; enable document unwarping.
#' @param use_chart_recognition Logical; enable chart recognition.
#' @param poll_interval Polling interval in seconds.
#' @param max_wait_seconds Maximum wait time per job.
#' @param timeout HTTP timeout.
#' @param ... Ignored (for future compatibility).
#' @return A list with PDF path, image paths, Markdown file paths, combined
#'   Markdown path and text, and output directory.
#' @export
#' @examples
#' \dontrun{
#' result <- pdf_to_markdown_with_paddle("document.pdf")
#' cat(result$combined_markdown_text)
#' }
pdf_to_markdown_with_paddle <- function(pdf_path,
                                        output_dir = NULL,
                                        combined_markdown = TRUE,
                                        dpi = 300,
                                        batch_trigger = 3,
                                        token = "",
                                        job_url = "",
                                        model = "",
                                        use_doc_orientation_classify = FALSE,
                                        use_doc_unwarping = FALSE,
                                        use_chart_recognition = FALSE,
                                        poll_interval = 5,
                                        max_wait_seconds = 1800,
                                        timeout = 600,
                                        ...) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF conversion. Install it with install.packages('pdftools').", call. = FALSE)
  }
  if (!file.exists(pdf_path)) {
    stop("PDF file does not exist: ", pdf_path, call. = FALSE)
  }

  token <- resolve_token(token)
  job_url <- resolve_job_url(job_url)
  model <- resolve_model(model)

  batch_trigger <- max(1L, as.integer(batch_trigger))

  base_name <- tools::file_path_sans_ext(basename(pdf_path))
  if (is.null(output_dir) || !nzchar(output_dir)) {
    output_dir <- file.path(dirname(pdf_path), paste0(base_name, "_paddle_output"))
  }
  image_dir <- file.path(output_dir, "pages")
  page_output_dir <- file.path(output_dir, "markdown_pages")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(page_output_dir, showWarnings = FALSE, recursive = TRUE)

  page_count <- pdftools::pdf_info(pdf_path)$pages
  message(sprintf("PDF has %d page(s). Streaming render -> OCR (trigger=%d).", page_count, batch_trigger))

  combined_text <- character(page_count)
  markdown_files <- character(page_count)
  image_paths <- character(page_count)

  pending <- integer(0)

  run_ocr_for <- function(indices) {
    for (idx in indices) {
      message(sprintf("Running OCR for page %d/%d: %s", idx, page_count, basename(image_paths[idx])))

      page_result <- image_to_markdown(
        image_path = image_paths[idx],
        output_dir = page_output_dir,
        page_index = idx,
        token = token,
        job_url = job_url,
        model = model,
        use_doc_orientation_classify = use_doc_orientation_classify,
        use_doc_unwarping = use_doc_unwarping,
        use_chart_recognition = use_chart_recognition,
        poll_interval = poll_interval,
        max_wait_seconds = max_wait_seconds,
        timeout = timeout
      )

      combined_text[idx] <<- paste(page_result$markdown_texts, collapse = "\n\n")
      markdown_files[idx] <<- paste(page_result$markdown_paths, collapse = ", ")
    }
  }

  for (i in seq_len(page_count)) {
    message(sprintf("Rendering page %d/%d to PNG...", i, page_count))
    image_paths[i] <- pdf_page_to_image(
      pdf_path = pdf_path,
      page_index = i,
      image_dir = image_dir,
      dpi = dpi
    )
    pending <- c(pending, i)

    if (length(pending) >= batch_trigger) {
      message(sprintf("Reached %d pending pages, starting OCR batch...", length(pending)))
      run_ocr_for(pending)
      pending <- integer(0)
    }
  }

  if (length(pending) > 0) {
    message(sprintf("Processing final %d pending page(s)...", length(pending)))
    run_ocr_for(pending)
  }

  combined_md_path <- NULL
  combined_md_text <- paste(combined_text, collapse = "\n\n---\n\n")

  if (isTRUE(combined_markdown)) {
    combined_md_path <- file.path(output_dir, paste0(base_name, ".md"))
    writeLines(combined_md_text, combined_md_path, useBytes = TRUE)
    message("Combined Markdown saved to: ", combined_md_path)
  }

  invisible(list(
    pdf_path = pdf_path,
    image_paths = image_paths,
    markdown_files = markdown_files,
    combined_markdown_path = combined_md_path,
    combined_markdown_text = combined_md_text,
    output_dir = output_dir
  ))
}
