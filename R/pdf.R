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
#' @param overwrite Logical; overwrite an existing rendered page. Set to
#'   \code{FALSE} to reuse it (default: \code{TRUE}).
#' @return Path to the saved PNG image.
#' @export
#' @examples
#' \dontrun{
#' img <- pdf_page_to_image("document.pdf", page_index = 1, image_dir = "pages")
#' }
pdf_page_to_image <- function(pdf_path,
                              page_index,
                              image_dir,
                              dpi = 300,
                              overwrite = TRUE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF conversion. Install it with install.packages('pdftools').", call. = FALSE)
  }
  dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)

  base_name <- tools::file_path_sans_ext(basename(pdf_path))
  dest_path <- file.path(image_dir, sprintf("%s_%03d.png", base_name, page_index))

  if (!isTRUE(overwrite) && file.exists(dest_path)) {
    message("Reusing rendered page: ", dest_path)
    return(dest_path)
  }

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

#' Read a completed OCR result from disk
#' @noRd
read_existing_page_result <- function(output_dir, page_index) {
  state <- read_page_state(output_dir, page_index)
  if (!is.null(state) && identical(state$status, "completed")) {
    return(state$result)
  }

  markdown_path <- file.path(output_dir, sprintf("doc_%d.md", page_index - 1L))
  if (!file.exists(markdown_path)) {
    return(NULL)
  }

  list(
    markdown_paths = markdown_path,
    markdown_texts = paste(readLines(markdown_path, warn = FALSE), collapse = "\n"),
    job_id = NULL
  )
}

#' Validate the requested OCR concurrency
#' @noRd
normalize_workers <- function(workers, page_count) {
  if (length(workers) != 1L || !is.numeric(workers) || !is.finite(workers) ||
      workers < 1 || workers != as.integer(workers)) {
    stop("`workers` must be a positive integer.", call. = FALSE)
  }
  min(as.integer(workers), as.integer(page_count))
}

#' Return the checkpoint path for one PDF page
#' @noRd
page_state_path <- function(output_dir, page_index) {
  file.path(output_dir, ".paddle_state", sprintf("page_%06d.rds", page_index))
}

#' Read one page checkpoint
#' @noRd
read_page_state <- function(output_dir, page_index) {
  path <- page_state_path(output_dir, page_index)
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) NULL)
}

#' Atomically write one page checkpoint
#' @noRd
write_page_state <- function(output_dir, page_index, state) {
  path <- page_state_path(output_dir, page_index)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  temporary_path <- tempfile("page_state_", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)
  saveRDS(state, temporary_path)
  if (file.exists(path)) {
    file.remove(path)
  }
  if (!file.rename(temporary_path, path)) {
    stop("Failed to save OCR checkpoint for page ", page_index, ".", call. = FALSE)
  }
  invisible(path)
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
#' @param workers Maximum number of OCR jobs submitted concurrently (default:
#'   1). OCR runs concurrently on the PaddleOCR service; result polling remains
#'   local and sequential.
#' @param resume Logical; reuse rendered page images, completed Markdown files,
#'   and submitted job IDs from an interrupted run (default: \code{TRUE}).
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
                                        workers = 1,
                                        resume = TRUE,
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
  workers <- normalize_workers(workers, page_count)
  render_trigger <- max(batch_trigger, workers)
  message(sprintf(
    "PDF has %d page(s). Streaming render -> OCR (trigger=%d, workers=%d, resume=%s).",
    page_count, render_trigger, workers, isTRUE(resume)
  ))

  combined_text <- character(page_count)
  markdown_files <- character(page_count)
  image_paths <- character(page_count)

  pending <- integer(0)

  run_ocr_for <- function(indices) {
    page_results <- vector("list", length(indices))
    names(page_results) <- as.character(indices)

    if (isTRUE(resume)) {
      for (idx in indices) {
        existing <- read_existing_page_result(page_output_dir, idx)
        if (!is.null(existing)) {
          message(sprintf("Reusing OCR result for page %d/%d.", idx, page_count))
          page_results[[as.character(idx)]] <- existing
        }
      }
    }

    remaining <- indices[vapply(
      as.character(indices),
      function(index) is.null(page_results[[index]]),
      logical(1)
    )]

    chunks <- split(remaining, ceiling(seq_along(remaining) / workers))
    for (chunk in chunks) {
      job_ids <- character(length(chunk))
      names(job_ids) <- as.character(chunk)

      for (idx in chunk) {
        state <- if (isTRUE(resume)) read_page_state(page_output_dir, idx) else NULL
        if (!is.null(state) && identical(state$status, "submitted") &&
            nzchar(state$job_id %||% "")) {
          job_ids[[as.character(idx)]] <- state$job_id
          message(sprintf("Resuming submitted OCR job for page %d/%d.", idx, page_count))
        } else {
          message(sprintf("Submitting OCR for page %d/%d: %s", idx, page_count, basename(image_paths[idx])))
          job_id <- submit_paddle_job(
            file_path = image_paths[idx],
            token = token,
            job_url = job_url,
            model = model,
            use_doc_orientation_classify = use_doc_orientation_classify,
            use_doc_unwarping = use_doc_unwarping,
            use_chart_recognition = use_chart_recognition,
            timeout = timeout
          )
          job_ids[[as.character(idx)]] <- job_id
          write_page_state(
            page_output_dir,
            idx,
            list(status = "submitted", job_id = job_id)
          )
        }
      }

      for (idx in chunk) {
        job_id <- job_ids[[as.character(idx)]]
        message(sprintf("Collecting OCR result for page %d/%d.", idx, page_count))
        jsonl_url <- tryCatch(
          poll_paddle_job(
            job_id = job_id,
            token = token,
            job_url = job_url,
            poll_interval = poll_interval,
            max_wait_seconds = max_wait_seconds
          ),
          error = function(e) {
            if (grepl("^Job failed", conditionMessage(e))) {
              write_page_state(
                page_output_dir,
                idx,
                list(
                  status = "failed",
                  job_id = job_id,
                  error = conditionMessage(e)
                )
              )
            }
            stop(e)
          }
        )
        parsed_result <- process_paddle_jsonl_result(
          jsonl_url = jsonl_url,
          output_dir = page_output_dir,
          starting_doc_index = idx - 1L
        )
        page_result <- list(
          markdown_paths = parsed_result$markdown_files,
          markdown_texts = parsed_result$markdown_texts,
          job_id = job_id
        )
        page_results[[as.character(idx)]] <- page_result
        write_page_state(
          page_output_dir,
          idx,
          list(status = "completed", job_id = job_id, result = page_result)
        )
      }
    }

    for (idx in indices) {
      page_result <- page_results[[as.character(idx)]]
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
      dpi = dpi,
      overwrite = !isTRUE(resume)
    )
    pending <- c(pending, i)

    if (length(pending) >= render_trigger) {
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
