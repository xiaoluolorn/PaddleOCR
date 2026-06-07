#' Save layout parsing result as Markdown with images
#'
#' Writes the Markdown text from a layout result to a file, then downloads
#' any inline images and output images referenced in the result.
#'
#' @param layout_result A single layout parsing result (list).
#' @param output_dir Directory to save files into.
#' @param doc_name Base name for the Markdown file (without extension).
#' @return Path to the saved Markdown file, invisibly.
#' @keywords internal
save_layout_markdown <- function(layout_result, output_dir, doc_name) {
  markdown <- layout_result$markdown %||% list()
  markdown_text <- markdown$text %||% ""
  markdown_path <- file.path(output_dir, paste0(doc_name, ".md"))

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(markdown_text, markdown_path, useBytes = TRUE)

  # Download markdown inline images (preserving relative paths)
  markdown_images <- markdown$images
  if (!is.null(markdown_images) && length(markdown_images) > 0) {
    for (image_key in names(markdown_images)) {
      img_url <- markdown_images[[image_key]]
      if (is.null(img_url) || !nzchar(img_url)) next
      try(
        download_binary_file(
          url = img_url,
          dest_path = file.path(output_dir, image_key)
        ),
        silent = TRUE
      )
    }
  }

  # Download output images as {name}_{doc_name}.jpg (matching Python SDK)
  output_images <- layout_result$outputImages
  if (!is.null(output_images) && length(output_images) > 0) {
    for (image_key in names(output_images)) {
      img_url <- output_images[[image_key]]
      if (is.null(img_url) || !nzchar(img_url)) next
      try(
        download_binary_file(
          url = img_url,
          dest_path = file.path(output_dir, paste0(image_key, "_", doc_name, ".jpg"))
        ),
        silent = TRUE
      )
    }
  }

  invisible(markdown_path)
}

#' Download and parse a PaddleOCR JSONL result
#'
#' Fetches the JSONL result from the URL, parses each line, and saves
#' layout-parsed Markdown documents and their associated images.
#'
#' @param jsonl_url URL to the JSONL result file.
#' @param output_dir Directory to save output files.
#' @param starting_doc_index Integer offset for naming output files
#'   (default: 0).
#' @return A list with elements \code{markdown_files} (file paths),
#'   \code{markdown_texts} (text content), and \code{doc_count} (number of
#'   documents saved).
#' @export
#' @examples
#' \dontrun{
#' result <- process_paddle_jsonl_result(
#'   jsonl_url = "https://example.com/result.jsonl",
#'   output_dir = "output"
#' )
#' }
process_paddle_jsonl_result <- function(jsonl_url,
                                        output_dir,
                                        starting_doc_index = 0L) {
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
  combined_texts <- character(0)
  doc_index <- as.integer(starting_doc_index)

  for (line in lines) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    result <- parsed$result %||% list()
    layout_results <- result$layoutParsingResults %||% list()

    for (res in layout_results) {
      doc_name <- sprintf("doc_%d", doc_index)
      save_layout_markdown(res, output_dir = output_dir, doc_name = doc_name)
      md_path <- file.path(output_dir, paste0(doc_name, ".md"))
      markdown_files <- c(markdown_files, md_path)
      combined_texts <- c(combined_texts, res$markdown$text %||% "")
      message("Markdown saved: ", md_path)
      doc_index <- doc_index + 1L
    }
  }

  list(
    markdown_files = markdown_files,
    markdown_texts = combined_texts,
    doc_count = doc_index - as.integer(starting_doc_index)
  )
}
