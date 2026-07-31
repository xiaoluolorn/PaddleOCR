# Tests for PaddleOCR package
# These tests do not require an API token and test local logic only.

test_that("%||% returns fallback for NULL, empty, and blank string", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(character(0) %||% "fallback", "fallback")
  expect_equal("" %||% "fallback", "fallback")
  expect_equal("value" %||% "fallback", "value")
  expect_equal(42 %||% 0, 42)
})

test_that("guess_extension_from_url works correctly", {
  expect_equal(PaddleOCR:::guess_extension_from_url("https://example.com/img.png"), "png")
  expect_equal(PaddleOCR:::guess_extension_from_url("https://example.com/img.jpg?x=1"), "jpg")
  expect_equal(PaddleOCR:::guess_extension_from_url("https://example.com/noext"), "jpg")
  expect_equal(PaddleOCR:::guess_extension_from_url("https://example.com/noext", fallback = "png"), "png")
})

test_that("resolve_token stops when no token is available", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = ""),
    expect_error(PaddleOCR:::resolve_token(""), "API token is required")
  )
})

test_that("resolve_token uses env var when token is empty", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = "test_token_123"),
    expect_equal(PaddleOCR:::resolve_token(""), "test_token_123")
  )
})

test_that("resolve_token prefers explicit token over env var", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = "env_token"),
    expect_equal(PaddleOCR:::resolve_token("explicit_token"), "explicit_token")
  )
})

test_that("resolve_job_url falls back to default", {
  expect_equal(PaddleOCR:::resolve_job_url(""), PaddleOCR:::PADDLE_JOB_URL)
  expect_equal(PaddleOCR:::resolve_job_url("https://custom.url"), "https://custom.url")
})

test_that("resolve_model falls back to default", {
  expect_equal(PaddleOCR:::resolve_model(""), PaddleOCR:::PADDLE_DEFAULT_MODEL)
  expect_equal(PaddleOCR:::resolve_model("CustomModel"), "CustomModel")
})

test_that("submit_paddle_job errors on missing file", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = "test_token"),
    expect_error(
      submit_paddle_job(file_path = "/nonexistent/file.png"),
      "Input file does not exist"
    )
  )
})

test_that("submit_paddle_job errors on missing token", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = ""),
    expect_error(
      submit_paddle_job(file_path = "test.png"),
      "API token is required"
    )
  )
})

test_that("paddle_ocr errors on missing token", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = ""),
    expect_error(
      paddle_ocr(file_path = "test.png"),
      "API token is required"
    )
  )
})

test_that("paddle_ocr errors on missing file", {
  withr::with_envvar(
    list(PADDLE_OCR_TOKEN = "test_token"),
    expect_error(
      paddle_ocr(file_path = "/nonexistent/file.png"),
      "Input file does not exist"
    )
  )
})

test_that("pdf_to_images errors when pdftools is not available", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(
    pdf_to_images("test.pdf", "images"),
    "Package 'pdftools' is required"
  )
})

test_that("batch_pdf_to_markdown_with_paddle errors on empty directory", {
  withr::with_tempdir({
    expect_error(
      batch_pdf_to_markdown_with_paddle("."),
      "No PDF files found"
    )
  })
})

test_that("pdf_page_to_image reuses an existing rendered page", {
  withr::with_tempdir({
    pdf_path <- "document.pdf"
    image_dir <- "pages"
    writeLines("placeholder", pdf_path)
    dir.create(image_dir)
    existing <- file.path(image_dir, "document_001.png")
    writeBin(charToRaw("already rendered"), existing)

    expect_message(
      result <- pdf_page_to_image(
        pdf_path,
        page_index = 1,
        image_dir = image_dir,
        overwrite = FALSE
      ),
      "Reusing rendered page"
    )
    expect_equal(result, existing)
    expect_equal(readBin(existing, "raw", n = 100), charToRaw("already rendered"))
  })
})

test_that("existing page markdown is returned as a completed OCR result", {
  withr::with_tempdir({
    dir.create("markdown_pages")
    path <- file.path("markdown_pages", "doc_4.md")
    writeLines("cached OCR text", path)

    result <- PaddleOCR:::read_existing_page_result("markdown_pages", 5)

    expect_equal(result$markdown_paths, path)
    expect_equal(result$markdown_texts, "cached OCR text")
    expect_null(result$job_id)
  })
})

test_that("worker count is validated and capped by the page count", {
  expect_equal(PaddleOCR:::normalize_workers(1, 10), 1L)
  expect_equal(PaddleOCR:::normalize_workers(8, 3), 3L)
  expect_error(PaddleOCR:::normalize_workers(0, 3), "positive integer")
  expect_error(PaddleOCR:::normalize_workers(1.5, 3), "positive integer")
})

test_that("multiple OCR jobs are submitted before result polling", {
  withr::with_tempdir({
    writeLines("placeholder", "document.pdf")
    events <- character(0)

    local_mocked_bindings(
      pdf_info = function(...) list(pages = 2L),
      .package = "pdftools"
    )
    local_mocked_bindings(
      pdf_page_to_image = function(pdf_path, page_index, image_dir, ...) {
        path <- file.path(image_dir, sprintf("document_%03d.png", page_index))
        writeBin(as.raw(page_index), path)
        path
      },
      submit_paddle_job = function(file_path, ...) {
        page <- as.integer(sub(".*_(\\d+)\\.png$", "\\1", file_path))
        events <<- c(events, paste0("submit-", page))
        paste0("job-", page)
      },
      poll_paddle_job = function(job_id, ...) {
        events <<- c(events, paste0("poll-", sub("job-", "", job_id)))
        paste0("result-", job_id)
      },
      process_paddle_jsonl_result = function(jsonl_url, output_dir,
                                             starting_doc_index) {
        path <- file.path(output_dir, sprintf("doc_%d.md", starting_doc_index))
        text <- paste0("page ", starting_doc_index + 1L)
        writeLines(text, path)
        list(markdown_files = path, markdown_texts = text, doc_count = 1L)
      },
      .package = "PaddleOCR"
    )

    pdf_to_markdown_with_paddle(
      "document.pdf",
      output_dir = "output",
      workers = 2,
      batch_trigger = 1,
      resume = FALSE,
      token = "token",
      combined_markdown = FALSE
    )

    expect_equal(events, c("submit-1", "submit-2", "poll-1", "poll-2"))
  })
})

test_that("a submitted job checkpoint is resumed without resubmission", {
  withr::with_tempdir({
    writeLines("placeholder", "document.pdf")
    dir.create(file.path("output", "pages"), recursive = TRUE)
    image_path <- file.path("output", "pages", "document_001.png")
    writeBin(as.raw(1), image_path)
    page_output_dir <- file.path("output", "markdown_pages")
    PaddleOCR:::write_page_state(
      page_output_dir,
      1,
      list(status = "submitted", job_id = "existing-job")
    )

    local_mocked_bindings(
      pdf_info = function(...) list(pages = 1L),
      .package = "pdftools"
    )
    local_mocked_bindings(
      submit_paddle_job = function(...) stop("must not resubmit"),
      poll_paddle_job = function(job_id, ...) {
        expect_equal(job_id, "existing-job")
        "result-url"
      },
      process_paddle_jsonl_result = function(jsonl_url, output_dir,
                                             starting_doc_index) {
        path <- file.path(output_dir, "doc_0.md")
        writeLines("resumed result", path)
        list(markdown_files = path, markdown_texts = "resumed result", doc_count = 1L)
      },
      .package = "PaddleOCR"
    )

    result <- pdf_to_markdown_with_paddle(
      "document.pdf",
      output_dir = "output",
      resume = TRUE,
      token = "token",
      combined_markdown = FALSE
    )

    expect_equal(result$combined_markdown_text, "resumed result")
  })
})
