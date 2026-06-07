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
