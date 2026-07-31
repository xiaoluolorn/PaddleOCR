# PaddleOCR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/PaddleOCR)](https://CRAN.R-project.org/package=PaddleOCR)
[![R-CMD-check](https://github.com/xiaoluolorn/PaddleOCR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/xiaoluolorn/PaddleOCR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/xiaoluolorn/PaddleOCR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/xiaoluolorn/PaddleOCR?branch=master)
<!-- badges: end -->

An R client for the PaddleOCR cloud API. Submit images, PDFs, or URLs for OCR processing and get structured Markdown output with automatic image downloading.

## Installation

You can install the development version of PaddleOCR from GitHub:

```r
# install.packages("devtools")
devtools::install_github("xiaoluolorn/PaddleOCR")
```

## Setup

Set your PaddleOCR API token as an environment variable:

```r
Sys.setenv(PADDLE_OCR_TOKEN = "your_api_token_here")
```

Or add it to your `.Renviron` file for persistence:

```
PADDLE_OCR_TOKEN=your_api_token_here
```

## Quick Start

### OCR a single image or URL

```r
library(PaddleOCR)

# From a local file
result <- paddle_ocr("document.png")

# From a URL
result <- paddle_ocr("https://example.com/document.jpg")

# View results
result$markdown_files
result$page_count
```

### OCR a PDF document

```r
# Convert PDF to Markdown (requires pdftools)
result <- pdf_to_markdown_with_paddle("paper.pdf")

# The combined Markdown is saved and returned
cat(result$combined_markdown_text)
```

Interrupted runs resume automatically by reusing rendered page images,
completed Markdown files, and submitted OCR job IDs. To run up to eight OCR
jobs concurrently on the PaddleOCR service:

```r
result <- pdf_to_markdown_with_paddle("paper.pdf", workers = 8)
```

### Batch process PDFs

```r
results <- batch_pdf_to_markdown_with_paddle(
  pdf_dir = "papers/",
  output_root = "papers/paddle_output"
)
```

### Low-level API access

```r
# Submit a job
job_id <- submit_paddle_job("image.png", token = "your_token")

# Poll for completion
jsonl_url <- poll_paddle_job(job_id, token = "your_token")

# Process the result
result <- process_paddle_jsonl_result(jsonl_url, output_dir = "output")
```

## Features

- **Simple API**: `paddle_ocr()` handles everything from submission to result saving
- **File & URL support**: Submit local files or remote URLs
- **PDF processing**: Streaming page-by-page PDF rendering and OCR
- **Batch processing**: Process entire directories of PDFs
- **Auto image download**: Inline Markdown images and layout images are saved automatically
- **Configurable**: Model selection, orientation classification, unwarping, chart recognition

## API Options

| Parameter | Default | Description |
|-----------|---------|-------------|
| `model` | `"PaddleOCR-VL-1.6"` | OCR model to use |
| `use_doc_orientation_classify` | `FALSE` | Document orientation detection |
| `use_doc_unwarping` | `FALSE` | Document unwarping correction |
| `use_chart_recognition` | `FALSE` | Chart/table recognition |
| `poll_interval` | `5` | Seconds between status checks |
| `max_wait_seconds` | `3600` | Maximum wait time for job completion |

## Dependencies

- **Required**: `httr`, `jsonlite`
- **Optional**: `pdftools` (for PDF processing)

## License

MIT © PaddleOCR authors
