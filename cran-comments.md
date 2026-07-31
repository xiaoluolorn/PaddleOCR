## Release 0.2.1

In this version I have:

* added resumable PDF rendering and OCR processing;
* persisted submitted job IDs so interrupted jobs can be resumed;
* added bounded concurrent OCR job submission through the cloud API.

## Test environments

* local Windows 11, R 4.5.0
* R CMD check --as-cran --no-manual

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining note was `unable to verify current time`, which is specific to
the local check environment.

## Reverse dependencies

This is a new package, so there are no reverse dependencies.
