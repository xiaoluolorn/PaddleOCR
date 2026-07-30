## Resubmission

This is a resubmission. In this version I have:

* declared the `withr` test dependency;
* removed the unused vignette builder declaration;
* excluded development artifacts from the source package;
* regenerated package documentation and removed an invalid public URL.

## Test environments

* local Windows 11, R 4.5.0
* R CMD check --as-cran --no-manual

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining note was `unable to verify current time`, which is specific to
the local check environment.

## Reverse dependencies

This is a new package, so there are no reverse dependencies.
