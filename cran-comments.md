## Test environments

* Ubuntu 22.04.3 (R: devel, release, oldrel-1, 4.1.0)
* Windows 11 (R: release)
* macOS 12 (R: release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTES.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Other

* Fix packaging issue due to roxygen update.

## Note

* Local check done with
  `devtools::check(remote = TRUE, manual = TRUE, build_args = "--compact-vignettes=gs+qpdf")`
* CRAN submission done with
  `devtools::submit_cran(args = "--compact-vignettes=gs+qpdf")`
