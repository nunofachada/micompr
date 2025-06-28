## Test environments

* Ubuntu 24.04.2 (R: devel, release, oldrel-1, 4.4.0)
* Windows 11 (R: release)
* macOS 14 (R: release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTES.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Other

* Fix several issues due to updates in upstream package MVN.

## Note

* Local check done with
  `devtools::check(remote = TRUE, manual = TRUE, build_args = "--compact-vignettes=gs+qpdf")`
* CRAN submission done with
  `devtools::release(args = "--compact-vignettes=gs+qpdf")`