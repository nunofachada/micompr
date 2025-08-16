## Test environments

* Ubuntu 24.04.3 (R: devel, release, oldrel-1, 4.4.0)
* Windows Server 2022 (R: release)
* macOS 15.5 (R: release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTES.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Other

* More robust `centerscale()` function, with better handling of edge cases and
  warning the user when these occur.
* Improve error message when num. obs != num. levels.
* Slight increase in test coverage.
* Bump minimum testthat version to 3.0.0.

## Note

* Local check done with
  `devtools::check(remote = TRUE, manual = TRUE, build_args = "--compact-vignettes=gs+qpdf")`
* CRAN submission done with
  `devtools::release(args = "--compact-vignettes=gs+qpdf")`