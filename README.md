[![Checks](https://github.com/nunofachada/micompr/actions/workflows/check.yml/badge.svg)](https://github.com/nunofachada/micompr/actions/workflows/check.yml)
[![codecov](https://codecov.io/gh/nunofachada/micompr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/nunofachada/micompr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/micompr)](https://CRAN.R-project.org/package=micompr)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/micompr)
[![Documentation](https://img.shields.io/badge/docs-1.1.3-brightgreen.svg)](https://www.rdocumentation.org/packages/micompr)

### Summary

The _micompr_ [R] package implements a procedure for comparing multivariate
samples associated with different groups. The procedure uses principal component
analysis to convert multivariate observations into a set of linearly
uncorrelated statistical measures, which are then compared using a number of
statistical methods. This technique is independent of the distributional
properties of samples and automatically selects features that best explain their
differences, avoiding manual selection of specific points or summary statistics.
The procedure is appropriate for comparing samples of time series, images,
spectrometric measures or similar multivariate observations.

### How to install

Install the development version from GitHub with the following command (requires
the [devtools] package):

```R
devtools::install_github("nunofachada/micompr")
```
A stable version of the package is available on [CRAN] and can be installed with
the following instruction:

```R
install.packages("micompr")
```

### Documentation

All methods and functions are fully documented and can be queried using the
built-in help system. After installation, to access the man pages, invoke the
_micompr_ help page as follows:

```R
help("micompr")
```

Additionally, the package contains two vignettes with a number of examples.

### References

#### Practice

* Fachada N, Rodrigues J, Lopes VV, Martins RC, Rosa AC. (2016) micompr: An R
Package for Multivariate Independent Comparison of Observations. *The R Journal*
8(2):405–420. https://doi.org/10.32614/RJ-2016-055

#### Theory

* Fachada N, Lopes VV, Martins RC, Rosa AC. (2017)
Model-independent comparison of simulation output. *Simulation Modelling
Practice and Theory*. 72:131–149. https://doi.org/10.1016/j.simpat.2016.12.013
([arXiv preprint](https://arxiv.org/abs/1509.09174))

### License

[MIT License](LICENSE)

[R]: https://www.r-project.org/
[devtools]: https://cran.r-project.org/package=devtools
[CRAN]: https://cran.r-project.org/
