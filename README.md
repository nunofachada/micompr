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
devtools::install_github("fakenmc/micompr")
```
The package has been submitted to [CRAN] and, if accepted, will be installable
via the following instruction:

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
Package for Multivariate Independent Comparison of Observations. *Under
peer-review*. (arXiv version available at http://arxiv.org/abs/1603.06907)

#### Theory

* Fachada N, Lopes VV, Martins RC, Rosa AC. (2016) Model-independent
comparison of simulation output. *Under peer-review*. (arXiv version available
at http://arxiv.org/abs/1509.09174)

### License

[MIT License](LICENSE)

[R]: https://www.r-project.org/
[devtools]: https://cran.r-project.org/package=devtools
[CRAN]: https://cran.r-project.org/
