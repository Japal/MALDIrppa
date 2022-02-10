# MALDIrppa

MALDI mass spectrometry data robust pre-processing and other helper functions

<!-- badges: start -->
[![R build status](https://github.com/Japal/MALDIrppa/workflows/R-CMD-check/badge.svg)](https://github.com/Japal/MALDIrppa/actions)
<!-- badges: end -->

This package helps to implement a robust approach to deal with mass spectrometry (MS) data. It is aimed at alleviating reproducibility issues and pernicious effects of deviating signals on both data pre-processing and downstream data analysis. Based on robust statistical methods, it facilitates the identification and filtering of low-quality mass spectra and atypical peak profiles as well as monitoring and data handling through pre-processing, which extends existing computational tools for MS data. `MALDIrppa` integrates with and extends existing R packages for MS proteomics data. Helper functions are included that allow to export data into formats used for downstream analyses.

### Installation

The latest version of the package is available on CRAN and can be installed from R using

```
install.packages("MALDIrppa")
```

Alternatively, it can be installed from Github through the `devtools` package:

```
# For non-windows users
devtools::install_github(repo = "Japal/MALDIrppa")
# For windows users
devtools::install_url(url="https://github.com/Japal/MALDIrppa/archive/master.zip", INSTALL_opt= "--no-multiarch")
```

For compatibility with previous pre-processing pipelines, a previous version of MALDIrppa can be installed from source files. For example, for v1.0.5-1:

```
install.packages("https://cran.r-project.org/src/contrib/Archive/MALDIrppa/MALDIrppa_1.0.5-1.tar.gz", repo=NULL, type="source")
```


### Getting started

```
# Loading the library
library("MALDIrppa")
```

Documentation and examples are available through the help pages (`?MALDIrppa`).

### Illustrative pipeline

The package's vignette provides a walk through the main features and functions:

* Article on website: https://japal.github.io/MALDIrppa/
* CRAN: https://cran.r-project.org/web/packages/MALDIrppa/vignettes/MALDIrppa_vignette.html

### Citation

Palarea-Albaladejo J., McLean K., Wright F. and Smith (2018). MALDIrppa: quality control and robust analysis for mass spectrometry data. Bioinformatics 34(3):522–523. <doi: http://dx.doi.org/10.1093/bioinformatics/btx628>
