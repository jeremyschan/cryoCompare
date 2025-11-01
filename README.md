
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cryoCompare

<!-- badges: start -->

<!-- badges: end -->

Comparisons for cryo-ET segmentation and denoising algorithms for
BCB410H: Applied Bioinformatics and the Qu Lab.

## Description

cryoCompare is an R package designed to analyse cryo-electron tomography
(cryo-ET) data by retrieving tomograms from the CryoET Data Portal and
comparing the performance of segmentation and denoising algorithms. It
improves current bioinformatics workflows by providing a streamlined,
standardised way to access algorithm accuracy and visualise differences
across denoising and segmeentation methods, which has not previously
been available in R. The package was developed using
`R version 4.4.2 (2024-10-31)`, `Platform: aarch64-apple-darwin20`, and
`Running under: macOS 26.0.1`.

cryoCompare was developed as part of the BCB410H: Applied Bioinformatics
course at the University of Toronto, and for the Qu Lab at the National
University of Singapore.

## Installation

To install the latest version of the package:

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("jeremyschan/cryoCompare", build_vignettes = TRUE)
library("cryoCompare")
```

## Overview

``` r
ls("package:cryoCompare")
data(package = "cryoCompare")
browseVignettes("cryoCompare")
```

`cryoCompare` contains 5 functions: 1. ***runPipeline*** for running the
complete denoising and segmentation pipeline

2.  ***runDenoising*** for applying denoising algorithms to tomograms

3.  ***runSegmentation*** for applying segmentation algorithms to
    tomograms

4.  ***thresholdSeg*** for threshold-based segmentation of tomograms

5.  ***viewSegmentation*** for visualising segmentation results

The package contains some sample data from the CZ Biohub. Refer to the
package vignettes for more details. An overview of the package is
illustrated below.

## Contributions

## References

<https://cryoetdataportal.czscience.com/datasets/10006?ground_truth=true> -
for example dataset with ground truth

## Acknowledgements

This package was developed as part of an assessment for Fall 2025
BCB410H: Applied Bioinformatics course at the University of Toronto.
`cryoCompare` welcome issues, enhancement requests, and other
contributions. To submit an issue, use GitHub issues.

## Package Structure

The package tree structure is provided below:

``` r
- cryoCompare
  |- cryoCompare.rProj
  |- DESCRIPTION
  |- NAMESPACE
  |- LICENSE
  |- README
  |- data
     |- TS_001.133.tif
     |- TS_001.133_ground_truth.tif
  |- inst
     |- extdata
  |- man
     |- runDenoising.Rd
     |- runSegmentation.Rd
     |- viewSegmentation.Rd
     |- thresholdSeg.Rd
     |- runPipeline.Rd
  |- R
    |- runDenoising.R
    |- runPipeline.R
    |- runSegmentation.R
  |- vignettes
  |- tests
  
```
