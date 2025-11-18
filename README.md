
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseNumeric

<figure>
<img
src="https://github.com/nikhildevireddy/SDS375_Homework6/actions/workflows/R-CMD-check.yaml/badge.svg"
alt="R-CMD-check" />
<figcaption aria-hidden="true">R-CMD-check</figcaption>
</figure>

`sparseNumeric` is an R package that implements an S4 class
(`sparse_numeric`) for representing sparse numeric vectors and
performing efficient operations on them without converting to a full
dense representation.

The package extends the code originally written for Homework 5 by
adding:

- A complete R package structure  
- Additional sparse vector utilities (`mean`, `norm`, `standardize`)  
- Full roxygen2 documentation  
- Robust unit tests targeting ≥90% code coverage  
- Automated R CMD check via GitHub Actions  
- A pkgdown website

------------------------------------------------------------------------

## Installation

To install directly from GitHub:

\`\`\`r \# install.packages(“devtools”)
devtools::install_github(“nikhildevireddy/SDS375_Homework6”)
