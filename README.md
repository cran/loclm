
# loclm

<!-- badges: start -->
<!-- badges: end -->

A bare-bones implementation of local linear regression, using
high-level functions.  Can handle both numerical and non-numerical
(factor) variables.  Non-numerical regressors are converted to factors.

## Installation

You can install the current version of loclm like so:

``` r
install.packages("loclm") # once only
```

## Example

``` r
## local linear fit for the stack-loss data

library(loclm)

x <- stack.x
y <- stack.loss
fit <- loclm(x, y, span = 0.5)
yhat <- fitted(fit)
```

See the help file ?loclm for more examples.

## Versions

### 1.1.0 Improvements for speed:

* new function x2m() to replace calls to scale_df() and model.matrix().

* new function LOO_loclm() to do leave-one-out more efficiently.

* option of parallel predict using the 'future.apply' package.

* faster inline implementation of sweep().

* other small tidy-ups of the code and vignette.

* scale_df() is no longer used, but retained for backward compatibility.

### 1.0.0. Initial version, submitted Aug 2026. Three functions:

* loclm() : local linear regression

* predict(), fitted(), residuals(), hatvalues() : S3 regression methods.

* scale_df() : scale numeric variables in a data.frame.

