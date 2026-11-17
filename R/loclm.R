#' Local Linear Regression
#'
#' @description
#' A bare-bones implementation of local linear regression, using high-level functions.  Can handle both numerical and non-numerical (factor) variables.  Non-numerical regressors are converted to factors.
#'
#' @param x Dataframe or numeric matrix of regressors.  If a dataframe, `x` must contain at least one numeric variable.
#' @param y Numeric vector of responses.
#' @param span Proportion of points in the neighbourhood.  Can be larger than one: `span = Inf` is equivalent to linear regression.
#' @param object Object with S3 class `"loclm"`.
#' @param newdata Dataframe or matrix at which to predict, defaults to `x`.
#' @param ... For compatibility with S3 generic [predict()].
#'
#' @details Local linear regression fits a plane to the points in the neighbourhood of \eqn{x}, in order to predict \eqn{y(x)}.  These points are weighted in the fit using the tricube weighting function.  Because the neighbourhood and the weights depend on a measure of distance, numeric variables in `x` and `newdata` are prescaled using [scale_df()].  Factors are converted to columns of dummy variables.
#'
#' The distance between different levels of the same factor (in dummy variables) is `1` or `sqrt(2)`.  The mean distance between two independent standard Normal random variables is a little over one (about 1.13).  Therefore standardizing the numeric variables and dummying the factors puts all variables on roughly the same footing, as far as distance is concerned.
#'
#' The behaviour with missing values can be complicated, and therefore only complete cases are used, see [complete.cases()].
#'
#' @section Quadratic regression: Modifying the code to use quadratic regression is fairly straightforward: the main difference is that the model matrix is not longer the same as the data matrix.  However, there can be a lot of extra terms, especially when including factors with more than two levels.  This creates a prediction with low bias but high variance, especially for extrapolation.  For four or fewer numeric variables, use [stats::loess()] with `degree = 2` (the default).  Otherwise, change the code at your own risk.
#'
#' @section Parallel implementation: `predict.loclm()` uses [future.apply::future_vapply()] under the hood.  The default is sequential (one core).  If `newdata` has many rows it will be faster to use multiple cores using
#'```
#' plan(multisession)
#'```
#' before calling `predict.loclm()`.  See the vignette for an example, and [future::plan()] for more details about setting up a plan.
#'
#' @seealso [stats::loess()] is faster and more flexible. However, this function can only handle up to four variables, all numeric.
#'
#' [LOO_loclm()] for leave-one-out cross-validation to set `span`.
#'
#' [hatvalues.loclm()] calculates hat values.
#'
#' [x2m()] creates the model matrix (not including the constant).  This is a faster version than working through [model.matrix()].
#'
#' @returns A list with S3 class `"loclm"`. This has a `predict()` method which returns a numeric vector of point predictions for the rows of `newdata`.
#'
#' There are also `fitted()` and `residuals()` methods, although these are just wrappers for `predict()`.
#'
#' @references More detail about local regression:
#'
#' T. Hastie, R. Tibshirani, J. Friedman (2009), The Elements of Statistical Learning, Springer, 2nd ed, ch6.
#'
#' C. Loader (1999), Local Regression and Likelihood, Springer, ch2.
#'
#' @examples
#' ## loess example
#'
#' fit <- loclm(cars[, "speed", drop=FALSE], cars$dist)
#' newd <- data.frame(speed = seq(5, 30, 1))
#' yhat <- predict(fit, newd)
#' cex <- 0.5
#' plot(newd$speed, yhat, pch = 19, cex = cex, xlab = "Speed",
#'   ylab = "Predicted distance", main = "Cars dataset")
#'
#' ## stackloss data
#'
#' x <- stack.x
#' y <- stack.loss
#' fit <- loclm(x, y, span = 0.5)
#' yhat <- fitted(fit)
#' cex <- 0.8
#' plot(y, yhat, pch = 19, cex = cex, xlab = "Actual",
#'   ylab = "Predicted", main = "Stackloss dataset", asp = 1)
#' abline(a = 0, b = 1, lty = 2)
#'
#' ## add bogus factor
#'
#' x <- as.data.frame(x)
#' x$ferret <- factor(rep(LETTERS[1:3], length.out = nrow(x)))
#' y <- y + as.numeric(x$ferret)
#'
#' fit <- loclm(x, y, span = 0.5)
#' show(head(fit$m)) # now with ferret!
#' yhat <- fitted(fit)
#' plot(y, yhat, pch = 19, cex = 0.8, xlab = "Actual",
#'   ylab = "Predicted", main = "Stackloss dataset, with ferret",
#'   asp = 1)
#' abline(a = 0, b = 1, lty = 2)
#'
#' ## hat values
#'
#' infl <- hatvalues(fit)
#' show(sum(infl)) # 17.6, effective number of parameters
#' fit0 <- loclm(x, y, span = Inf) # linear
#' infl0 <- hatvalues(fit0)
#' show(sum(infl0)) # 6 parameters
#'
#' ## compare with linear model
#'
#' fit1 <- lm(y ~ ., data.frame(x, y = y))
#' infl1 <- hatvalues(fit1)
#' all.equal(infl0, infl1, check.attributes = FALSE) # TRUE
#' \donttest{
#' ## some timings
#'
#' big.x <- data.frame(A = x, B = x)
#' big.x <- do.call("rbind", rep(list(big.x), 100)) # 2100 x 8
#' big.y <- rep(y, length.out = nrow(big.x))
#' system.time({
#'   fit <- loclm(big.x, big.y)
#'   yhat <- predict(fit, big.x)
#' }) # about 2s
#'
#' # see the vignette for parallel predict
#' }
#'
#' @name loclm
#' @rdname loclm
#' @export
loclm <- function(x, y, span = 0.75) {

  x <- as.data.frame(x, stringsAsFactors = TRUE)
  stopifnot(is.numeric(y), length(y) == nrow(x),
    is.scalar(span), span > 0)

  ## complete cases only

  cc <- complete.cases(x, y)
  if (!all(cc)) {
    message(sprintf("loclm() : %i complete cases out of %i",
      sum(cc), length(cc)))
    x <- x[cc, , drop=FALSE]
    y <- y[cc]
  }

  ## replace x with model matrix and return

  m <- x2m(x)
  sc <- attr(m, "scale")
  structure(
    list(m = m, y = as.vector(y), scale = sc, span = span),
    class = c("loclm", "list"))
}

#' @rdname loclm
#' @export
predict.loclm <- function(object, newdata, ...) {
  if (!inherits(object, "loclm")) {
    stop("first argument must be a \'loclm\' object")
  }
  if (missing(newdata)) {
    newM <- object$m
  } else {
    newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    if (anyNA(newdata)) {
      stop("NAs in \'newdata\'")
    }
    newM <- try(x2m(newdata, object$scale), silent = TRUE)
    if (inherits(newM, "try-error")) {
      stop("cannot scale \'newdata\' using \'object$scale\'")
    }
  }

  M <- object$m
  y <- object$y
  n <- length(y)
  jj <- rep(1:ncol(M), each = n)
  span <- object$span
  span_min <- (1 + ncol(M)) / n
  span <- max(span, span_min)
  rm(object); gc() # little tidy up ahead of possible forking
  future.apply::future_vapply(
    X = 1:nrow(newM),
    FUN = \(i) {
      v <- newM[i, ]
      dd <- sqrt(rowSums((M - v[jj])^2)) # like sweep() but faster
      h <- if (span <= 1) {
        quantile(dd, probs = span, names = FALSE)
      } else {
        span * max(dd)
      }
      keep <- which(dd <= h)
      coff <- lm.wfit(
        x = cbind(1, M[keep, , drop=FALSE]), # add constant
        y = y[keep],
        w = (1 - (dd[keep] / h)^3)^3 # tricube
      )$coefficients
      sum(coff * c(1, v), na.rm = TRUE) # trouble with dummies
    }, FUN.VALUE = double(1L)
  )
}

#' @rdname loclm
#' @export
fitted.loclm <- function(object, ...) {
  stopifnot(inherits(object, "loclm"))
  predict.loclm(object)
}

#' @rdname loclm
#' @export
residuals.loclm <- function(object, ...) {
  stopifnot(inherits(object, "loclm"))
  yhat <- predict.loclm(object)
  object$y - yhat
}
