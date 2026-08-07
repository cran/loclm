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
#' One edge case is where a variable is a constant: scaling creates a variable of `NA`s.  These variables are removed with a warning, and will be ignored in the prediction.
#'
#' @section Quadratic regression: Modifying the code to use quadratic regression is fairly straightforward: the main difference is that the model matrix is not longer the same as the data matrix.  However, there can be a lot of extra terms, especially when including factors with more than two levels.  This creates a prediction with low bias but high variance, especially for extrapolation.  For four or fewer numeric variables, use [stats::loess()] with `degree = 2` (the default).  Otherwise, change the code at your own risk.
#'
#' @seealso [stats::loess()] is faster and more flexible. However, this function can only handle up to four variables, all numeric.
#'
#' [hatvalues.loclm()] calculates hat values.
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
#' plot(cars$speed, residuals(fit), pch = 19, cex = cex,
#'   main = "Residual plot")
#'
#' ## stackloss data
#'
#' x <- stack.x
#' y <- stack.loss
#' fit <- loclm(x, y, span = 0.5)
#' yhat <- fitted(fit)
#' plot(y, yhat, pch = 19, cex = 0.8, xlab = "Actual",
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
#' show(head(fit$x)) # now with ferret!
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
#' }
#'
#' @name loclm
#' @rdname loclm
#' @export
loclm <- function(x, y, span = 0.75) {
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  stopifnot(is.numeric(y), length(y) == nrow(x),
    is.scalar(span), span > 0)

  ## don't want constants

  drop <- lengths(lapply(x, unique)) == 1L
  if (any(drop)) {
    drop <- names(which(drop))
    warning("dropping constant columns, ", paste(drop, collapse = ", "))
    x <- x[, -match(drop, names(x)), drop=FALSE]
  } else drop <- NULL

  ## want at least one numeric variable

  if (!any(sapply(x, is.numeric))) {
    stop("\'x\' must contain at least one numeric variable.")
  }

  ## complete cases only

  cc <- complete.cases(x, y)
  if (!all(cc)) {
    message(sprintf("loclm() : %i complete cases out of %i",
      sum(cc), length(cc)))
    x <- x[cc, , drop=FALSE]
    y <- y[cc]
  }

  ## OK: check, scale, return

  stopifnot(nrow(x) >= 2L + ncol(x)) # must have >= 1 df
  x <- scale_df(x) # only the numeric columns
  scale <- list(center = attr(x, "scaled:center"),
    scale = attr(x, "scaled:scale"))
  attr(x, "scaled:center") <- attr(x, "scaled:scale") <- NULL
  x <- model.frame(~ ., data = x)
  structure(
    list(x = x, y = as.vector(y), drop = drop, scale = scale,
      span = span),
    class = c("loclm", "list"))
}

#' @rdname loclm
#' @export
predict.loclm <- function(object, newdata, ...) {
  if (!inherits(object, "loclm")) {
    stop("first argument must be a \'loclm\' object")
  }
  if (!missing(newdata)) {
    newdata <- na.fail(as.data.frame(newdata, stringsAsFactors = TRUE))
    newdata <- tryCatch(
      {
        if (!is.null(object$drop)) {
          newdata <- newdata[, -match(object$drop, names(newdata)), drop=FALSE]
        }
        df <- with(object$scale,
          scale_df(newdata, center = center, scale = scale))
        model.matrix(terms(object$x), df)
      },
      error = function(cnd) {
        stop("in predict(), incompatible \'newdata\'", call. = FALSE)
      }
    )
  }
  object$x <- model.matrix(terms(object$x), object$x)
  if (missing(newdata)) newdata <- object$x

  span <- max(object$span, (1 + ncol(object$x)) / nrow(object$x))
  apply(newdata, 1L, function(v) {
    dd <- sqrt(rowSums(sweep(object$x, 2L, v, "-")^2))
    h <- if (span <= 1) {
      quantile(dd, probs = span, names = FALSE)
    } else {
      span * max(dd)
    }
    keep <- which(dd <= h)
    coff <- lm.wfit(
      x = object$x[keep, , drop=FALSE],
      y = object$y[keep],
      w = (1 - (dd[keep] / h)^3)^3 # tricube
    )$coefficients
    coff[is.na(coff)] <- 0 # trouble with dummies
    sum(coff * v)
  })
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
