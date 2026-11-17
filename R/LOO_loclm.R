#' Leave-One-Out Tuning of the Span
#'
#' This is a quick version of leave-one-out, in which, for each row of the training dataset, the model is trained on all other rows and used to predict that row.
#'
#' @inheritParams loclm
#'
#' @returns A vector of residuals.  For tuning, several candidate values for the span can be evaluated in parallel, see the vignette.
#'
#' @examples
#' ## stackloss data
#'
#' x <- stack.x
#' y <- stack.loss
#' fit <- loclm(x, y, span = 0.5)
#' 
#' ## LOO-RMSE for different spans
#'
#' span_vals <- sort(c(seq(0.7, 1.6, 0.1), 0.75))
#' rmse <- sapply(span_vals, \(span) {
#'   loo_resid <- LOO_loclm(fit, span = span)
#'   sqrt(mean(loo_resid^2))
#' })
#' show(cbind(span = span_vals, RMSE = rmse))
#'
#' @export
LOO_loclm <- function(object, span = NULL) {
  if (!inherits(object, "loclm")) {
    stop("first argument must be a \'loclm\' object")
  }
  M <- object$m
  y <- object$y
  n <- length(y)
  jj <- rep(1:ncol(M), each = n)
  span <- span %||% object$span
  span_min <- (1 + ncol(M)) / n
  if (span < span_min) {
    warning("span must be at least ", format(span_min, digits = 3))
    return(rep(NA_real_, n))
  }
  rm(object); gc()
  yhat <- vapply(1:n, \(i) {
    v <- M[i, ]
    dd <- sqrt(rowSums((M - v[jj])^2))
    h <- if (span <= 1) {
      quantile(dd, probs = span, names = FALSE)
    } else {
      span * max(dd)
    }
    dd[i] <- Inf # drop row i from training data
    keep <- which(dd <= h)
    coff <- lm.wfit(
      x = cbind(1, M[keep, , drop=FALSE]), # add constant
      y = y[keep],
      w = (1 - (dd[keep] / h)^3)^3 # tricube
    )$coefficients
    sum(coff * c(1, v), na.rm = TRUE) # trouble with dummies
  }, FUN.VALUE = double(1L))
  y - yhat # residuals
}
