#' Hat Values for Local Linear Regression
#'
#' Compute the diagonal of the hat matrix.  The trace of the hat matrix (the sum of the diagonal) measures the effective number of parameters.
#'
#' @param model Object with S3 class `"loclm"`, see [loclm()].
#' @param ... For compatibility with S3 generic [hatvalues()].
#'
#' @returns A vector of hat values.
#'
#' @examples
#' ## see ?loclm
#'
#' @export
hatvalues.loclm <- function(model, ...) {
  if (!inherits(model, "loclm")) {
    stop("argument must have S3 class \"loclm\"")
  }
  x <- model.matrix(terms(model$x), model$x)
  span <- model$span
  rm(model)
  N <- nrow(x)
  d <- ncol(x) # includes intercept
  span <- max(span, (1 + d) / N)
  vapply(1:N, \(i) {
    dd <- sqrt(rowSums(sweep(x, 2L, x[i, ], "-")^2))
    h <- if (span <= 1) {
      quantile(dd, probs = span, names = FALSE)
    } else {
      span * max(dd)
    }
    keep <- which(dd <= h)
    Qf <- qr.Q(qr(
      sqrt((1 - (dd[keep] / h)^3)^3) *
      x[keep, , drop=FALSE]
    ))[, 1:d, drop=FALSE]
    sum(Qf[keep == i, ]^2)
  }, FUN.VALUE = 0)
}
