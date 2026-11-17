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
  M <- model$m
  y <- model$y
  N <- length(y)
  jj <- rep(1:ncol(M), each = N)
  span <- model$span
  span_min <- (1 + ncol(M)) / N
  span <- max(span, span_min)
  rm(model); gc()
  d <- ncol(M)
  vapply(1:N, \(i) {
    v <- M[i, ]
    dd <- sqrt(rowSums((M - v[jj])^2))
    h <- if (span <= 1) {
      quantile(dd, probs = span, names = FALSE)
    } else {
      span * max(dd)
    }
    keep <- which(dd <= h)
    Qf <- qr.Q(qr(
      sqrt((1 - (dd[keep] / h)^3)^3) *
      cbind(1, M[keep, , drop=FALSE])
    ))[, 1:(d+1), drop=FALSE]
    sum(Qf[keep == i, ]^2)
  }, FUN.VALUE = double(1L))
}
