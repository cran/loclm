#' Scale a Dataframe
#'
#' @description Only rescale the numeric variables in a dataframe.
#'
#' \emph{Warning:} does not retain the default functionality of `base::scale()`, because the returned object is always a dataframe.  If `base::scale()` is called with a dataframe it will convert the returned object into a matrix.
#'
#' @param x Dataframe.
#' @param center Either a logical value or a numeric vector of length equal to the number of numeric columns of `x`.
#' @param scale Either a logical value or a numeric vector of length equal to the number of numeric columns of `x`.
#'
#' @details If `center` is numeric, `names(center)` identifies the numeric columns in `x`.  Otherwise, if `scale` is numeric, `names(scale)` identifies the numeric columns in `x`.  `center` and `scale` are passed to [base::scale()].
#'
#' @returns A dataframe like `x`, with numeric columns centered and/or scaled.  The numeric centering and scalings used (if any) are returned as attributes `"scaled:center"` and `"scaled:scale"`.
#'
#' @seealso [base::scale()], which this function uses but only on a subset of the columns.
#'
#' @examples
#' n <- 11
#' X <- data.frame(
#'   x = rnorm(n), 
#'   foo = factor(sample(LETTERS[1:3], n, replace = TRUE)),
#'   bar = sample(LETTERS[4:5], n, replace = TRUE),
#'   y = rexp(n, rate = 0.2))
#' print(Y <- scale_df(X))
#' str(Y) # see attributes
#'
#' ## scale only
#'
#' sc <- attr(Y, "scaled:scale") # has names
#' print(Z <- scale_df(X, center = FALSE, scale = sc))
#'
#' ## no numeric variables
#'
#' Y <- X[, c("foo", "bar")]
#' Z <- scale_df(Y)
#' identical(Y, Z) # TRUE
#'
#' ## error messages
#'
#' try(Y <- scale_df(X, center = c(foo = 1, bob = 2)))
#' try(Y <- scale_df(X, scale = c(foo = 1, bob = 2)))
#' 
#'
#' @export
scale_df <- function(x, center = TRUE, scale = TRUE) {

  if (!inherits(x, "data.frame")) {
    stop("first argument must be a dataframe")
  }
  num0 <- names(x)[sapply(x, is.numeric)]
  if (is.numeric(center)) {
    num1 <- names(center)
  } else if (is.numeric(scale)) {
    num1 <- names(scale)
  } else num1 <- num0
  if (!setequal(num0, num1)) {
    collapse <- function(x) paste(x, collapse = ", ")
    msg <- "incompatible names of numeric variables:
    * names(x) = %s
    * names(center or scale) = %s"
    stop(sprintf(msg, collapse(num0), collapse(num1)))
  }

  if (length(num1) > 0) {
    tmp <- base::scale(as.matrix(x[, num1, drop=FALSE]),
      center = center, scale = scale)
    x[, num1] <- tmp
    attr(x, "scaled:center") <- attr(tmp, "scaled:center")
    attr(x, "scaled:scale") <- attr(tmp, "scaled:scale")
  }
  x
}
