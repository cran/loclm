#' Dataframe to Model Matrix
#'
#' Rescales numeric variables, expands out factors, and drops variables with no variation.
#'
#' @param x Matrix or dataframe.
#' @param scale Parameter of the transformation, see Details.
#'
#' @returns A numeric model matrix, _not_ including a constant, with a `"scale"` attribute.
#'
#' @details The `scaled` argument is a named list including the variables in `x`. If `scale[[j]]` is `0` the variable is ignored.  If it is postive, the numeric variable is rescaled.  If is it a character vector of levels, the factor is expanded out into dummy variables, dropping the first level.
#'
#' If `scale = NULL`, `scale` is deduced from `x`.
#'
#' @examples
#' ## stackloss data
#'
#' x <- data.frame(stack.x)
#' lev <- c("sable", "chocolate", "champagne", "silver")
#' x$Ferret <- factor(sample(lev, nrow(x), replace = TRUE), lev)
#' x$boring <- rep(17.2, nrow(x))
#' x$also_boring <- factor(rep(lev[2], nrow(x)), lev)
#' M <- x2m(x)
#' show(head(M))
#'
#' sc <- attr(M, "scale")
#' y <- x[, sample.int(ncol(x))] # permute
#' My <- x2m(y, sc)
#' identical(M, My) # TRUE
#'
#' @export
x2m <- function(x, scale = NULL) {
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  n <- nrow(x)

  if (is.null(scale)) {
    scale <- lapply(x, \(y) {
      if (length(unique(y)) == 1) return(0)
      if (is.numeric(y)) {
        return(c(mean(y, na.rm = TRUE), sd(y, na.rm = TRUE)))
      } else {
        return(levels(y))
      }
    }) # named list
  } else {
    stopifnot(is.list(scale), names(scale) %in% names(x))
  }
  nms <- names(scale)

  m <- matrix(numeric(0L), n, 0L, dimnames = list(row.names(x), NULL))
  for (j in seq_along(scale)) {
    sc <- scale[[j]]
    if (identical(sc, 0)) next
    y <- x[[ nms[j] ]]
    if (is.numeric(sc)) {
      y <- matrix((y - sc[1L]) / sc[2L], ncol = 1L,
        dimnames = list(NULL, nms[j]))
    } else {
      lev <- levels(y)
      stopifnot(!is.null(lev), all(lev == sc))
      y <- dummy(as.numeric(y))
      colnames(y) <- paste0(nms[j], lev[-1L])
    }
    m <- cbind(m, y)
  }

  structure(m, scale = scale)
}

dummy <- function(i) {
  n <- length(i)
  vapply(2:max(i), \(j) 0.0 + (i == j), FUN.VALUE = double(n))
}
