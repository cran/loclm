## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.asp = 0.65,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(loclm)
show(packageVersion("loclm"))
show(date())

## ----setup1-------------------------------------------------------------------
oldpar <- par(mgp = c(2.5, 0.7, 0), mar = c(4, 4, 2, 0.5), family = "Hershey",
  cex.main = 0.8, cex.axis = 0.8, cex.lab = 0.8, las = 1)
op <- par(no.readonly = TRUE)

## ----banana function----------------------------------------------------------
#### d-dimensional Banana function

## (d >= 2 is implied by x)

banana <- function(x) {
  stopifnot(is.vector(x, mode = "numeric"))
  d <- length(x)
  if (d == 1) {
    x <- c(x, 1)
    d <- 2L
  }
  robj <- 0
  for (j in seq_len(d - 1)) {
    robj <- robj + 100 * (x[j]^2 - x[j+1])^2 + (x[j] - 1)^2
  }
  robj
}

## ----make_dataset-------------------------------------------------------------
## make a dataset

set.seed(1234) # for reproducibility, feel free to change

d <- 3
sigma <- 1
X <- matrix(runif(d * 101), ncol = d)
y <- apply(X, 1, banana) + rnorm(nrow(X), sd = sigma)

## here's where we'll predict

newX <- matrix(runif(d * 23), ncol = d)
newy <- apply(newX, 1, banana) # leave off the noise

## ----fit_default--------------------------------------------------------------
## default span

fit <- loclm(X, y)
pp <- predict(fit, newX)
cex <- 0.6
col <- c("darkblue", "forestgreen")
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col,
  main = "Default span")

## ----LOO0---------------------------------------------------------------------
## LOO cross-validation

LOO <- function(X, y, reg, ...) {
  n <- length(y)
  yhat <- vapply(seq.int(n), \(i) {
    fit <- reg(X[-i, , drop=FALSE], y[-i], ...)
    predict(fit, X[i, , drop=FALSE])
  }, FUN.VALUE = 0)
  list(yhat = yhat, res = y - yhat)
}

## ----LOO1---------------------------------------------------------------------
## try different spans

span_vals <- c(seq(0.1, 0.2, 0.01), 0.75)
rmse <- vapply(span_vals, \(span) {
    loo <- LOO(X, y, reg = loclm, span = span)
    sqrt(mean(loo$res^2))
  }, FUN.VALUE = 0)
show(cbind(span = span_vals, rmse = rmse))
spanhat <- span_vals[which.min(rmse)]
show(c(span = spanhat))

## ----LOO2---------------------------------------------------------------------
fit <- loclm(X, y, span = spanhat)
pp <- predict(fit, newX)
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col,
  main = sprintf("span = %s (optimal)", format(spanhat)))

## ----factor-------------------------------------------------------------------
## first input is a factor

breaks <- seq(0, 1, 0.2)
X <- data.frame(X)
X[[1]] <- factor(LETTERS[cut(X[[1]], breaks)])
show(head(X, 10))

newX <- data.frame(newX)
newX[[1]] <- factor(LETTERS[cut(newX[[1]], breaks)])

## OK, here is the fit

fit <- loclm(X, y, span = spanhat)
pp <- predict(fit, newX)
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col,
  main = sprintf("First input is a factor"))

## ----wrap-up------------------------------------------------------------------
## reset the session

par(oldpar)

