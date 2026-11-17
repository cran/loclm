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
X <- matrix(runif(101 * d), ncol = d)
y <- apply(X, 1, banana) + rnorm(nrow(X), sd = sigma)

## here's where we'll predict

newX <- matrix(runif(23 * d), ncol = d)
newy <- apply(newX, 1, banana) # leave off the noise

## ----colnames-----------------------------------------------------------------
## to assign column names

# nms <- paste0("x", 1:ncol(X)) # or something else
# colnames(X) <- nms
# colnames(newX) <- nms

## ----fit_default--------------------------------------------------------------
## default span

fit <- loclm(X, y)
pp <- predict(fit, newX)
cex <- 0.6
col <- c("darkblue", "forestgreen")
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col, xlab = "x[2]", ylab = "Actual/predicted",
  main = "Default span")
legend("topleft", c("actual", "predicted"), pch = 1:2, col = col,
  cex = par("cex.axis"), bty = "n")

## ----LOO1---------------------------------------------------------------------
## try different spans, can be executed in parallel

mc <- options("mc.cores")
options(mc.cores = 2L) # or more

span_vals <- c(seq(0.1, 0.2, 0.02), 0.75)
rmse <- parallel::mclapply(span_vals, \(span) {
    loo_resid <- LOO_loclm(fit, span = span)
    sqrt(mean(loo_resid^2))
  }) |> unlist()
show(cbind(span = span_vals, rmse = rmse))
spanhat <- span_vals[which.min(rmse)]
show(c(span = spanhat))

## ----LOO2---------------------------------------------------------------------
fit <- loclm(X, y, span = spanhat)
pp <- predict(fit, newX)
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col, xlab = "x[2]", ylab = "Actual/predicted",
  main = sprintf("span = %s (optimal)", format(spanhat)))
legend("topleft", c("actual", "predicted"), pch = 1:2, col = col,
  cex = par("cex.axis"), bty = "n")

## ----factor-------------------------------------------------------------------
## first input is a factor

breaks <- seq(0, 1, 0.2)
X <- as.data.frame(X) # into a data.frame
X[[1]] <- factor(LETTERS[cut(X[[1]], breaks)]) # five levels
show(head(X, 10))

newX <- as.data.frame(newX)
newX[[1]] <- factor(LETTERS[cut(newX[[1]], breaks)])

## ----factor1------------------------------------------------------------------
## fit, don't bother to retune

fit <- loclm(X, y, span = spanhat)
pp <- predict(fit, newX)
par(op)
matplot(newX[, 2], cbind(newy, pp), pch = c(1, 2), cex = cex,
  col = col, xlab = "x[2]", ylab = "Actual/predicted",
  main = sprintf("First input is a factor"))
legend("topleft", c("actual", "predicted"), pch = 1:2, col = col,
  cex = par("cex.axis"), bty = "n")

## ----parallel, eval = FALSE---------------------------------------------------
# ## parallel processing -- NOT RUN
# 
# if (requireNamespace("future", quietly = TRUE)) {
# 
#   big_newX <- do.call("rbind", rep(list(newX), 1E4))
#   show(dim(big_newX))
#   t1 <- system.time(pp1 <- predict(fit, big_newX))
#   show(t1) # 22 sec
# 
#   future::plan(future::multisession, workers = 2) # two cores
#   t2 <- system.time(pp2 <- predict(fit, big_newX))
#   show(t2) # 11 sec
#   all.equal(pp1, pp2) # TRUE
# 
#   future::plan(future::sequential) # single core
# }

## ----hatvalues----------------------------------------------------------------
## effective number of params

npeff <- sum(hatvalues(fit))
show(npeff)
fit_inf <- loclm(X, y, span = Inf)
np_inf <- sum(hatvalues(fit_inf))
show(np_inf) # 7 (hopefully!)

## ----wrap-up------------------------------------------------------------------
## reset the session

par(oldpar)
options(mc.cores = mc$mc.cores)

