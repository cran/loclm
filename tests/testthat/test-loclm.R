#### test loclm with stackloss

test_that("loclm() works", {

expect_num <- function(x, n) {
  if (!(typeof(x) %in% c("integer", "double")) || length(x) != n) {
    fail(
      sprintf("Expected numeric of length %i", n),
      sprintf("Actual is %s of length %s", typeof(x),
        format(length(x))))
  }
  pass()
  invisible(x)
}

library(datasets)
x <- stack.x
y <- stack.loss
expect_s3_class(fit <- loclm(x, y), "loclm")
z <- x[sample.int(5, 5), ]
expect_num(yhat <- predict(fit, z), 5)

## drops training data with missing values

x[2,1] <- NA
y[3] <- NA
expect_s3_class(fit <- loclm(x, y), "loclm")

## fail if z has any NAs

z[1, 1] <- NA
expect_error(predict(fit, z))

## now add a factor

x <- as.data.frame(stack.x)
x$ferret <- factor(rep(LETTERS[1:3], length.out = nrow(x)))
y <- stack.loss + as.numeric(x$ferret)
expect_s3_class(fit <- loclm(x, y), "loclm")
z <- x[sample.int(5, 5), ]
expect_num(yhat <- predict(fit, z), 5)

## hat values, span = Inf

n <- nrow(stack.x)
expect_num(infl <- hatvalues(fit), n)
expect_s3_class(fit0 <- loclm(x, y, span = Inf), "loclm")
expect_num(infl0 <- hatvalues(fit0), n)
expect_equal(sum(infl0), 6)

## add a constant column

x$const <- 12.3
z$const <- 12.5
expect_s3_class(fit <- loclm(x, y), "loclm")
expect_num(yhat <- predict(fit, z), 5)

## error if z incompatible with x

z$Acid.Conc. <- NULL
expect_error(predict(fit, z))

## LOO analysis

loo <- LOO_loclm(fit)
loo1 <- LOO_loclm(fit, span = 0.3)
expect_warning(LOO_loclm(fit, span = 0.1))

})
