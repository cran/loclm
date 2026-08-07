## internal functions

is.scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}
