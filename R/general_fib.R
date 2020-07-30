#' @export
fib <- function() {
  a <- 0
  b <- 1
  generator({
    while(TRUE) {
      yield(a)
      ab <- a + b
      a <- b
      b <- ab
    }
  })
}

#' @export
general_fib <- function(a = 0, b = 1) {
  generator({
    while(TRUE) {
      yield(a)
      a <- a + b
      yield(b)
      b <- a + b
    }
  })
}

