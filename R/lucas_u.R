
#' @export
lucas_u_general_seeds <- function(a = 0, b = 1, p = 1, q = -1) {
  generator({
    while(TRUE) {
      yield(a)
      a <- p * b - q * a
      yield(b)
      b <- p * a - q * b
    }
  })
}

