
#' @export
general_trib <- function(a = 0, b = 0, c = 1) {
  generator({
    while(TRUE) {
      yield(a)
      a <- a + b + c
      yield(b)
      b <- b + c + a
      yield(c)
      c <- c + a + b
    }
  })
}

#' @export
general_quad <- function(a = 0, b = 0, c = 0, d = 1) {
  generator({
    while(TRUE) {
      yield(a)
      a <- a + b + c + d
      yield(b)
      b <- b + c + d + a
      yield(c)
      c <- c + d + a + b
      yield(d)
      d <- d + a + b + c
    }
  })
}

#' @export
general_pent <- function(a = 0, b = 0, c = 0, d = 0, e = 1) {
  generator({
    while(TRUE) {
      yield(a)
      a <- a + b + c + d + e
      yield(b)
      b <- b + c + d + e + a
      yield(c)
      c <- c + d + e + a + b
      yield(d)
      d <- d + e + a + b + c
      yield(e)
      e <- e + a + b + c + d
    }
  })
}



