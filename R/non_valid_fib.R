
#' @export
non_valid_fib <- function() {
  generator({
    f01 <- general_fib(0, 1)
    f10 <- general_fib(1, 0)

    while(TRUE) {
      yield(genus(f01(), f10()))
    }
  })
}

#' @export
non_valid_fib_size <- function() {
  generator({
    f01 <- general_fib(0, 1)
    f10 <- general_fib(1, 0)

    while(TRUE) {
      yield(genus_size(f01(), f10()))
    }
  })
}



