
#' @export
collatz <- function(n) {
  generator({
    yield(n)
    while(n > 1) {
      if(n %% 2 == 0) {
        n <- n / 2
      } else {
        n <- 3*n + 1
      }
      yield(n)
    }
  })
}

#' @export
collatz_shortcut <- function(n) {
  generator({
    yield(n)
    while(n > 1) {
      if(n %% 2 == 0) {
        n <- n / 2
      } else {
        n <- (3*n + 1) / 2
      }
      yield(n)
    }
  })
}

#' @export
collatz_check <- function(n, terminating) {
  generator({
    while(!(n %in% terminating)) {
      yield(n)
      if(n %% 2 == 0) {
        n <- n / 2
      } else {
        n <- (3*n + 1) / 2
      }
    }
  })
}

#' @export
collatz_sequence_gen <- function() {
  i <- 1
  generator({
    while(TRUE) {
      it <- collatz(i)
      yield(drain_int(it))
      i <- i + 1
    }
  })
}

#' @export
collatz_sequence_check <- function() {
  terminating <- 1
  i <- 1
  generator({
    while(TRUE) {
      it <- collatz_check(i, terminating)
      seq <- drain_int(it)
      yield(seq)
      terminating <- c(seq, terminating)
      i <- i + 1
    }
  })
}


f <- function(n) {
  if(n %% 6 == 4) {
    return(c(2 * n, (n - 1) / 3))
  } else {
    return(2 * n)
  }
}

#' @export
inverse_collatz <- function(start = 1) {
  a <- start
  generator({
    while(TRUE) {
      yield(a)
      a <- a %>% map(f) %>% reduce(c)
    }
  })
}






