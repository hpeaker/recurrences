
#' @import purrr
NULL

#' @import flowery
NULL

#' @export
gcd2 <- function(a, b) {
  if(b == 0) {
    return(a)
  } else {
    gcd(b, a %% b)
  }
}

#' @export
gcd <- function(...) {
  list(...) %>% reduce(gcd2)
}

#' @export
gcd_accumulate <- function(...) {
  list(...) %>% accumulate(gcd2)
}

#' @export
frobenius2 <- function(a, b) {
  if(gcd2(a, b) != 1) {
    return(Inf)
  }
  if(a == 1 || b == 1) {
    return(integer(0))
  }
  a * b - a - b
}

#' @export
frobenius_ub_naive <- function(...) {
  if(gcd(...) != 1) {
    return(Inf)
  }
  l <- list(...)
  if(any(l == 1)) {
    return(integer(0))
  }
  ordered_n <- sort(unlist(l))
  a1 <- ordered_n[1]
  a2 <- ordered_n[2]

  return(frobenius2(a1, a2))
}

#' @export
frobenius_ub_brauer <- function(...) {
  if(gcd(...) != 1) {
    return(Inf)
  }
  l <- list(...)
  if(any(l == 1)) {
    return(integer(0))
  }

  d <- gcd_accumulate(...)
  t <- list(unlist(l)[-1], d[-1], d[-length(d)]) %>%
    pmap(~ (..1 * ..3) / ..2) %>%
    reduce(`+`)

  t - reduce(l, `+`)
}

#' @export
nsg_genus2 <- function(a, b) {
  f <- frobenius2(a, b)
  if(length(f) == 0) {
    warning("No frobenius number, genus is empty")
    return(integer(0))
  }
  if(!is.finite(f)) {
    stop("Genus is infinite")
  }
  if(f == 1) {
    return(1)
  }

  mults_a <- a * 0:floor(f / a)
  mults_b <- b * 0:floor(f / b)

  possible_sums <- rowSums(expand.grid(mults_a, mults_b))

  (1:f)[-possible_sums]
}

#' @export
nsg_genus <- function(...) {
  f <- frobenius_ub_brauer(...)
  if(length(f) == 0) {
    warning("No frobenius number, genus is empty")
    return(integer(0))
  }
  if(!is.finite(f)) {
    stop("Genus is infinite")
  }
  if(f == 1) {
    return(1)
  }

  l <- list(...)
  mults <- l %>% map(~ . * 0:floor(f / .))

  possible_sums <- rowSums(exec(expand.grid, mults))

  (1:f)[-possible_sums]
}

