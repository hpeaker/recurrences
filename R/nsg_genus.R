
#' @export
genus <- function(a, b) {
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

  n1 <- min(c(a, b))
  n2 <- max(c(a, b))

  p <- 0:floor(f / n1)
  q <- 0:floor(f / n2)

  pp <- rep(p, each = length(q))
  qq <- rep(q, length(p))

  g <- f - n1 * pp - n2 * qq
  g[g > 0]
}

#' @export
genus_size <- function(a, b) {
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

  d <- 0:floor(f / b)

  sum(ceiling((f - d * b) / a))
}
