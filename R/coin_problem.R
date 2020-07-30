# Assumes gcd(a, b) = 1
frobenius <- function(a, b) {
  a * b - a - b
}

# Assumes gcd(a, b) = 1
non_creatable <- function(a, b) {
  f <- frobenius(a, b)
  if(f == 1) {
    return(1)
  }
  if(f < 1) {
    return(integer(0))
  }
  
  mults_a <- a * 0:floor(f / a)
  mults_b <- b * 0:floor(f / b)
  
  possible_sums <- rowSums(expand.grid(mults_a, mults_b))
  
  (1:f)[-possible_sums]
}

