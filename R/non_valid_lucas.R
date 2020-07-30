#' @export
non_valid_lucas_u_2_neg1 <- function() {
  generator({
    l01 <- lucas_u_general_seeds(0, 1, 2, -1)
    l10 <- lucas_u_general_seeds(1, 0, 2, -1)

    while(TRUE) {
      yield(genus(l01(), l10()))
    }
  })
}

#' @export
non_valid_lucas_u_2_neg1_size <- function() {
  generator({
    l01 <- lucas_u_general_seeds(0, 1, 2, -1)
    l10 <- lucas_u_general_seeds(1, 0, 2, -1)

    while(TRUE) {
      yield(genus_size(l01(), l10()))
    }
  })
}

