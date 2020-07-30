#' @export
tribonacci_frobenius_numbers <- function() {
  generator({
    t001 <- general_trib(0, 0, 1)
    t010 <- general_trib(0, 1, 0)
    t100 <- general_trib(1, 0, 0)

    while(TRUE) {
      yield(bfd_frob(t100(), t010(), t001()))
    }
  })
}




#' @export
non_valid_trib <- function() {
  generator({
    t001 <- general_trib(0, 0, 1)
    t010 <- general_trib(0, 1, 0)
    t100 <- general_trib(1, 0, 0)

    while(TRUE) {
      yield(nsg_genus(t100(), t010(), t001()))
    }
  })
}

#' @export
non_valid_quad <- function() {
  generator({
    q0001 <- general_quad(0, 0, 0, 1)
    q0010 <- general_quad(0, 0, 1, 0)
    q0100 <- general_quad(0, 1, 0, 0)
    q1000 <- general_quad(1, 0, 0, 0)

    while(TRUE) {
      yield(nsg_genus(q1000(), q0100(), q0010(), q0001()))
    }
  })
}

#' @export
non_valid_pent <- function() {
  generator({
    p00001 <- general_pent(0, 0, 0, 0, 1)
    p00010 <- general_pent(0, 0, 0, 1, 0)
    p00100 <- general_pent(0, 0, 1, 0, 0)
    p01000 <- general_pent(0, 1, 0, 0, 0)
    p10000 <- general_pent(1, 0, 0, 0, 0)

    while(TRUE) {
      yield(nsg_genus(p00001(), p00010(), p00100(), p00010(), p10000()))
    }
  })
}
