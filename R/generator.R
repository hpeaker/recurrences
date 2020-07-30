
# generator <- function(body) {
#   if (!requireNamespace("flowery", quietly = TRUE)) {
#     stop("Package \"flowery\" needed for this function to work. Please install it.", call. = FALSE)
#   } else {
#     flowery::generator(body)
#   }
# }

check_flowery <- function() {
  if (!requireNamespace("flowery", quietly = TRUE)) {
    stop("Package \"flowery\" needed for this function to work. Please install it.", call. = FALSE)
  }
}
