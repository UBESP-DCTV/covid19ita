#' Delayed cumulation
#'
#' given a vector cumulate the sum supposing each elements count
#' for a given period of time.
#'
#' @param x vector to cumulate
#' @param n number of "times" each element count in the sum
#'
#' @return a vector of lenght length(x) + n -1
#' @export
#'
#' @examples
#'
#' cumulate_for_days(c(1, 2, 3), 2) # c(1, 3, 5, 3)
#' cumulate_for_days(c(1, 2, 3), 3) # c(1, 3, 6, 5, 3)
#' cumulate_for_days(c(NA, 2, 3), 3) # c(0, 2, 5, 5, 3)
cumulate_for_days <- function(x, n) {
  purrr::map(seq_len(n), ~c(rep(0, .), x, rep(0, n - .))[-1]) %>%
    purrr::map(~{
      .x[is.na(.x)] <- 0
      .x
    }) %>%
    purrr::reduce(`+`)
}
