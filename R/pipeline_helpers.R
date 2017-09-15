# pipeline_helpers.R

#' repeat_add_columns
#'
#' Repeat a data frame for each entry in a second, binding the columns together.
#'
#' @param x Data frame (tibble) to repeat
#' @param y A copy of \code{x} is created for each row of this tibble
#' @return A repeated \code{x} with columns from \code{y} added.
#' @details This corresponds to \code{repeat_and_add_vector} in the old data system.
#'          (Note: KVC copied this from the \code{gcamdata} package.)
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
#' @examples
#' x <- tibble::tibble(x = 1:3)
#' y <- tibble::tibble(y = c(4, 5), z = c(6, 7))
#' repeat_add_columns(x, y)
repeat_add_columns <- function(x, y) {
  UNIQUE_JOIN_FIELD <- NULL           # silence package checks.
  assert_that(tibble::is_tibble(x))
  assert_that(tibble::is_tibble(y))

  x %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD)
}
