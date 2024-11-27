#' Keep rows containing missing values
#'
#' `keep_na()` retains rows where any column specified by `...` contains a
#' missing value.
#'
#' @details
#' Another way to interpret `keep_na()` is that it only keeps the "incomplete"
#' rows (where rows contain missing values). Internally, this "incompleteness" is
#' computed through [vctrs::vec_detect_missing()].
#'
#' @param data A data frame.
#' @param ... <[`tidy-select`][tidyr_tidy_select]> Columns to inspect for
#'   missing values. If empty, all columns are used.
#' @examples
#' df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
#' df %>% keep_na()
#' df %>% keep_na(x)
#'
#' vars <- "y"
#' df %>% keep_na(x, any_of(vars))
#' @export
keep_na <- function(data, ...) {
  check_dots_unnamed()
  UseMethod("keep_na")
}

#' @export keep_na.data.frame
keep_na.data.frame <- function(data, ...) {
  dots <- enquos(...)

  if (is_empty(dots)) {
    # Use all columns if no `...` are supplied
    cols <- data
  } else {
    vars <- tidyselect::eval_select(expr(c(!!!dots)), data, allow_rename = FALSE)
    cols <- data[vars]
  }

  loc <- vec_detect_missing(cols)
  out <- vec_slice(data, loc)

  reconstruct_tibble(data, out)
}
