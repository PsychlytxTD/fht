#' @title Calculate Grouped Means
#' @name calc_group_mean
#' @description Calculate means by one or more grouping variables.
#'
#' @param df A tibble
#' @param cont_var A continuous variable
#'
#' @importFrom rlang enquo !!
#' @importFrom dplyr group_by summarise
#'
#' @examples
#' calc_group_mean(mtcars, mpg, cyl)
#'
#' @export

calc_group_mean <- function(df, cont_var, ...) {

  cont_var <- rlang::enquo(cont_var)

  df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(mean = mean(!!cont_var))
}
