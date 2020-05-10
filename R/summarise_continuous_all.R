#' @title Summarise Continuous Variables
#' @name summarise_continuous_all
#' @description Generate a tibble of key statistics for numeric variables.
#'
#' @param df A tibble.
#'
#' @importFrom dplyr select_if group_by summarise_at mutate select
#' @importFrom tidyr gather
#' @importFrom psych skew kurtosi
#'
#' @examples
#' summarise_continuous(mtcars)
#'
#' @export

summarise_continuous_all <- function(df, ...) {


  df %>% dplyr::select_if(is.numeric) %>%
    tidyr::gather("variable", "value", ...) %>%
    dplyr::group_by(variable, .add = TRUE) %>%
    dplyr::summarise_at(
      "value",
      list(
        N =      ~ length(.),
        mean =   ~ mean(.),
        median = ~ median(.),
        mode =   ~ (function(v) unique(v)[which.max(tabulate(match(v, unique(v))))])(.),
        sd =     ~ sd(.),
        min =    ~ min(.),
        max =    ~ max(.),
        skewness = ~ psych::skew(.),
        kurtosis = ~ psych::kurtosi(.)
      )
    ) %>%
    dplyr::mutate(
      range = paste(min, "-", max),
      cv = 100 * sd / mean
    ) %>% dplyr::select(-min, -max)
}

