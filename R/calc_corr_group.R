#' @title Calculate correlations by group
#' @name calc_corr_group
#' @description For two continuous variables, get correlations by leves of a categorical variable
#'
#' @param df A tibble
#' @param cont_var_1 A numeric vector (continuous variable)
#' @param cont_var_2 A numeric vector (continuous variable)
#' @param ... One or more categorical variables
#'
#' @importFrom tidyr drop_na nest unnest
#' @importFrom dplyr filter n mutate select arrange
#' @importFrom purrr map
#' @importFrom stats cor.test
#' @importFrom broom tidy
#'
#' @export

calc_corr_group<- function(df, cont_var_1, cont_var_2, ...) {

df<- dplyr::select(df, {{cont_var_1}}, {{cont_var_2}}, ...)

df %>% tidyr::drop_na() %>%
  dplyr::group_by(...) %>% dplyr::filter(n() >= 10) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    test = purrr::map(data, ~ stats::cor.test(.x[[1]], .x[[2]])),
    # S3 list-col
    tidied = purrr::map(test, broom::tidy)
  ) %>%
  tidyr::unnest(cols = tidied) %>% dplyr::mutate(p.value = fht::format_p_val(p.value)) %>%
  select(-data, -test) %>% dplyr::arrange(p.value)

}

