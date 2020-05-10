#' @title Calculate all correlations greater than .3 in dataset
#' @name calc_corr_all
#' @description Generate a list of tibbles - one per numeric variable - showing correlations above .3
#'
#' @param df A tibble
#'
#' @importFrom rlang syms !!
#' @importFrom dplyr select_if arrange desc filter
#' @importFrom purrr map set_names
#' @importFrom corrr correlate focus
#'
#' @examples
#' find_correlations(mtcars) %>% purrr::pluck("mpg")
#'
#' @export


calc_corr_all<- function(df) {

  numeric_data<- dplyr::select_if(df, is.numeric)

  numeric_var_names<- rlang::syms(names(numeric_data))

  correlations<- corrr::correlate(numeric_data)

  correlation_by_var_list<- purrr::map(numeric_var_names, ~{corrr::focus(correlations, !!.x) %>%
      dplyr::filter(!!.x >= 0.3 | !!.x <= -0.3) %>% dplyr::arrange(desc(!!.x))})

  correlation_by_var_list<- correlation_by_var_list %>% purrr::set_names(names(numeric_data))

  correlation_by_var_list
}

