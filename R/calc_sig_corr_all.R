#' @title Calculate all significant correlations
#' @name calc_sig_corr_all
#' @description Calculate all significant correlations in a dataset and store in a tibble.
#'
#' @param df A tibble
#'
#' @importFrom rstatix cor_test
#' @importFrom dplyr select_if filter arrange row_number
#'
#' @examples
#' calc_sig_corr_all(mtcars)
#'
#' @export

calc_sig_corr_all<- function(df) {

  rstatix::cor_test(dplyr::select_if(df, is.numeric)) %>%
    dplyr::filter( p < .05) %>% dplyr::arrange(p) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 1, cor != 1) %>%
    print(n = Inf)
}
