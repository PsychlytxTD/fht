#' @title Calculate correlations against a target variable
#' @name calc_cor_target
#' @description Calculate correltions for all numeric variables against a target variable.
#'
#' @param df A tibble
#' @param target Name of continuous target variable
#' @param use A string indicating missing valuable treatment for correlation analysis
#' @param fct_reorder Re-order factors?
#' @param fct_rev Reverse factors?
#'
#' @importFrom rlang !! enquo quo_name
#' @importFrom dplyr mutate mutate_if select filter arrange case_when
#' @importFrom forcats fct_reorder fct_rev
#' @importFrom tibble tibble
#'
#' @examples
#' get_cor(mtcars, mpg)
#'
#' @export


calc_corr_target<- function(df, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {

  feature_expr <- rlang::enquo(target)
  feature_name <- rlang::quo_name(feature_expr)

  data_cor <- df %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_if(is.factor, as.numeric) %>%
    stats::cor(use = use) %>%
    tibble::as.tibble() %>%
    dplyr::mutate(feature = names(.)) %>%
    dplyr::select(feature, !! feature_expr) %>%
    dplyr::filter(!(feature == feature_name)) %>%
    dplyr::mutate_if(is.character, as_factor)

  if (fct_reorder) {
    data_cor <- data_cor %>%
      dplyr::mutate(feature = forcats::fct_reorder(feature, !! feature_expr)) %>%
      dplyr::arrange(feature)
  }

  if (fct_rev) {
    data_cor <- data_cor %>%
      dplyr::mutate(feature = forcats::fct_rev(feature)) %>%
      dplyr::arrange(feature)
  }

  return(data_cor)

}

