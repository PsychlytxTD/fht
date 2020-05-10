#' @title Plot correlations against target variable
#' @name plot_corr_target
#' @description Plot all correlations against a target variable.
#'
#' @param df A tibble
#' @param fct_reorder Re-order factors?
#' @param fct_rev Reverse factors?
#' @param include_lbl Logical scalar - include label in plot?
#' @param lbl_precision Numeric scalar indicating placement of label
#' @param size Numeric scalar indicating size of plot geoms
#' @param line_size Numeric scalar indicating line size for plot
#' @param vert_size Numeric scalar indicating size of segment
#' @param color_pos Character scalar for color (positive correlations)
#' @param color_neg Character scalar for color (negative correlations)
#'
#' @importFrom rlang enquo quo_name !!
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_segment geom_vline expand_limits scale_color_manual geom_label
#' @importFrom dplyr mutate select arrange case_when
#' @importFrom tidyquant theme_tq
#' @importFrom forcats fct_reorder fct_rev
#'
#' @examples
#' mtcars %>% plot_cor(target = mpg, fct_reorder = T, fct_rev = F, lbl_position = "outward")
#' @export


plot_corr_target <- function(df, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {

  feature_expr <- rlang::enquo(target)
  feature_name <- rlang::quo_name(feature_expr)

  data_cor <- df %>%
    fht::calc_corr_target(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    dplyr::mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    dplyr::mutate(Correlation = dplyr::case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())

  g <- data_cor %>%
    ggplot2::ggplot(ggplot2::aes_string(x = feature_name, y = "feature", group = "feature")) +
    ggplot2::geom_point(aes(color = Correlation), size = size) +
    ggplot2::geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    ggplot2::geom_vline(xintercept = 0, color = tidyquant::palette_light()[[1]], size = vert_size) +
    ggplot2::expand_limits(x = c(-1, 1)) +
    tidyquant::theme_tq() +
    ggplot2::scale_color_manual(values = c(color_neg, color_pos))

  if (include_lbl) g <- g + ggplot2::geom_label(aes(label = feature_name_text),
                                                hjust = lbl_position)

  return(g)

}


