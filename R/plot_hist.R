#' @title Plot Histogram
#' @name plot_hist
#' @description Plot a single histogram
#'
#' @param data A tibble.
#' @param cont_var A continuous variable to plot.
#' @param bins A numeric scalar. Number of bins for histograms.
#' @param fill A character scalar. Fill colour for histogram bars.
#' @param colour A character scalor. Colour for outline of bars
#'
#' @importFrom dplyr mutate_if mutate
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap
#' @importFrom plotly ggplotly
#' @importFrom psych skew kurtosi
#' @importFrom tidyquant theme_tq palette_light
#'
#' @examples
#' plot_hist(mtcars)
#'
#' @export

plot_hist<- function(df, cont_var,  bins = 10, fill = tidyquant::palette_light()[[3]], colour = "white", interactive = FALSE) {

  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ cont_var }},
                                 text = paste("skew:", round(psych::skew( {{ cont_var }} ), 2),
                                              "<br>",
                                              "kurtosis:", round(psych::kurtosi( {{ cont_var }}), 2)))) +
    ggplot2::geom_histogram(bins = bins, fill = fill, colour = colour) +
    tidyquant::theme_tq()

  if(interactive == FALSE) {

    p

  } else {

    plotly::ggplotly(p, tooltip = "text")

  }


}


#' @title Plot Faceted Histograms
#' @name plot_hist_all
#' @description Plot histograms for all continuous variables in dataframe.
#'
#' @param data A tibble.
#' @param fct_reorder A logical scalar. Whether factor should retain natural ordering of levels.
#' @param fct_rev A logical scalar. Whether factor levels should be reversed
#' @param bins A numeric scalar. Number of bins for histograms.
#' @param fill A character scalar. Fill colour for histogram bars.
#' @param colour A character scalor. Colour for outline of bars
#' @param ncol A numeric scalar. Number of columns for faceting panel.
#' @param scale A character scalar. Are scales shared across facets for both rows and columns?
#' @param interactive If true, plots are passed to plotly and display tooltips.
#'
#' @importFrom dplyr mutate_if mutate
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap
#' @importFrom forcats fct_reorder fct_rev
#' @importFrom plotly ggplotly
#' @importFrom psych skew kurtosi
#' @importFrom tidyquant theme_tq palette_light
#'
#' @examples
#' plot_hist_all(mtcars)
#'
#' @export

plot_hist_all<- function(df, fct_reorder = FALSE, fct_rev = FALSE,
                            bins = 10, fill = tidyquant::palette_light()[[3]], colour = "white",
                            ncol = 5, scale = "free", interactive = FALSE) {

  data_factored <- df %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_if(is.factor, as.numeric) %>%
    tidyr::gather(key = key, value = value, factor_key = TRUE)

  if (fct_reorder) {
    data_factored <- data_factored %>%
      dplyr::mutate(key = as.character(key) %>% as.factor())
  }

  if (fct_rev) {
    data_factored <- data_factored %>%
      dplyr::mutate(key = fct_rev(key))
  }

  p <- data_factored %>%
    ggplot2::ggplot(ggplot2::aes(x = value, group = key,
                                 text = paste("skew:", round(psych::skew( value ), 2),
                                              "<br>",
                                              "kurtosis:", round(psych::kurtosi( value ), 2)))) +
    ggplot2::geom_histogram(bins = bins, fill = fill, colour = colour) +
    ggplot2::facet_wrap(~ key, ncol = ncol, scale = scale) +
    tidyquant::theme_tq()

  if(interactive == FALSE) {

    p

  } else {

    plotly::ggplotly(p, tooltip = "text")

  }

}
