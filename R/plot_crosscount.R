#' @title Plot Crosscounts
#' @name plot_crosscount
#' @description Plot counts and percentages of one categorical variable by another.
#'
#' @param df A tibble, returned from calc_crosscount()
#' @param interactive If true, a plotly plot is generated.
#'
#' @importFrom rlang parse_exprs !! as_string
#' @importFrom ggplot2 ggplot aes geom_text geom_col position_stack xlab ylab coord_flip theme
#' @importFrom tidyquant theme_tq scale_fill_tq
#' @importFrom plotly ggplotly layout
#'
#' @examples
#' calc_crosscount(mtcars, am, cyl) %>% plot_crosscount()
#'
#' @export

plot_crosscount<- function(df, interactive = FALSE, ...) {

  var_names<- rlang::parse_exprs(names(df))

  p<- ggplot(df, aes(x = !! var_names[[1]], y = pct, fill = !! var_names[[2]],
                     text = paste0("Category: ", !! var_names[[2]], ": ", "<br>",
                                   "N in category: ", n,  "<br>",
                                   "Percent in category: ", round(pct, 2), "<br>",
                                   "N for all categories combined: ", total_n))) +
    geom_col() + geom_text(aes(label = paste0(round(pct, 2), "%")), position = position_stack(vjust = 0.5),
                           size = 3, color = "white") +
    tidyquant::theme_tq() + ylab("Percentage (%)") + xlab(rlang::as_string(var_names[[1]])) +
    theme(legend.position = "bottom") + coord_flip() +
    tidyquant::scale_fill_tq()

  if(interactive == FALSE) {

    p

  } else {

    plotly::ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))

  }

}



#' @title Plot Crosscounts for All Categorical Variables Against Target Variable
#' @name plot_crosscount_all
#' @description Plot counts and percentages for target categorical variable against all others.
#'
#' @param df A tibble
#' @param interactive If true, a plotly plot is generated.
#'
#' @importFrom rlang parse_exprs !! as_string
#' @importFrom ggplot2 ggplot aes geom_text geom_col position_stack xlab ylab coord_flip theme
#' @importFrom tidyquant theme_tq scale_fill_tq
#' @importFrom plotly ggplotly layout
#'
#' @examples
#' fht::calc_crosscount_all(mtcars, cyl) %>% plot_crosscount_all()
#'
#' @export

plot_crosscount_all<- function(df, interactive = FALSE) {

  var_names<- rlang::parse_exprs(names(df))

  p<- ggplot2::ggplot(df, ggplot2::aes(x = category, y = pct, fill = !! var_names[[2]],
                                       text = paste0("Category: ", !! var_names[[2]], ": ", "<br>",
                                                     "N in category: ", n,  "<br>",
                                                     "Percent in category: ", round(pct, 2), "<br>",
                                                     "N for all categories combined: ", total_n))) +
    ggplot2::geom_col() + ggplot2::geom_text(aes(label = paste0(round(pct, 2), "%")),
                                             position = ggplot2::position_stack(vjust = 0.5),
                                             size = 3, color = "white") +
    tidyquant::theme_tq() + ggplot2::ylab("Percentage (%)") +
    ggplot2::theme(legend.position = "bottom") + ggplot2::coord_flip() + ggplot2::xlab("Variable Category") +
    tidyquant::scale_fill_tq()

  if(interactive == FALSE) {

    p

  } else {

    plotly::ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))

  }

}

