#' @title Plot all scatterplots
#' @name plot_scatter_all
#' @description Plot scatterplot for all possible pairs of numeric variables and store in list.
#'
#' @param df A tibble.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr select_if filter
#' @importFrom plotly plot_ly add_markers add_markers add_lines config layout
#' @importFrom tidyr unite
#' @importFrom purrr map2 set_names
#' @importFrom stats fitted
#'
#' @examples
#' all_scat<- plot_all_scatter(mtcars) ; all_scat %>% purrr::pluck("mpgANDdrat")
#'
#' @export

plot_scatter_all<- function(df) {

  df<- dplyr::select_if(df, is.numeric)

  #Create a two-column dataframe with all possible pairs of numeric variable names

  numeric_pairs<- tibble::tibble(x_variable = names(df),
                                 y_variable = x_variable) %>% expand.grid()

  #Iterate over the two columns of numeric variable names we've created and use them
  #to select variable from mtcars programatically

  plot_list<- purrr::map2(numeric_pairs$x_variable, numeric_pairs$y_variable, ~ {

    #Create linear models for each pair of variables — required for fitting a line
    #to each plot

    model<- lm(df[[.y]] ~ df[[.x]], data = df)

    #Create the plotly base for the plot. Programmatic mapping in plotly requires us
    #to specify variables using square bracket notation, rather than the
    #usual x = ~x, y = ~y. I don't know why.

    plotting_df<- tibble::tibble(x_plot_var = df[[.x]],
                                 y_plot_var = df[[.y]]) %>%
      dplyr::filter(complete.cases(.))

    plotly::plot_ly(plotting_df, x = ~x_plot_var, y = ~y_plot_var) %>%
      plotly::add_markers(x = ~jitter(x_plot_var), y = ~jitter(y_plot_var),
                  color = "#18BC9C", opacity = 0.5, showlegend = FALSE) %>%

      #Add the line of best fit to each plot

      plotly::add_lines(y = ~fitted(model), color = "#E31A1C")  %>%
      plotly::layout(title = paste0(.x, " vs ", .y,":", "
                            Correlation = ", round(cor(df[[.x]],
                                                       df[[.y]],
                                                       use = "pairwise.complete.obs"), 2)),
             xaxis = list(title = paste(.x)),
             yaxis = list(title = paste(.y))) %>% plotly::config(displayModeBar = F)
  })

  plot_names<- numeric_pairs %>% tidyr::unite("plot_labels", x_variable:y_variable,
                                              sep = "AND", remove = TRUE) %>% dplyr::pull(plot_labels)

  plot_list<- plot_list %>% purrr::set_names(plot_names)


}
