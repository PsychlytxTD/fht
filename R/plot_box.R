#' @title Plot a Boxplot
#' @name plot_box
#' @description Plot boxplots for a continuous variable across zero or one categorical variable.
#'
#' @param df A tibble.
#' @param cont_var A continuous variable.
#' @param grp_var A categorical variable to group by
#' @param identifier A variable to identify individual points if interactive = TRUE
#' @param interactive If true, plots are passed to plotly and display tooltips.
#'
#' @importFrom dplyr select pull
#' @importFrom rlang quo_is_null
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_text geom_jitter xlab ylab
#' @importFrom plotly ggplotly
#' @importFrom tidyquant theme_tq
#'
#' @examples
#'
#' plot_box(mtcars, cont_var = mpg, grp_var = cyl, identifier = am)
#'
#' @export

plot_box<- function(df, cont_var, grp_var = NULL, identifier, interactive = FALSE) {

  unquoted_cont_var<- dplyr::select(df, {{ cont_var }}) #Needed to be able to use variable outside of aes()
  unquoted_cont_var_name<- names(unquoted_cont_var) #Needed for ylab
  unquoted_cont_var_values<- dplyr::select(df, {{ cont_var }}) %>% dplyr::pull() #Needed to be able to use mean()
  #outside quasiquotation context

  #Needed to make the y- aesthetic to aes() optional

  if(!rlang::quo_is_null(enquo(grp_var))) {

    grp_var<- dplyr::select(df, {{ grp_var }} ) %>% dplyr::pull() %>% as.factor() %>% droplevels()

  } else {

    grp_var<- ""

  }

  unquoted_identifier_name<- names(dplyr::select(df, {{ identifier }}))

  plot<- ggplot(df, aes(x = grp_var, y = {{ cont_var }})) +

    # make boxplot transparent with alpha = 0
    geom_boxplot(alpha = 0) +

    # If a given point is an outlier, colour it red

    geom_jitter(aes(text = paste(unquoted_identifier_name, {{ identifier }}, "<br>",
                                 unquoted_cont_var_name, "value:", {{ cont_var }}, "<br>",
                                 unquoted_cont_var_name, "mean:", round(mean({{ cont_var }}, na.rm = TRUE), 2), "<br>",
                                 unquoted_cont_var_name, "sd:", round(sd({{ cont_var }}, na.rm = TRUE), 2)


    )), color = ifelse(unquoted_cont_var > (mean( unquoted_cont_var_values, na.rm = TRUE) +
                                              2 * sd( unquoted_cont_var_values, na.rm = TRUE)), "#18BC9C", "#2c3e50"), alpha = 0.7) +

    tidyquant::theme_tq() + ylab(paste(unquoted_cont_var_name)) + xlab("")

  if(interactive == FALSE) {

    plot

  } else {

    plot %>% ggplotly(tooltip = "text")

  }

}


#' @title Plot All Boxplots
#' @name plot_box_all
#' @description Plot boxplots for all continuous variable across zero or one categorical variable.
#'
#' @param df A tibble.
#' @param cont_var A continuous variable.
#' @param grp_var A categorical variable to group by
#' @param idenitifer A variable to identify individual points if interactive = TRUE
#' @param interactive If true, plots are passed to plotly and display tooltips.
#' @param n_rows How many rows should tbe present in he faceted panel of plots?
#' @param margins Margins to be passed to plotly::subplot() if interactive = TRUE.
#'
#' @importFrom dplyr select select_if pull filter
#' @importFrom rlang enquo !! quo_is_null
#' @importFrom purrr pmap set_names
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_text geom_jitter xlab ylab
#' @importFrom plotly ggplotly subplot
#' @importFrom patchwork wrap_plots
#' @importFrom tidyquant theme_tq
#'
#' @examples
#' plot_box(mtcars, cont_var = mpg, grp_var = cyl, identifier = am)
#'
#' @export

plot_box_all<- function(df, grp_var = NULL, identifier, interactive = FALSE,
                        n_rows = 4, margins = c(0.059, 0.02, 0.02, 0.02)) {

  identifier<- dplyr::select(df, {{ identifier}} )
  identifier_names<- names(identifier)

  #Iterate over all numeric variables
  #Iterate over names of these variables (needed for labels)

  num_vars<- dplyr::select_if(df, is.numeric)
  num_var_names<- names(num_vars)


  #Needed to make the y- aesthetic to aes() optional

  if(!rlang::quo_is_null(enquo(grp_var))) {

    grp_var<- dplyr::select(df, {{ grp_var }} ) %>% dplyr::pull() %>% as.factor() %>% droplevels()

  } else {

    grp_var<- ""

  }

  plot_list<- purrr::pmap(list(num_vars, num_var_names, identifier), ~{


    plot<- ggplot(df, aes(x = grp_var, y = ..1)) +

      # make boxplot transparent with alpha = 0
      geom_boxplot(alpha = 0) +

      # If a given point is an outlier, colour it red

      geom_jitter(
        aes(text = paste(identifier_names, ..3, "<br>",
                         ..2, "value:", ..1, "<br>",
                         ..2, "mean:", round(mean(..1, na.rm = TRUE), 2), "<br>",
                         ..2, "sd:", round(sd(..1, na.rm = TRUE), 2), "<br>",
                         ..2, "N outliers:",
                         nrow(dplyr::filter(df,
                                            ..1 > (mean(..1, na.rm = TRUE) + 2 * sd(..1, na.rm = TRUE))))

        )), color = ifelse(..1 > (mean(..1, na.rm = TRUE) +
                                    2 * sd(..1, na.rm = TRUE)), "#18BC9C", "#2c3e50"), alpha = 0.7) +

      tidyquant::theme_tq() + ylab(paste(..2)) + xlab("")

    if(interactive == FALSE) {

      plot

    } else {

      plotly::ggplotly(plot, tooltip = "text")

    }

  })

  #Due to plotly bug, we need to set opacity of boxplot markers
  #to zero, so that outliers aren't duplicated on the plot.
  #Below, go inside each plot object in the list to do this.

  for(i in 1:length(plot_list)) {

    plot_list[[i]][["x"]][["data"]][1][[1]][["marker"]][["opacity"]]<- 0

  }

  plot_list<- plot_list %>% purrr::set_names(num_var_names)

  if(interactive == FALSE) {

    patchwork::wrap_plots(plot_list, nrow = n_rows)

  }  else {

    plotly::subplot(plot_list, nrows = n_rows, titleY=T, margin = margins)
  }

}
