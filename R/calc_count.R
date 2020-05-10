#' @title Get frequency and percentage counts for categorical variables
#' @name calc_count
#' @description Calculate counts & percentages for selection of categorical variables
#'
#' @param df A tibble.
#' @param col A fixed value that is quoted, representing number of cases.
#' @param show_percent Round and format percentages with %.

#' @importFrom rlang enquo !! !!! := expr
#' @importFrom dplyr count mutate ungroup arrange
#' @importFrom scales label_percent
#' @importFrom tidyr unite drop_na
#' @importFrom forcats fct_reorder
#'
#' @examples
#' mtcars %>% calc_count(cyl)
#' mtcars %>% calc_count(cyl, am)
#'
#' @export


calc_count<- function(df, ..., col = n, show_percent = TRUE) {

  df<- df %>% dplyr::count( ... ) %>% tidyr::drop_na() %>%
    dplyr::mutate(pct = ({{ col }}) / sum( {{ col }}), total_n = sum(n, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(ncol(df) > 3) {

    cols_to_unite<- paste(names(df)[1:(ncol(df) - 3)], collapse = "_")

    df<- df %>% tidyr::unite(!! cols_to_unite, ...)

  }


  df<- df %>% dplyr::mutate_at(vars(-n, -pct), ~forcats::fct_reorder(droplevels(as.factor(.x)), n)) %>%
    dplyr::arrange(desc(pct))

  if(show_percent == TRUE) {

    df<- df %>% dplyr::mutate(pct = scales::label_percent(accuracy = 0.01)(pct))

  }

  return(df)

}


#' @title Calculate All Frequency & Percentage Counts
#' @name calc_count_all
#' @description Calculate frequency and percentage caounts for all categorical variables.
#'
#' @param df A tibble.
#' @param col A fixed value that is quoted, representing number of cases.
#' @param show_percent Round and format percentages with %.
#' @param max_categories A numeric scalar. Treat numeric variable as categorical for max_categories.
#'
#' @importFrom purrr keep map2
#' @importFrom glue glue
#' @importFrom rlang enquo !!
#' @importFrom dplyr arrange select ungroup mutate mutate_at count
#' @importFrom tibble tibble
#' @importFrom scales label_percent
#' @importFrom tidyr drop_na
#'
#' @examples
#' mtcars %>% get_counts_all(max_categories = 3)
#'
#' @export


calc_count_all<- function(df, col = n, show_percent = TRUE, max_categories = 5) {

  vars_to_keep<- df %>% purrr::keep(~ is.character(.x) | length(unique(.x)) <= max_categories)

  if(ncol(vars_to_keep) < 1) {

    stop(glue::glue("There are no categorical variables or variables with less than (or equal to)
                    the number of unique values you specified."))

  } else {

    col_expr<- rlang::enquo(col)

    purrr::map2(vars_to_keep, names(vars_to_keep), ~ {

      df<- dplyr::count(tibble::tibble(category = .x), category) %>% tidyr::drop_na() %>%
        dplyr::mutate(pct = (!! col_expr) / sum(!! col_expr), variable = .y, total_n = sum(n, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(variable, everything())

      if(show_percent == TRUE) {

        df<- df %>% dplyr::mutate(pct = scales::label_percent(accuracy = 0.01)(pct))

      }

      df<- df %>% dplyr::mutate_at(vars(-n, -pct), ~forcats::fct_reorder(droplevels(as.factor(.x)), n)) %>%
        dplyr::arrange(desc(pct))

    })

  }


}




