#' @title Crosstablulate Counts & Percentages
#' @name calc_crosscount
#' @description Breakdown counts and percentages of one categorical variable by another.
#'
#' @param df A tibble
#' @param grp_var_1 A categorical variable
#' @param grp_var_2 A categorical variable
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr count mutate group_by ungroup
#' @importFrom tidyr nest drop_na
#'
#' @examples
#' calc_crosscount(mtcars, am, cyl)
#'
#' @export

calc_crosscount<- function(df, grp_var_1, grp_var_2) {

  nested_df<- df %>% mutate(identifier = {{ grp_var_1 }} ) %>%
    nest(data = -{{ grp_var_1 }} ) #%>% dplyr::ungroup(identifier)

  counts_df<- nested_df$data %>% purrr::map_dfr(~ {

    .x %>% dplyr::count(identifier, {{ grp_var_2 }}) %>% tidyr::drop_na() %>%
      dplyr::mutate(pct = (n / sum(n)) * 100) %>%
      dplyr::group_by(identifier) %>% dplyr::mutate(total_n = sum(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select( {{ grp_var_1 }}  := identifier, dplyr::everything())


  })

  counts_df<- counts_df %>%
    dplyr::mutate_if(is.character, as.factor)


  target_categories_ordered<- counts_df %>% dplyr::filter({{ grp_var_2  }} == tail(levels({{ grp_var_2 }}), 1)) %>%
    dplyr::arrange(desc(pct)) %>% dplyr::pull({{ grp_var_1 }})

  #Some variables have zero count of the last level of the target var.
  #So append these levels to the character vector after ordering.
  #Only do this if needed.

  to_append<- setdiff(levels(target_categories_ordered), target_categories_ordered)

  if(length(to_append) == 0 ) {

    new_levels<- target_categories_ordered

  } else {

    new_levels<- c(as.character(target_categories_ordered), to_append)

    #Re-order the category variable

    counts_df<- counts_df %>% dplyr::mutate(

      {{ grp_var_1 }} := forcats::fct_relevel({{ grp_var_1 }}, new_levels)

    )

  }

  counts_df

}


#' @title Caclculate Counts and Percentages for All Categorical Variables Against Another
#' @name calc_crosscount_all
#' @description For a target categorical variable, get counts and percentages for levels of all categorical variables.
#'
#' @param df A tibble
#' @param target_var A categorical variable to target in analysis.
#' @param max_categories A numeric scalar. Treat numeric variable as categorical for max_categories.
#'
#' @importFrom rlang parse_exprs
#' @importFrom tidyr pivot_longer unite
#' @importFrom dplyr group_by select summarise n mutate
#' @importFrom forcats fct_relevel
#'
#' @examples
#' calc_crosscount_all(mtcars, cyl)
#'
#' @export

calc_crosscount_all<- function(df, target_var, max_categories = 5) {

  #Only retain categorical variables

  df<- df %>% purrr::keep(~ is.character(.x) | length(unique(.x)) <= max_categories)

  #Derive single column representing levels of all variables.
  #Gather the data to get counts and percentages of target variable within levels of each variable category

  df<- df %>% tidyr::pivot_longer(cols = - {{ target_var }}, names_to = "variable") %>%
    dplyr::group_by({{ target_var }}, variable, value) %>%  dplyr::summarise(n = n()) %>% arrange(variable, value) %>%
    dplyr::select(variable, value, {{ target_var }}, n) %>% tidyr::unite("category", variable:value) %>%
    dplyr::group_by(category) %>% dplyr::mutate(pct = (n / sum(n, na.rm = TRUE)) * 100,
                                                total_n = sum(n, na.rm = TRUE)) %>% dplyr::ungroup() %>%
    dplyr::mutate(category = droplevels(factor(category)),
                  {{ target_var }} := droplevels(factor( {{ target_var }})))

  #Reorder the bars of the plot.
  # Reorder category (y-axis) by the last level of the target variable (which appears closest to y-axis)
  #Get the reordered category variable as a factor

  target_categories_ordered<- df %>% filter({{ target_var }} == tail(levels(factor( {{ target_var }})), 1)) %>%
    dplyr::arrange(desc(pct)) %>% dplyr::pull(category)

  #Some variables have zero count of the last level of the target var.
  #So append these levels to the character vector after ordering.
  #Only do this if needed.

  to_append<- setdiff(levels(target_categories_ordered), target_categories_ordered)

  if(length(to_append) == 0 ) {

    new_levels<- target_categories_ordered

  } else {

    new_levels<- c(as.character(target_categories_ordered), to_append)

  }

  #Re-order the category variable

  df<- df %>% dplyr::mutate(

    category = forcats::fct_relevel(category, new_levels)
  )

  df

}
