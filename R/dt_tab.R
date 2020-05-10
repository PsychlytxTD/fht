#' @title Make Fixed-Columns Datatable
#' @name dt_tab
#' @description Render a fixed column DT table
#'
#' @importFrom DT datatable
#'
#' @export

dt_tab<- function(tab) {

DT::datatable(tab,
  extensions = 'FixedColumns',
  options = list(
    dom = 't',
    scrollX = TRUE,
    fixedColumns = TRUE
  )
)

}
