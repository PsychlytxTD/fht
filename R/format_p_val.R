#' @title Format p-values
#' @name format_p_val
#' @description Format p-values to 5 decimal places
#'
#' @param p_val_column A numeric scalar or vector of p-values
#'
#' @examples
#' format_p_val(x)
#'
#' @export

format_p_val<- function(p_val_column) {
  round(as.numeric(format(p_val_column, scientific = FALSE)), 5)
}
