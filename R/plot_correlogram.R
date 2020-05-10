#' @title Plot a correlogram
#' @name plot_correlogram
#' @description Plot a correlogram of numeric variables
#'
#' @param df A tibble
#'
#' @importFrom stats cor
#' @importFrom corrplot cor.mtest corrplot
#'
#' @examples plot_correlogram(mtcars)
#'
#' @export

plot_correlogram<- function(df) {
  #First generate the correlation matrix. We want pairwise complete cases -
  #otherwise, only cases complete for
  #all variables will be used. And this number could be very small

  pairwise_correlations<- stats::cor(df, use = "pairwise.complete.obs")

  #Generate a matrix of p-values for this correlation grid

  corr_matrix<- corrplot::cor.mtest(pairwise_correlations, conf.level = .95)

  #Use the p-value matrix to generate a plot that crosses out non-significant correlations
  corrplot::corrplot(pairwise_correlations, p.mat = corr_matrix$p, sig.level = .05)

}

