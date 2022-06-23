#' @title Normalize function
#' @description  Normalize from 0-1
#' @details INPUT: 1) data
#' @details OUTPUT: 1) normalized data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
