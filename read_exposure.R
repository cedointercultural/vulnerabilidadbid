#' @title Read exposure data
#' @description  Read exposure scenarios
#' @details INPUT: 1) data
#' @details OUTPUT: 1) object with data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

read_exposure <- function(eachfile){
  
  this.data.wide <- read_csv(eachfile) %>% 
    pivot_wider(names_from = variable, values_from = value) 
  
  print(str(this.data.wide))
  
  this.data <- read_csv(eachfile)
  return(this.data)
}