#' @title Normalize function
#' @description  Normalize from 0-1
#' @details INPUT: 1) data
#' @details OUTPUT: 1) normalized data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


pca_factor <- function(thisdata, datasetname){

  #eliminate column with row names, needs only data values
  
  data.set <- thisdata %>% 
    dplyr::select(-COM_ID,-NOM_LOC) %>% 
    as.data.frame
  
  items.scale  <- ncol(data.set)
  #obtain correlation matrix
  hozdatamatrix.impact <- cor(data.set, use="complete.obs")
  #print correlation plot
  #pairs(data.set)
  
  # print out the correlation matrix but ask for numbers to 3 decimal places
  corrmat.impact  <- round(hozdatamatrix.impact,3)
  corrmat.impact
  # bartlett test - want a small p value here to indicate c0rrelation matrix not zeros
  cortest.bartlett(data.set)
  
  #principal components analysis
  
  # Determine Number of Factors to Extract
  
  #This code can be use to determine the optimal number of factors, however Jacob et al (2012)
  #recommends using a single factor solution
  # Generally, if the goal is simple and parsimonious description of a correlation 
  # or covariance matrix, the first k principal components 
  # will do a better job than any other k-dimensional solution
  ev <- eigen(hozdatamatrix.impact) # get eigenvalues
  ap <- parallel(subject=nrow(data.set),var=ncol(data.set),
                 rep=100,cent=.05)
  try(nS <- nScree(ev$values, ap$eigen$qevpea))
  #plotnScree(nS)
  
  
  
  #run single factor solution
  model1.impact<- principal(data.set, nfactors = 1, rotate = "none",scores=TRUE) #can specify rotate="varimax"
  model1.impact
  #SS loading is the eigenvalue
  # h2is called the communality estimate. Measures the % of variance 
  # in an observed variable accounted for by the retained components
  
  
  # can find the reproduced correlations and the communalities (the diagonals)
  factor.model(model1.impact$loadings)
  
  model.loadings <- unclass(model1.impact$loadings) %>% 
    as_tibble(rownames="var") %>% 
    dplyr::rename(loadings = PC1) %>% 
    mutate(data_name = datasetname)
  
  write_csv(model.loadings,here("outputs","analysis",paste0("model_loadings_",datasetname,".csv"))) 
  
 
  # the diagonals represent the uniqueness values (1- R squared):
  residuals.impact <- factor.residuals(hozdatamatrix.impact, model1.impact$loadings)
  residuals.impact
  # nice to plot the residuals to check there are normally distributed
  #hist(residuals.impact)
  
  # to save the above values we need to add them to a dataframe
  factorscores.impact <- model1.impact$scores %>% 
    as_tibble(rownames="LOC")
  
  write_csv(factorscores.impact,here("outputs","analysis",paste0("factor_scores_",datasetname,".csv"))) 
  
  #obtain largest eigenvalue
  larg.eigen  <- max(model1.impact$values)
  #Armor's Theta tests for internal consistency of a factor scale 
  Theta  <- (items.scale/(items.scale-1)) * (1-(1/larg.eigen))
  
  #shows the summary of the loadings table
  
  p <- print(model1.impact)
  
  model.summary <-  round(p$Vaccounted,2)  %>% 
    as_tibble(rownames = "Var") %>% 
    bind_rows(tibble(Var = c("Eigenvalue","Theta"),PC1=c(larg.eigen, Theta))) %>% 
    filter(Var!="SS loadings")
    
  write_csv(model.summary,here("outputs","analysis",paste0("model_summary_",datasetname,".csv"))) 
  
}

