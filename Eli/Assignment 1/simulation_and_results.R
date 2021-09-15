
#function simulating n means, biases, standard errors and CI's out of standard normal distirbution with 100 observations
simneyman <- function(n, metrics){
  metrics <- c('Mean', 'Bias', 'Se', 'Upper', "Lower") #define metrics
  mat <- data.frame(matrix(NA, 0, length(metrics))) #create storage for output
  
  #actual simulation
  for(i in 1:n){
    sam <-rnorm(100, 0, 1)
    M <- mean(sam)
    bias <- M
    se <- 1/sqrt(100)
    ub <- M+1.96*se
    lb <- M-1.96*se
    vals <- c(M, bias, se, ub, lb)
    mat <- rbind(mat,vals)
  }
  
  colnames(mat) <- metrics #set names
  return(mat) #return the results
}

set.seed(6164900) #set seed for reproducibility
results <- simneyman(100) #simulate 100 times

#load necessary packages
packages <- c('tidyr', 'plyr', 'dplyr', 'magrittr', 'ggplot2')
lapply(packages, library, character = T)

results %$% sum(Upper < 0 | Lower > 0) #5 occurences where either upperlimit < 0 or Lower limit > 0
results <- results %>% 
  mutate(`True mean in CI?` = ifelse(test = (Upper < 0 | Lower > 0), yes = 'no', no = 'yes')) #if out of bounds, give 1, otherwise 0

#plot the data using geom_point() and geom_errorbar()
ggplot(results, aes(y=Mean, x=1:100, colour = `True mean in CI?`)) + 
  geom_hline(aes(yintercept = 0), color = "black", size = 1) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) + 
  geom_text(label = row.names(results))+
  xlab("Simulation") +
  ylab("Means and 95% Confidence Intervals") +
  theme_classic()
 