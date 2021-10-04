# Facet of specialization: Focus

setwd('WoS')

# Load theta
load('Github/Data/Theta_Repo.RData') # theta
topics <- paste('X', 1:60, sep = '')

# Calculate Herfindahl (HHI)
theta$hhi <- apply(theta[, topics], 1, DescTools::Herfindahl)


## For robustness checks
### Share of max.topic (its theta)
theta$max.share <- apply(theta[, topics], 1, max)


### Entropy 
entro <- function(vec, normalize){
  pi <- as.matrix(vec/sum(vec))
  if(normalize == TRUE){
    H <- -sum( (pi * log(pi, base = 2)) / log(length(vec), base = 2))
  }
  if(normalize == FALSE){
    H <- -sum(pi * log(pi, base = 2))
  }
  return(H)
}

theta$entro<- apply(theta[, topics], 1, FUN = entro, normalize = TRUE)

focus <- theta[, c('ID', 'hhi', 'max.share', 'entro')]

save(focus, file = 'Github/HHI/Repo_HHI.RData')