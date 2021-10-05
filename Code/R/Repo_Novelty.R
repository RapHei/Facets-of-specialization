# Facet of specialization: Novelty

setwd('C:/Users/ac135138/Documents/GitHub/Facets-of-specialization')

# Functions
## Binarize DF, each cell counting two highest topics per thesis
binarize.topics <- function(temp){
  dummies <- fastDummies::dummy_cols(temp$max)
  dummies[,1] <- NULL
  names(dummies) <- gsub('.data_', '', names(dummies))
  dummies <- dummies[, gtools::mixedsort(names(dummies))]
  
  dummies2 <- fastDummies::dummy_cols(temp$max2)
  dummies2[,1] <- NULL
  names(dummies2) <- gsub('.data_', '', names(dummies2))
  dummies2 <- dummies2[, gtools::mixedsort(names(dummies2))]
  temp.bin <- dummies + dummies2
  return(temp.bin)
}

## Get n-highest value of a (topic) vector
max.n <- function(x,n=1) order(x,decreasing=TRUE)[n] 

## Calculation mirrors 'novel integration' (Leahey and Moody 2014) 
novelty.score <- function(theta, year, w){
  temp <- theta[theta$Year %in% ((year-w):(year-1)), c('max', 'max2')] # get t-1 (t = 'year')
  temp <- data.frame(apply(temp, 2, function(x){paste('X', x, sep = '')}))
  # binarize matrix of previous year 
  temp.bin <- binarize.topics(temp)
  # Expected number of combinations by chance, E_ij = p_i*p_j
  P_i <- colSums(temp.bin[, topics]) / nrow(temp.bin)
  # Observed combinations of i and j
  obs <- t(as.matrix(temp.bin)) %*% as.matrix(temp.bin)
  # Loop over thesis of year t and compare to prior year (t-1)
  thesis.t <- theta[theta$Year == year,]
  thesis.t$novel.inte <- NA
  for(temp.row in 1:nrow(thesis.t)){
    i <- thesis.t[temp.row, 'max']
    j <- thesis.t[temp.row, 'max2']
    E_ij <- P_i[i] * P_i[j]
    obs_ij <- obs[i, j] / nrow(temp.bin)
    novel.inte_ij <- 1- obs_ij / E_ij
    thesis.t$novel.inte[temp.row] <- novel.inte_ij
  }
  return(thesis.t[, c('ID','novel.inte')])
}


# Load theta and define 2 max-topics for each thesis
load('Data/Theta_Repo.RData') # theta

topics <- paste('X', 1:60, sep = '')
theta$max <- apply(theta[, topics], 1, max.n, n = 1)
theta$max2 <- apply(theta[, topics], 1, max.n, n = 2)

## Loop over years with window w (default: 3)
w <- 3
years <- (1980 + w):2015

for(year in years){
  thesis.t <- novelty.score(theta, year, w)
  if(year == min(years)){
    novelty <- thesis.t
  }
  else{
    novelty <- rbind(novelty, thesis.t)
  }
}

save(novelty, file = paste('Data/Repo_Novelty_', w,'.RData', sep = '')) 



## For robustness tests
windows <- c(2,4)

for(w in windows){
  years <- (1980 + w):2015
  for(year in years){
    thesis.t <- novelty.score(theta, year, w)
    if(year == min(years)){
      novelty <- thesis.t
    }
    else{
      novelty <- rbind(novelty, thesis.t)
    }
  }
  save(novelty, file = paste('Data/Repo_Novelty_', w,'.RData', sep = '')) 
}