# Facet of specialization: Targeted specialization

setwd('WoS')

# Functions
## Iterate over all rows and binarize by threshold
thres.rows <- function(x, thres){
  temp <- x > thres
  temp <- ifelse(temp == TRUE, 1, 0)
  return(temp)
}

## Workhorse-fct to derive whatever baseline
topic.contri.thres <- function(theta, year, baseline){
  # thetas from which we derive baseline (theses from prior year)
  temp.theta.prior <- theta[theta$Year == year-1, topics]
  thres <- stack(lapply(temp.theta.prior, quantile, prob = baseline, names = FALSE))
  thres <- thres$values
  # thetas we want to binarize
  temp.theta.current <- theta[theta$Year == year, topics]
  result <- t(apply(temp.theta.current, MARGIN = 1, FUN = thres.rows, thres = thres)) 
  result <- as.data.frame(result)
  # rename cols
  colnames(result) <-  gsub('X', 'T', colnames(result)) 
  # attach vars
  result$ID <- theta[theta$Year == year, 'ID']
  result$Year <- year
  result$Threshold <- baseline
  return(result)
}


### Construct "topic contributions" w/ different baselines
## Load theta
load('Github/Data/Theta_Repo.RData') # theta
topics <- paste('X', 1:60, sep = '')[-47] # exclude noise topic T47 (included in online supplemental)

## iterate over years
years <- c(1981:2015)
base <- 0.9 # alternative baselines (cf. online supplemental)

for(year in years){
  temp <- topic.contri.thres(theta, 
                             year = year,
                             baseline = base)
  if(year == min(years)){
    df <- temp
  }
  else{
    df <- rbind(df, temp)
  }
}

save(df, file = paste('Github/TargetSpec/Repo_TargetSpec_', base, '.RData', sep = ''))
