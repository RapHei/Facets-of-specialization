# Merge all metrics to initial DF

setwd('C:/Users/ac135138/Documents/GitHub/Facets-of-specialization')

# Load initial DF
df.main <- foreign::read.dta(file = 'Data/Main_Repo.dta') # N = 82.363


# HHI
source('Code/R/Repo_HHI.R') # output: 'focus'
df.main <- merge(df.main, focus,
                 by = 'ID',
                 all.x = T)

# Novelty 
source('Code/R/Repo_Novelty.R') # 'novelty'
df.main <- merge(df.main, novelty,
                 by = 'ID',
                 all.y = T)

# Consistency
# source('Code/R/Repo_Consistency.R') # 'consistency' (takes a bit; change to "load(file = 'Data/Repo_Consistency_Final.RData')" after running it 1st time 
load(file = 'Data/Repo_Consistency_Final.RData')
df.main <- merge(df.main, consistency,
                 by = 'ID',
                 all.x = T)
df.main$Consistency.Mean[is.na(df.main$Consistency.Mean)] <- 0
df.main$Pub.Cum.Year[is.na(df.main$Pub.Cum.Year)] <- 0
df.main$Pubs_Before_Total[is.na(df.main$Pubs_Before_Total)] <- 0


# TargetSpec
source('Code/R/Repo_TargetSpec.R') # df
base <- 0.9
df.main <- merge(df.main, df, 
                 by = "ID",
                 all.x = T)

## save to stata
df.main$Pub_Total <- df.main$Pub.Cum.Year + df.main$Pubs_Before_Total
foreign::write.dta(df.main, file = paste('Output/Stata/Repo_Event_DF_Final_', base, '.dta', sep = ''))


# To construct DF for Model 1: same as above, just omit novelty

# Load initial DF
df.main <- foreign::read.dta(file = 'Data/Main_Repo.dta') # N = 82.363

# HHI
load(file = 'Data/Repo_HHI.RData') # file-name: 'focus'
df.main <- merge(df.main, focus,
                 by = 'ID',
                 all.x = T)

# Consistency
load(file = 'Data/Repo_Consistency_Final.RData') # 'consistency'
df.main <- merge(df.main, consistency,
                 by = 'ID',
                 all.x = T)
df.main$Consistency.Mean[is.na(df.main$Consistency.Mean)] <- 0
df.main$Pub.Cum.Year[is.na(df.main$Pub.Cum.Year)] <- 0
df.main$Pubs_Before_Total[is.na(df.main$Pubs_Before_Total)] <- 0


# TargetSpec
base <- 0.9 # default
load(file = paste('Data/Repo_TargetSpec_', base, '.RData', sep = '')) # 'df'
df.main <- merge(df.main, df, 
                 by = "ID",
                 all.x = T)

## save to stata
df.main$Pub_Total <- df.main$Pub.Cum.Year + df.main$Pubs_Before_Total
foreign::write.dta(df.main, file = 'Output/Stata/Repo_Event_DF_Final_M1.dta')