# Merge all metrics to initial DF

# setwd('C:/Users/ac135138/Documents/Artikel/Global Research Trends/Article')
setwd('WoS')


# Load initial DF
df.main <- foreign::read.dta(file = 'Github/Data/Main_Repo.dta') # N = 82.363


# HHI (cf. 'Repo_HHI.R')
load(file = 'Github/HHI/Repo_HHI.RData') # file-name: 'focus'
df.main <- merge(df.main, focus,
                 by = 'ID',
                 all.x = T)

# Novelty 
load(file = 'Github/Novelty/Repo_Novelty.RData') # 'novelty'
df.main <- merge(df.main, novelty,
                 by = 'ID',
                 all.y = T)

# Consistency
load(file = 'Github/Consistency/Repo_Consistency_Final.RData') # 'consistency'
df.main <- merge(df.main, consistency,
                 by = 'ID',
                 all.x = T)
df.main$Consistency.Mean[is.na(df.main$Consistency.Mean)] <- 0
df.main$Pub.Cum.Year[is.na(df.main$Pub.Cum.Year)] <- 0
df.main$Pubs_Before_Total[is.na(df.main$Pubs_Before_Total)] <- 0


# TargetSpec
base <- 0.9 # default
load(file = paste('Github/TargetSpec/Repo_TargetSpec_', base, '.RData', sep = '')) # 'df'
df.main <- merge(df.main, df, 
                 by = "ID",
                 all.x = T)

## save to stata
df.main$Pub_Total <- df.main$Pub.Cum.Year + df.main$Pubs_Before_Total
foreign::write.dta(df.main, file = paste('Github/Output/Repo_Event_DF_Final_', base, '.dta', sep = ''))



# To derive Model 1: same as above, just outcomment novelty

# Load initial DF
df.main <- foreign::read.dta(file = 'Github/Data/Main_Repo.dta') # N = 82.363

# HHI (cf. 'Repo_HHI.R')
load(file = 'Github/HHI/Repo_HHI.RData') # file-name: 'focus'
df.main <- merge(df.main, focus,
                 by = 'ID',
                 all.x = T)

# # Novelty 
# load(file = 'Github/Novelty/Repo_Novelty.RData') # 'novelty'
# df.main <- merge(df.main, novelty,
#                  by = 'ID',
#                  all.y = T)

# Consistency
load(file = 'Github/Consistency/Repo_Consistency_Final.RData') # 'consistency'
df.main <- merge(df.main, consistency,
                 by = 'ID',
                 all.x = T)
df.main$Consistency.Mean[is.na(df.main$Consistency.Mean)] <- 0
df.main$Pub.Cum.Year[is.na(df.main$Pub.Cum.Year)] <- 0
df.main$Pubs_Before_Total[is.na(df.main$Pubs_Before_Total)] <- 0


# TargetSpec
base <- 0.9 # default
load(file = paste('Github/TargetSpec/Repo_TargetSpec_', base, '.RData', sep = '')) # 'df'
df.main <- merge(df.main, df, 
                 by = "ID",
                 all.x = T)

## save to stata
df.main$Pub_Total <- df.main$Pub.Cum.Year + df.main$Pubs_Before_Total
foreign::write.dta(df.main, file = paste('Github/Output/Repo_Event_DF_Final_M1.dta', sep = ''))