# Facet of specialization: Consistency
setwd('WoS')

# Functions
## Cosine
cosine <- function(x, y){
  cos <- (x %*% y) / sqrt(x%*%x * y%*%y)
  return(cos)
}


# Load thetas (from theses and merged WoS articles)
load(file = 'Github/Data/Theta_WoS_Repo_AsIs.RData') # theta.wos
load('Github/Data/Theta_Repo.RData') # theta (from dissertations)

topics <- paste('X', 1:60, sep = '')

start.time <- Sys.time()
for(id in unique(theta.wos$ID)){
  # get students' WOS-articles
  wos <- theta.wos[theta.wos$ID == id,] 
  # get theta of their thesis
  thesis <- theta[theta$ID == id,]
  thesis.x <- as.numeric(thesis[,topics])
  # gather publications before graduation 
  pub.temp <- data.frame(ID = id, Pubs_Before_Total = nrow(wos[wos$Year <= thesis$Year,]))
  # keep only WOS-articles *after* graduation
  wos <- wos[wos$Year > thesis$Year,]
  if(nrow(wos) > 0){
    # compare thesis-vector to each article
    wos.x <- sapply(wos[, topics], as.numeric)
    if(nrow(wos) == 1){
      dist.v <- cosine(wos.x, thesis.x)
    }
    else{
      dist.v <- apply(wos.x, 1, function(x) cosine(x, thesis.x))
    }
    wos$Consistency <- dist.v 
    wos <- wos[, c('ID', 'Year', 'Consistency')]
  }
  if(id == unique(theta.wos$ID)[1]){
    df <- wos
    df.PubBefore <- pub.temp
  }
  else{
    df <- rbind(df, wos)
    df.PubBefore <- rbind(df.PubBefore,pub.temp)
  }
}
print(Sys.time() - start.time) # takes around 7 minutes
save(df,df.PubBefore, file = paste('Github/Consistency/Repo_Consistency_Raw.RData', sep = '')) # temporary storage

## Load
load(file = 'Github/Consistency/Repo_Identity_Raw.RData') #df, df.PubBefore


# Consistency/Pub_cum by year
temp.Ident.Yr <- aggregate(Consistency ~ Year + ID,
                           data = df,
                           FUN = mean)
names(temp.Ident.Yr)[3] <- 'Consistency'

temp.Pub.Yr <- aggregate(rep(1, nrow(df)) ~ Year + ID, 
                         data = df,
                         FUN = seq_along)
temp.Pub.Yr$Pub.Year <- unlist(lapply(temp.Pub.Yr[,3], max))
temp.Pub.Yr[,3] <- NULL

df.Ident.Yr <- merge(temp.Ident.Yr, temp.Pub.Yr, 
                     by = c('Year', 'ID'))

## order by Year & ID
df.Ident.Yr <- df.Ident.Yr[order(df.Ident.Yr$ID, df.Ident.Yr$Year),]
## cumulate publications
df.Ident.Yr$Pub.Cum.Year <- ave(df.Ident.Yr$Pub.Year, df.Ident.Yr$ID, FUN=cumsum)

## "Rolling mean"
ident.mean <- aggregate(df.Ident.Yr$Consistency ~ df.Ident.Yr$ID, FUN= function(x){cumsum(x) / seq_along(x)})
for(i in 1:nrow(ident.mean)){
  df.temp <- data.frame(Consistency.Mean = as.vector(ident.mean$`df.Ident.Yr$Consistency`[[i]]), ID = ident.mean$`df.Ident.Yr$ID`[i])
  if(df.temp$ID[1] == ident.mean$`df.Ident.Yr$ID`[1]){
    df <- df.temp
  }
  else{
    df <- rbind(df, df.temp)
  }
}
df$Pub.Year <- df.Ident.Yr$Pub.Year; df$Pub.Cum.Year <- df.Ident.Yr$Pub.Cum.Year; df$Year <- df.Ident.Yr$Year;

## Merge main.df to add events to publications (all rows of df)
df.main <- foreign::read.dta(file = 'Github/Data/Main_Repo.dta')
df.merge <- df.main[, c('Thesisyear', 'ID' , 'Advisor', 'Event_Time')]

df.temp <- merge(df, df.merge,
                 by = 'ID',
                 all.x = T)
df.temp$Pub.Event.Time <- df.temp$Year - df.temp$Thesisyear


## Event == 0
event.0 <- df.temp[df.temp$Advisor == 0,]
# just take max.Pubyear (latest publication), w/o considering events
event.0.MaxYear <- aggregate(Year ~ ID, data = event.0, FUN = max)
event.0 <- merge(event.0, event.0.MaxYear,
                 by = c('ID', 'Year'),
                 all.y = T)

## Event == 1
event.1 <- df.temp[df.temp$Advisor == 1,]
# use only publications before/at first event 
event.1 <- event.1[event.1$Event_Time >= event.1$Pub.Event.Time,]
# take latest publication before event (and so latest rolling mean of consistency)
event.1.temp <- aggregate(Pub.Event.Time ~ ID, data = event.1, FUN = max)
event.1.temp <- merge(event.1.temp, event.1, by = c('ID', 'Pub.Event.Time'), all.x = T)

## Bind together
consistency <- rbind(event.0, event.1.temp)
consistency <- consistency[, c('ID', 'Consistency.Mean', 'Pub.Cum.Year')]
## Merge publication_before
consistency <