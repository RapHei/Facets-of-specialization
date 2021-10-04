# Figure 4: Plot effects of (selected) targeted specializations 

setwd('C:/Users/ac135138/Documents/Artikel/Global Research Trends/Article')
  

# Functions
## Assign topic labels
labels <- read.csv(file = 'Github/Data/Topic_description_final.csv', sep = ';')

labeller <- function(terms, labels){
  target <- stringr::str_extract(terms, "T\\d?.")
  target <- as.numeric(gsub('T', '', target))
  term.lables <- labels[match( target, labels$Topics),]$New.Label
  # add "Txx." before label
  term.lables <- paste("T", target, ".", term.lables, sep = '')
  term.out <- c(as.character(terms[is.na(target)]), term.lables[!is.na(target)])
  return(term.out)
}


## Clean stata output
clean.Stata <- function(df, type){
  # identify "" and "." as NA
  df <- df %>% mutate_all(na_if,"")
  df <- df %>% mutate_all(na_if,".")
  # drop all rows with NA
  df <- df[complete.cases(df), ]
  if(type == 'SE'){
    names(df) <- c('term', 'estimate', 'std.error')
  }
  if(type == 'p.value'){
    names(df) <- c('term', 'estimate', 'p.value')
  }
  # omit vars
  df <- df[!(df$term %in% c('lntheta', 'ln_p')),]
  # labels
  df$term <- labeller(df$term, labels)
  return(df)
}

### load results from stata
load.stata <- function(file.name){
  soc <- read.csv(file = paste('Github/Output/Robustness/SE_', file.name, '.csv', sep = ''), header = FALSE)
  soc <- clean.Stata(soc, type = 'SE')
  soc.pval <- read.csv(file = paste('Github/Output/Robustness/Pval_', file.name, '.csv', sep = ''), header = FALSE)
  soc.pval <- clean.Stata(soc.pval, type = 'p.value')
  soc$p.val <- soc.pval$p.value
  # freaking factors
  for(i in 2:4){
    soc[,i] <- as.numeric(as.character(soc[,i]))
  }
  soc$model <- file.name
  return(soc)
}
## Single
file.names <- c('Single_Base075', 'Single_Base08', 'Single_Base085','Single_Base09') # 'Single'

for(file.name in file.names){
  temp <- load.stata(file.name)
  if(file.name == file.names[1]){
    df <- temp
  }
  else{
    df <- rbind(df, temp)
  }
}
# clean var names
df$model <- plyr::mapvalues(df$model, from = unique(df$model), to = paste('Threshold_', c("0.75", "0.80", "0.85", "0.90"), sep = ''))


### Consider only topics that are significant across all thresholds (inner join) 

for(thres in unique(df$model)){
  df.plot <- df[grep("T\\d", df$term),] # only topics
  df.plot <- df.plot[df.plot$model == thres,]
  df.plot <- df.plot[df.plot$p.val < 0.001,]
  df.terms <- data.frame(Model = thres, 
                         Terms = df.plot$term)
  if(thres == unique(df$model)[1]){
    df.terms.all <- df.terms
  }
  else{
    df.terms.all <- rbind(df.terms.all, df.terms)
  }
}

# Intersection of models
A <- df.terms.all[df.terms.all$Model == 'Threshold_0.75', 'Terms']
B <- df.terms.all[df.terms.all$Model == 'Threshold_0.80', 'Terms']
C <- df.terms.all[df.terms.all$Model == 'Threshold_0.85', 'Terms']
D <- df.terms.all[df.terms.all$Model == 'Threshold_0.90', 'Terms']

topics.inner <- (Reduce(intersect, list(A, B, C, D)))

df.plot <- df[df$term %in% (Reduce(intersect, list(A, B, C, D))),]

# plot
px <- dwplot(df.plot,
             conf.level = .83) +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  labs(x = 'Hazard Rate') +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("cyan","deepskyblue","blue","darkblue"))

pdf('Github/Output/Figures/Figure4_TargetedSpec.pdf')
px
dev.off()