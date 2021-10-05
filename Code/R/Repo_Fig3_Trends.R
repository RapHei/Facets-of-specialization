## Prob hier:

# setwd('C:/Users/Heiberger/Documents/Artikel/Global Research Trends/STM')
setwd('WoS/Github')

library(ggplot2)
library(ggrepel)
library(reshape2)
library(stm)
library(RColorBrewer)


### stm.effect ("Data/stm60effect_onlyYR.rda") # stm55effect) produces different figure than if we run it with "Data/PRO_model_60_superset_onlyYR.RData"
# Try with "old version" from FindK-folder ("STM/Data/Paperversion) -> ganz andere resultate, egal ob mit outsider oder ohne
# k <- 60
# load(file = paste("Data/PRO_model_",as.character(k),"_superset_FindK.RData", sep = ""))
# stm.effect.FindK <- stm::estimateEffect(formula= 1:60 ~ s(Year) + outsider, stmobj=model, metadata=out$meta)
# save(stm.effect.FindK, file="Data/Repo_stm_FindK.effect.rda")


# load(file = paste("Data/PRO_model_60_superset_onlyYR.RData", sep = ""))
# meta <- data.frame(Year = out$meta$Year)
# stm.effect <- stm::estimateEffect(formula= 1:60 ~ s(Year), stmobj=model, metadata=meta) #  uncertainty = 'None'; default == "global"
# 
# save(stm.effect, file="Data/Repo_stm.effect.rda")


# load("Data/stm60effect_onlyYR.rda") # stm55effect
load("Data/Repo_stm.effect.rda") # stm.effect
# load(file="Data/Repo_stm_FindK.effect.rda") # stm.effect.FindK

k_topics <- c(1:60)[-47]

# # manipulate object so it fits
# stm.effect.new <- stm55effect
# stm.effect.new$call <- NULL
# stm.effect.new$formula <- formula(~s(Year))
# stm.effect.new$data$outsider <- NULL
# stm.effect.new$modelframe$outsider <- NULL
# stm.effect.new$varlist <- "Year"


### CURVES
p <- plot(stm.effect,
          covariate="Year",
          model=model,
          method="continuous",
          xlab="Publication Year",
          ylab="Expected Topic Proportions",
          main="Topic popularity",
          printlegend=F)

curve.df <- data.frame(Topics = k_topics) 
curve.df[,as.character(round(p$x,3))] <- NA
for(t in k_topics){ 
  curve.df[curve.df$Topics == t,
           as.character(round(p$x,3))] <- p$means[[t]]
}


###
# to LONG FORMAT
###
curve.df.long <- melt(curve.df, id.vars = c("Topics"))
curve.df.long$Year <-  as.numeric(as.character(curve.df.long$variable))
curve.df.long$Topics <- paste("T", curve.df.long$Topics, sep = '')
curve.df.long$Topics <-  as.factor(curve.df.long$Topics)


####
# CREATE CHANGE TRAJECTORY
####
traj <- data.frame(Topics = unique(curve.df.long$Topics), Slope = NA)

for(top in traj$Topics){
  test <- curve.df.long[curve.df.long$Topics ==top,]
  l <- lm(test$value ~ test$Year)
  traj$Slope[traj$Topics == top] <- l$coefficients[2]
}

# get most rising/falling topics
traj <- traj[order(-traj$Slope),]

traj$clustname <- "Medium"
traj$clustname[1:20] <- "Trending"
traj$clustname[40:59] <- "Declining"

# order
traj$order <- "Middle"
traj$order[1:10] <- "1-10"
traj$order[11:20] <- "11-20"
traj$order[50:59] <- "1-10"
traj$order[40:49] <- "11-20"

curve.df.long <- merge(curve.df.long, traj[, c("Topics", "clustname", "order")])


#################
#### as LINE GRAPHS
#################
### 
# ADD LABELS (doesn't work for all for SOME reason..)
###
labels <- read.csv(file = 'Data/Topic_description_final.csv', sep = ';')
labels <- labels[-47,]
labels <- labels[,c("Topics", "New.Label")]
labels$Topics <- as.factor(paste("T", labels$Topics, sep = ''))
labels$New.Label <- as.factor(labels$New.Label)



####
# ONlY 10
####

trend <- 'Trending' #   ''

curve.sub <- subset(curve.df.long, (clustname == trend) & (order == '1-10'))
curve.sub <- merge(curve.sub, labels)
# merge produces error for all (only 58 levels..dunno); length(unique(curve.sub$New.Label))

# Merge Topics and Labels
curve.sub$New.Label <- paste(curve.sub$Topics, curve.sub$New.Label, sep = '.')


## REORDER
curve.sub$Topics <- as.character(curve.sub$Topics)
curve.sub <- curve.sub[gtools::mixedorder(curve.sub$Topics),]
curve.sub$Topics <- factor(curve.sub$Topics, levels = unique(curve.sub$Topics))

# colors
colourCount = length(unique(curve.sub$Topics))
getPalette = colorRampPalette(brewer.pal(10, "Spectral"))
colours = getPalette(colourCount)


# PLOT
ggplot(curve.sub, 
             aes(x = Year, y = value, group = Topics, colour = Topics))  + 
  geom_line(linetype = "solid") +
  # geom_point(alpha = 0.6) + 
  # scale_colour_brewer(palette = "Paired") +
  scale_colour_manual(values = colours, labels = unique(curve.sub$New.Label)) +  # allows >12 lines
  # geom_line(data = curve.sub %>%
  #             group_by(clustname, Year) %>%
  #             summarise(value = mean(value)),
  #           aes(x = Year, y = value, group = NULL),
  #           color = "black", size = 2, linetype = "solid", alpha = 0.5) +
  geom_text_repel(data = subset(curve.sub, Year == 2015), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  geom_text_repel(data = subset(curve.sub, Year == 1980), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
  labs(y = 'Topic Prevalence', title = trend)  



## DECLINGIN
trend <- 'Declining' #   ''

curve.sub <- subset(curve.df.long, (clustname == trend) & (order == '1-10'))
curve.sub <- merge(curve.sub, labels)
# merge produces error for all (only 58 levels..dunno); length(unique(curve.sub$New.Label))

# Merge Topics and Labels
curve.sub$New.Label <- paste(curve.sub$Topics, curve.sub$New.Label, sep = '.')


## REORDER
curve.sub$Topics <- as.character(curve.sub$Topics)
curve.sub <- curve.sub[mixedorder(curve.sub$Topics),]
curve.sub$Topics <- factor(curve.sub$Topics, levels = unique(curve.sub$Topics))

# colors
colourCount = length(unique(curve.sub$Topics))
getPalette = colorRampPalette(brewer.pal(10, "Spectral"))
colours = getPalette(colourCount)


# PLOT
ggplot(curve.sub, 
             aes(x = Year, y = value, group = Topics, colour = Topics))  + 
  geom_line(linetype = "solid") +
  # geom_point(alpha = 0.6) + 
  # scale_colour_brewer(palette = "Paired") +
  scale_colour_manual(values = colours, labels = unique(curve.sub$New.Label)) +  # allows >12 lines
  # geom_line(data = curve.sub %>%
  #             group_by(clustname, Year) %>%
  #             summarise(value = mean(value)),
  #           aes(x = Year, y = value, group = NULL),
  #           color = "black", size = 2, linetype = "solid", alpha = 0.5) +
  geom_text_repel(data = subset(curve.sub, Year == 2015), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  geom_text_repel(data = subset(curve.sub, Year == 1980), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
  labs(y = 'Topic Prevalence', title = trend) 



## BOTH

pdf('Analysis/Figures/Curves_LINES_Combined_10.pdf', paper= "USr")
multiplot(p1,p2, cols = 1)
dev.off()


###################################################
###################################################

####
# use ALL, by CLUSTERS
####

# curve.df.long$trends <- curve.df.long$clustname

## build clusters (cf. Topic_description_final_NewClusts.csv)

curve.df.long$clustname[curve.df.long$Topics %in% c('T1', 'T2', 'T3', 'T4', 'T16', 'T21', 'T25','T30', 'T37', 'T53', 'T56', 'T58')] <- 'Culture'
curve.df.long$clustname[curve.df.long$Topics %in% c('T6','T33', 'T34')] <- 'Deviance'
curve.df.long$clustname[curve.df.long$Topics %in% c('T7', 'T13', 'T19', 'T40', 'T42')] <- 'Education'
curve.df.long$clustname[curve.df.long$Topics %in% c('T8','T18','T28','T29', 'T35', 'T46', 'T48', 'T49', 'T54')] <- 'Family'
curve.df.long$clustname[curve.df.long$Topics %in% c('T9','T24','T52','T55', 'T59')] <- 'Public Health'
curve.df.long$clustname[curve.df.long$Topics %in% c('T5','T11','T14','T22', 'T23', 'T36', 'T39', 'T45')] <- 'Social Psychology'
curve.df.long$clustname[curve.df.long$Topics %in% c('T10','T20','T31', 'T43', 'T51')] <- 'Stratification'
## assign "social change" to rest
curve.df.long$clustname[is.na(curve.df.long$clustname)] <- 'Social Change'

# curve.sub <- curve.df.long[curve.df.long$clustname %in% c('Cultural Turn', 'Deviance', 'Education', 'Family', 'Public Health'),]


# Merge Topics and Labels
labels <- read.csv(file = 'Analysis/Regression/Topic_description_final.csv', sep = ';')
labels <- labels[-47,]
labels <- labels[,c("Topics", "New.Label")]
labels$Topics <- as.factor(paste("T", labels$Topics, sep = ''))
labels$New.Label <- as.factor(labels$New.Label)

curve.df.long <- merge(curve.df.long, labels, by = 'Topics')

curve.df.long$New.Label <- paste(curve.df.long$Topics, curve.df.long$New.Label, sep = '.')

## REORDER
curve.df.long$Topics <- as.character(curve.df.long$Topics)
curve.df.long <- curve.df.long[mixedorder(curve.df.long$Topics),]
curve.df.long$Topics <- factor(curve.df.long$Topics, levels = unique(curve.df.long$Topics))


colourCount = length(unique(curve.df.long$Topics))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colours = getPalette(colourCount)

## on two pages

first <- c('Culture', 'Deviance', 'Education', 'Family')
second <- c('Public Health', 'Social Change', 'Social Psychology', 'Stratification')

curve.df.temp <- curve.df.long[curve.df.long$clustname %in% second,]

## NO Labels
# p1 <- ggplot(curve.df.temp, 
#              aes(x = Year, y = value, group = Topics, colour = Topics))  + 
#   geom_line(linetype = "solid") +
#   # geom_point(alpha = 0.6, size = 0.2) +
#   # scale_colour_brewer(palette = "Paired") +
#   scale_colour_manual(values = colours) +  # allows >12 lines
#   # geom_line(data = curve.sub %>%
#   #             group_by(clustname, Year) %>%
#   #             summarise(value = mean(value)),
#   #           aes(x = Year, y = value, group = NULL),
#   #           color = "black", size = 2, linetype = "solid", alpha = 0.5) +
#   geom_text_repel(data = subset(curve.df.temp, Year == 2015), aes(label = Topics), hjust = 0.9, vjust = 0.4, point.padding = NA) +
#   theme_bw()+ 
#   theme(legend.position = 'empty', 
#         plot.margin=unit(c(0,0.5,0,0.5), "cm"))  +
#   labs(y = 'Topic Prevalence') +
#   facet_wrap(clustname ~., ncol = 2)
# 
# pdf('Analysis/Figures/Curves_LINES_second.pdf')
# p1
# dev.off()


# WITH labels
p1 <- ggplot(curve.df.temp, 
             aes(x = Year, y = value, group = Topics, colour = Topics))  + 
  geom_line(linetype = "solid") +
  scale_colour_manual(values = colours, labels = unique(curve.df.temp$New.Label)) +   # allows >12 lines
  geom_text_repel(data = subset(curve.df.temp, Year == 2015), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  # geom_text_repel(data = subset(curve.df.temp, Year == 1980), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  theme_bw()+ 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
  labs(y = 'Topic Prevalence') +
  facet_wrap(clustname ~., ncol = 2)


pdf('Analysis/Figures/Curves_LINES_second_withLabels.pdf', paper = 'US')
p1
dev.off()