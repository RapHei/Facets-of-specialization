# Facet of specialization: Figure 3 (Topic trends)

setwd('C:/Users/ac135138/Documents/GitHub/Facets-of-specialization')

library(ggplot2)
library(ggrepel)
library(reshape2)
library(stm)
library(RColorBrewer)


# Functions
## create trend lines from stm.effect, long.format
create.trends.long <- function(estimate.trends, k_topics){
  curve.df <- data.frame(Topics = k_topics) 
  curve.df[,as.character(round(estimate.trends$x,3))] <- NA
  for(t in k_topics){ 
    curve.df[curve.df$Topics == t,
             as.character(round(estimate.trends$x,3))] <- estimate.trends$means[[t]]
  }
  curve.df.long <- melt(curve.df, id.vars = c("Topics"))
  curve.df.long$Year <-  as.numeric(as.character(curve.df.long$variable))
  curve.df.long$Topics <- paste("T", curve.df.long$Topics, sep = '')
  curve.df.long$Topics <-  as.factor(curve.df.long$Topics)
  ### create change trajectory (cf. Griffith’s and Steyver’s, 2004)
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
  return(curve.df.long)
}

## select 10 highest rising/falling topics
top.trends <- function(curve.df.long, trend){
  curve.sub <- subset(curve.df.long, (clustname == trend) & (order == '1-10'))
  curve.sub <- merge(curve.sub, labels)
  curve.sub$Label <- paste(curve.sub$Topics, curve.sub$Label, sep = '.')
  ## reorder
  curve.sub$Topics <- as.character(curve.sub$Topics)
  curve.sub <- curve.sub[gtools::mixedorder(curve.sub$Topics),]
  curve.sub$Topics <- factor(curve.sub$Topics, levels = unique(curve.sub$Topics))
  return(curve.sub)
}

## multiplots
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


## Estimate topic trends
# load(file = paste("Data/PRO_model_60_superset_onlyYR.RData", sep = ""))
# meta <- data.frame(Year = out$meta$Year)
# stm.effect <- stm::estimateEffect(formula= 1:60 ~ s(Year), stmobj=model, metadata=meta) 
# save(stm.effect, file="Data/Repo_stm.effect.RData")
load("Data/Repo_stm.effect.RData") # stm.effect
k_topics <- c(1:60)[-47]

estimate.trends <- plot(stm.effect,
                        covariate="Year",
                        model=model,
                        method="continuous",
                        xlab="Publication Year",
                        ylab="Expected Topic Proportions",
                        main="Topic popularity",
                        printlegend=F)
curve.df.long <- create.trends.long(estimate.trends, k_topics)


## load labels
labels <- read.csv(file = 'Data/Topic_description_final.csv', sep = ';')
labels <- labels[-47,]
labels <- labels[,c("Topics", "Label")]
labels$Topics <- as.factor(paste("T", labels$Topics, sep = ''))
labels$Label <- as.factor(labels$Label)


# Top10 trending topics 
trend <- 'Trending' 
curve.sub <- top.trends(curve.df.long, trend)

# colors
colourCount = length(unique(curve.sub$Topics))
getPalette = colorRampPalette(brewer.pal(10, "Spectral"))
colours = getPalette(colourCount)

# plot
p1 <- ggplot(curve.sub, 
             aes(x = Year, y = value, group = Topics, colour = Topics))  + 
  geom_line(linetype = "solid") +
  scale_colour_manual(values = colours, labels = unique(curve.sub$Label)) +  # allows >12 lines
  geom_text_repel(data = subset(curve.sub, Year == 2015), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  geom_text_repel(data = subset(curve.sub, Year == 1980), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
  labs(y = 'Topic Prevalence', title = trend)  


# Top10 declining topics 
trend <- 'Declining'
curve.sub <- top.trends(curve.df.long, trend)

# colors
colourCount = length(unique(curve.sub$Topics))
getPalette = colorRampPalette(brewer.pal(10, "Spectral"))
colours = getPalette(colourCount)

# plot
p2<- ggplot(curve.sub, 
       aes(x = Year, y = value, group = Topics, colour = Topics))  + 
  geom_line(linetype = "solid") +
  scale_colour_manual(values = colours, labels = unique(curve.sub$Label)) +  # allows >12 lines
  geom_text_repel(data = subset(curve.sub, Year == 2015), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  geom_text_repel(data = subset(curve.sub, Year == 1980), aes(label = Topics), hjust = 0.7, show_guide = FALSE) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.margin=unit(c(0,0.5,0,0.5), "cm")) +
  labs(y = 'Topic Prevalence', title = trend)  

## depict both in one figure
pdf('Output/Figures/Fig3_Trends.pdf', paper= "USr")
multiplot(p1,p2, cols = 1)
dev.off()