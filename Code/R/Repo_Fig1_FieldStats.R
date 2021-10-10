# Figure 1: Demographic Developments in U.S. Sociology.  

## Figure 1 rests on the following, publicly available, data:  
### [ethnicity](https://www.asanet.org/academic-professional-resources/data-about-disipline/data-dashboard/degrees-awarded/doctorates-awarded-sociology-race-or-ethnicity) 
### [gender](https://www.asanet.org/academic-professional-resources/data-about-disipline/data-dashboard/degrees-awarded/doctorates-awarded-sociology-gender)
### [NSF Survey of Earned Doctorates](https://ncsesdata.nsf.gov/home/)

setwd('C:/Users/ac135138/Documents/GitHub/Facets-of-specialization/Data/Figure 1')

require(ggplot2)
require(reshape2)

# function
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
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


# Part 1: Gender & Ethnicity
## Gender
df.gender <- read.csv('SED_Sociology_Gender_1966-2018.csv', sep = ',', header = T)
df.gender$X <- df.gender$Male + df.gender$Female
df.gender$Female <- df.gender$Female / df.gender$X 
df1 <- melt(df.gender[, c('Year', 'Female')], id = c('Year'))

## Race
df.race <- read.csv('SED_Sociology_Ethnicity_LikeASA.csv', sep = ';', header = T)
df.race$Minorities <- 1 - df.race$White / 100
df2 <- melt(df.race[, c('Year', 'Minorities')], id = c('Year'))

df <- rbind(df1, df2)
df$clust <- 'Gender & Race'

p1 <- ggplot(df, aes(x=Year, y=value, group = variable)) +
  geom_line(aes(linetype=variable, color = variable))+
  geom_point(size = 0.7) +
  labs(x = 'Year', y = 'Share of sociology doctorates') +
  facet_grid(as.factor(clust)~. ) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_blank(), legend.position="bottom",
        panel.background = element_blank()) +
  scale_linetype_manual(values=c("dotted", 'longdash')) +
  scale_colour_manual(values=c('black', "grey60")) +
  scale_y_continuous(limits=c(0, 0.7), breaks=seq(0,0.7,0.1)) +
  scale_x_continuous(limits=c(1965, max(df$Year)), breaks=seq(1965, 2015, 5)) # use 5-year ticks


## Number of students
df.recip <- read.csv('SED_Sociology_1966-2018.csv', sep = ',', header = T)
df.recip$X <- NULL
df3 <- melt(df.recip, id = 'Year')
df3$clust <- 'Number of doctorates'


p2 <- ggplot(df3, aes(x=Year, y=value, group = variable)) +
  geom_line(linetype='solid', color = 'black', alpha = 0.8)+
  geom_point(size = 0.7) +
  labs(x = '', y = 'Number of sociology doctorates') +
  facet_grid(as.factor(clust)~. ) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_blank(), legend.position="bottom",
        panel.background = element_blank()) +
  scale_x_continuous(limits=c(1965, max(df$Year)), breaks=seq(1965, 2015, 5)) # use 5-year ticks


multiplot(p2, p1, cols = 1)
