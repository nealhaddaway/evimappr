# Load packages

if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(plotly)){install.packages('plotly')}
library(plotly)
if(!require(RCurl)){install.packages('RCurl')}
library(RCurl)
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)
if(!require(bubblyr)){install.packages('bubblyr')}
library(bubblyr)
if(!require(dplyr)){install.packages('dplyr')}
library(dplyr)

#call in file from github
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/Cuan-crypto/EGM_RCT/main/EDRCT.csv")
EGM <- read.csv(text = x)

#summarise across outcome and inst
library(magrittr)
plotdat <- EGM %>% 
  count(outcome, inst, loc)
locations <- sort(unique(plotdat$loc))
africa <- subset(plotdat, loc == locations[1])
asia <- subset(plotdat, loc == locations[2])
austnz <- subset(plotdat, loc == locations[3])
csam <- subset(plotdat, loc == locations[4])
mult <- subset(plotdat, loc == locations[5])
rest <- subset(plotdat, loc == locations[6])
ukire <- subset(plotdat, loc == locations[7])
usca <- subset(plotdat, loc == locations[8])
none <- subset(plotdat, loc == locations[9])
plotdat2 <- rbind(rest, usca)
plotdat2$loc

#create offset table for bubble position
xpos <- c(0.2, -0.2)
ypos <- c(0.2, -0.2)
loc <- c('Rest of Europe', 'USA/Canada')
offset_table <- data.frame(loc = loc, offset_x = xpos, offset_y = ypos)

#apply offset values to data for the plot
data4plot <- plotdat2 %>%
  left_join(by = c("loc" = "loc"), y = offset_table)

data4plot$outcome <- as.factor(data4plot$outcome)
levels(data4plot$outcome)[levels(data4plot$outcome) == "Behaviour and Social Wellbeing"] <- "1"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Literacy/English"] <- "2"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Numeracy/Maths"] <- "3"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Other School Subjects"] <- "4"
levels(data4plot$outcome)[levels(data4plot$outcome) == "physical Health and Wellbeing"] <- "5"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Physical Health and Wellbeing"] <- "5"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Professional Training"] <- "6"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Range of Academic Outcomes"] <- "7"
levels(data4plot$outcome)[levels(data4plot$outcome) == "Study-Related Skills"] <- "8"
data4plot$outcome <- as.numeric(data4plot$outcome)

data4plot$inst <- as.factor(data4plot$inst)
levels(data4plot$inst)[levels(data4plot$inst) == "College/University"] <- "1"
levels(data4plot$inst)[levels(data4plot$inst) == "Middle/High School"] <- "2"
levels(data4plot$inst)[levels(data4plot$inst) == "Multiple"] <- "3"
levels(data4plot$inst)[levels(data4plot$inst) == "Preschool/Kindergarten"] <- "4"
levels(data4plot$inst)[levels(data4plot$inst) == "Primary/Elementary"] <- "5"
levels(data4plot$inst)[levels(data4plot$inst) == "Special School"] <- "6"
data4plot$inst <- as.numeric(data4plot$inst)

#remove NA
data4plot <- na.omit(data4plot)


#make static
library(hrbrthemes)
library(ggBubbles)

ylabels <- c("Behaviour and Social Wellbeing", 
             "Literacy/English", 
             "Numeracy/Maths",
             "Other School Subjects",
             "Physical Health and Wellbeing",
             "Professional Training",
             "Range of Academic Outcomes",
             "Study-Related Skills")
ylabels <- stringr::str_wrap(ylabels, width = 20)
xlabels <- c("College/University",
             "Middle/High School",
             "Multiple",
             "Preschool/Kindergarten", 
             "Primary/Elementary", 
             "Special School")
xlabels <- stringr::str_wrap(xlabels, width = 20)



#as function
#' @param data A dataframe consisting of pairwise x- and y- axis variables 
#' (x_var and y_var, respectively) grouped by a third variable, by which 
#' bubbles are coloured. Bubble size is indicated by a fourth variable 'n'.
bubbleplot <- function(data, 
                       x_var, 
                       y_var, 
                       clr_var, #variable by which bubbles are coloured
                       n, #variable for number of studies in the cell
                       offset_x, 
                       offset_y,
                       bg_col = '#BFD5E3',
                       title,
                       subtitle,
                       x_title,
                       y_title){
  x <- data %>%
    ggplot(mapping = aes (x=(x_var + offset_x), 
                          y=(y_var + offset_y))) +
    geom_hline(yintercept = 1:length(unique(y_var)), 
               colour = 'white', 
               size = 16, 
               alpha=0.2) +
    geom_vline(xintercept = 1:length(unique(x_var)), 
               colour = 'white', 
               size = 16, 
               alpha=0.2) +
    scale_size(range = c(.1, 6), 
               name="Number of studies") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = bg_col,
                                          size = 2, 
                                          linetype = "solid"),
          axis.text.x = element_text(size = 10, 
                                     angle = 45), 
          axis.text.y = element_text(size = 10, 
                                     angle = 45)) +
    geom_point(mapping = aes(colour = clr_var, 
                             size = n)) +
    labs(title = title, 
         subtitle = subtitle, 
         x = x_title, 
         y = y_title) +
    scale_y_continuous(labels = ylabels, 
                       breaks = 1:8, 
                       limits = c(1,8)) +
    scale_x_continuous(labels = xlabels, 
                       breaks = 1:6, 
                       limits = c(1,6))
  ggplotly(x)
}

attach(data4plot)
bubbleplot(data = data4plot, 
         x_var = inst, 
         y_var = outcome, 
         clr_var = data4plot$loc,
         n = n,
         offset_x = offset_x, 
         offset_y = offset_y,
         bg_col = '#BFD5E3',
         title = 'title',
         subtitle = 'subtitle',
         x_title = 'x',
         y_title = 'y')
