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
                       xlabels ='',
                       y_var, 
                       ylabels = '',
                       clr_var, #variable by which bubbles are coloured
                       n, #variable for number of studies in the cell
                       wrap_width = '10',
                       bg_col = '#BFD5E3',
                       title,
                       subtitle,
                       x_title,
                       y_title,
                       clr_by_name,
                       palette = 'Set2'){

    #ensure input variables are numeric for offset to work
  if(is.numeric(x_var) == FALSE){
    xlabels <- stringr::str_to_sentence(unique(tolower(x_var)))
    x_var <- c(factor(x_var, order = TRUE))
  }
  if(is.numeric(y_var) == FALSE){
    ylabels <- str_to_sentence(unique(tolower(y_var)))
    y_var <- c(factor(y_var, order = TRUE))
  }
  
  #remove NAs from dataframe
  #data <- na.omit(data)
  
  #if axis label inputs are blank, use numerical values instead
  if(paste(xlabels, collapse = '') == ''){
    xlabels <- as.character(1:length(unique(x_var)))
  }
  if(paste(ylabels, collapse = '') == ''){
    ylabels <- as.character(1:length(unique(y_var)))
  }
  
  #wrap axis labels
  xlabels <- stringr::str_wrap(xlabels, width = wrap_width)
  ylabels <- stringr::str_wrap(ylabels, width = wrap_width)
  
  #create offset table
  mtrxn <- length(unique(clr_var)) #number of bubbles needed
  #matrix positions (max n = 5)
  pos0 <- c(offset_x = 0, offset_y = 0) 
  pos1 <- c(offset_x = -0.2, offset_y = 0.2)
  pos2 <- c(offset_x = 0.2, offset_y = 0.2)
  pos3 <- c(offset_x = 0.2, offset_y = -0.2)
  pos4 <- c(offset_x = -0.2, offset_y = -0.2)
  #offset positions based on number of bubbles needed
  n1 <- cbind(n = 1, rbind(pos0)) 
  n2 <- cbind(n = 2, rbind(pos1, pos2))
  n3 <- cbind(n = 3, rbind(pos1, pos2, pos3))
  n4 <- cbind(n = 4, rbind(pos1, pos2, pos3, pos4))
  n5 <- cbind(n = 5, rbind(pos0, pos1, pos2, pos3, pos4))
  mtrxlookup <- as.data.frame(rbind(n1,n2,n3,n4,n5))
  clr_by_name <- c(unique(clr_var))
  offset_table <- cbind(clr_by_name, filter(mtrxlookup, n == mtrxn)[,2:3])
  data <- merge(data, offset_table, all.x = TRUE, by.x = clr_by,  by.y = 'clr_by_name')
  
  #create plot
  x <- data %>%
    ggplot(mapping = aes (x=(x_var + offset_x), 
                          y=(y_var + offset_y)#,
                          #text = sprintf('<a href="mailto:neal_haddaway@hotmail.com">Email</a>', outcome, inst)
                          )) +
    geom_hline(yintercept = (2:length(unique(y_var))-0.5), 
               colour = 'white', 
               size = .7, 
               alpha=0.8) +
    geom_vline(xintercept = (2:length(unique(x_var))-0.5), 
               colour = 'white', 
               size = .7, 
               alpha=0.8) +
    geom_point(aes(colour = clr_var, 
                   size = n)) +
    scale_size(range = c(.1, 6), 
               name = 'Number of studies') + 
    scale_color_brewer(palette = palette,
                       name = clr_by_name,
                       guide = FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = bg_col,
                                          size = 2, 
                                          linetype = "solid"),
          axis.text.x = element_text(size = 10, 
                                     angle = 45,
                                     hjust=0.95,
                                     vjust=0.95), 
          axis.text.y = element_text(size = 10, 
                                     angle = 45,
                                     hjust=0.8,
                                     vjust=0.95)) +
    labs(title = title, 
         subtitle = subtitle, 
         x = x_title, 
         y = y_title) +
    scale_y_continuous(labels = ylabels, 
                       breaks = 1:length(ylabels), 
                       limits = c(0.8,(length(ylabels)+0.2))) +
    scale_x_continuous(labels = xlabels, 
                       breaks = 1:length(xlabels), 
                       limits = c(0.8,(length(xlabels)+0.2))) + 
    guides(
           size = guide_legend(override.aes = list(colour = '#444444')))
  y <- ggplotly(x)
  layout(y, hovermode = 'closest')
}

attach(data4plot)
bubbleplot(data = data4plot, 
         x_var = inst, 
         y_var = outcome, 
         clr_var = data4plot$loc,
         n = n,
         bg_col = '#F1EFF3',
         title = 'title',
         subtitle = 'subtitle',
         x_title = 'Institution',
         y_title = 'Outcome',
         clr_by = 'loc',
         palette = 'Set2')
