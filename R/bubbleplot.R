#' Bubble evidence map plotting function
#' 
#' @description Plots a bubble evidence map for a systematic map, evidence 
#' gap map or similar.
#' @param x_var Variable to plot on the x-axis. Variable should be ordinal 
#' or categorical. Vectors of strings are converted to ordinal for plotting, 
#' with original levels shown on the plot.
#' @param xlabels Optional vector of strings used to plot x-axis labels if 
#' x_var is ordinal.
#' @param y_var Variable to plot on the y-axis. Variable should be ordinal 
#' or categorical. Vectors of strings are converted to ordinal for plotting, 
#' with original levels shown on the plot.
#' @param ylabels Optional vector of strings used to plot x-axis labels if 
#' y_var is ordinal.
#' @param clr_var Variable (vector) used to colour bubbles. Up to five 
#' levels of the vector may be plotted as different bubbles.
#' @param clr_legend Optional name of clr_var variable used to colour bubbles. 
#' This string is used to label the legend.
#' @param n Numerical variable corresponding to the number of studies and 
#' used to alter the size of the bubbles.
#' @param wrap_width The width of axis labels using character wrapping. 
#' The default is 'wrap_width = 10'.
#' @param bg_col The background colour for the plot area. The default is 
#' 'bg_col = "#BFD5E3"'.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param x_title X-axis title.
#' @param y_title Y-axis title.
#' @param palette Palette colour used to colour bubbles. The default is 
#' 'palette = "Set2"'.
#' @param interactive Logical argument (TRUE or FALSE) specifying whether the 
#' plot should be made interactive or not. Interactive plots include a 
#' hoverover tooltip containing additional text.
#' @param hovertext Optional string containing text to plot in the hoverover 
#' tooltip.
#' @return Plots an interactive bubble evidence map (using ggplotly)
#' @importFrom magrittr "%>%"
#' @examples 
#' \dontrun{
#' data <- read.csv2('inst/extdata/data.csv', sep=',')
#' attach(data)
#' plot <- bubbleplot(x_var = inst, 
#'     y_var = outcome, 
#'     clr_var = loc,
#'     clr_legend = 'Location',
#'     n = n,
#'     bg_col = '#F1EFF3',
#'     title = 'RCTs in Education',
#'     subtitle = 'Data from 1980 to 2017',
#'     x_title = 'Institution',
#'     y_title = 'Outcome',
#'     palette = 'Set2',
#'     interactive = FALSE,
#'     hovertext = 'test')
#' plot
#' }
#' @export
bubbleplot <- function(x_var, 
                       xlabels ='',
                       y_var, 
                       ylabels = '',
                       clr_var, #variable by which bubbles are coloured
                       clr_legend = '', #name for the colour legend
                       n, #variable for number of studies in the cell
                       wrap_width = 10,
                       bg_col = '#BFD5E3',
                       title = '',
                       subtitle = '',
                       x_title = '',
                       y_title = '',
                       palette = 'Set2',
                       interactive = FALSE,
                       hovertext = ''){
  
  #ensure input variables are ordinal (numeric) for offset to work, if not numeric, convert to ordinal
  if(is.numeric(x_var) == FALSE){
    x_var2 <- c(factor(x_var, 
                       ordered = TRUE))
    labdfx <- data.frame(lbl = unique(x_var), 
                         order = unique(x_var2))
    labdfx <- labdfx[order(labdfx$order),]
    xlabels <- stringr::str_to_sentence(tolower(labdfx$lbl)) #convert labels to sentence case
  }
  if(is.numeric(y_var) == FALSE){
    y_var2 <- c(factor(y_var, ordered = TRUE))
    labdfy <- data.frame(lbl = unique(y_var), 
                         order = unique(y_var2))
    labdfy <- labdfy[order(labdfy$order),]
    ylabels <- stringr::str_to_sentence(tolower(labdfy$lbl))
  }
  
  #if axis label inputs are blank, use numerical values instead
  if(paste(xlabels, 
           collapse = '') == ''){
    xlabels <- as.character(1:length(unique(x_var)))
  }
  if(paste(ylabels, 
           collapse = '') == ''){
    ylabels <- as.character(1:length(unique(y_var)))
  }
  
  #wrap axis labels
  xlabels <- stringr::str_wrap(xlabels, 
                               width = wrap_width)
  ylabels <- stringr::str_wrap(ylabels, 
                               width = wrap_width)
  
  #error checking
  #check that there are a maximum of five colour-by variables and return error if not
  if(length(unique(clr_var)) > 5) stop('Levels of clr_var >5. Cannot plot.')
  
  #create offset table
  mtrxn <- length(unique(clr_var)) #number of bubbles needed
  #matrix positions (max n = 5)
  pos0 <- c(offset_x = 0, 
            offset_y = 0) 
  pos1 <- c(offset_x = -0.2, 
            offset_y = 0.2)
  pos2 <- c(offset_x = 0.2, 
            offset_y = 0.2)
  pos3 <- c(offset_x = 0.2, 
            offset_y = -0.2)
  pos4 <- c(offset_x = -0.2, 
            offset_y = -0.2)
  #offset positions based on number of bubbles needed
  n1 <- cbind(n = 1,
              rbind(pos0)) 
  n2 <- cbind(n = 2, 
              rbind(pos1, 
                    pos2))
  n3 <- cbind(n = 3, 
              rbind(pos1,
                    pos2, 
                    pos3))
  n4 <- cbind(n = 4, 
              rbind(pos1, 
                    pos2, 
                    pos3, 
                    pos4))
  n5 <- cbind(n = 5, 
              rbind(pos0, 
                    pos1, 
                    pos2, 
                    pos3, 
                    pos4))
  mtrxlookup <- as.data.frame(rbind(n1, 
                                    n2, 
                                    n3, 
                                    n4, 
                                    n5))
  offset_table <- cbind(clr_by_name = c(unique(clr_var)), 
                        dplyr::filter(mtrxlookup, 
                                      n == mtrxn)[,2:3])
  data <- data.frame(clr_var, 
                     x_var2, 
                     x_var, 
                     y_var2, 
                     y_var, 
                     n)
  data <- merge(data, 
                offset_table, 
                all.x = TRUE, 
                by.x = 'clr_var', 
                by.y = 'clr_by_name')
  
  #supress bubble colour legend title if interactive
  if(interactive == TRUE){
    bub_legend = ''
  }
  
  #create static plot
  x <- data %>%
    ggplot2::ggplot(mapping = ggplot2::aes (x=(x_var2 + offset_x), #offset variables according to their matrix location
                                            y=(y_var2 + offset_y),
                                            label1 = x_var,
                                            label2 = y_var,
                                            label3 = clr_var,
                                            label4 = n,
                                            text = hovertext
                                   )) +
    ggplot2::geom_hline(yintercept = (2:length(unique(y_var2))-0.5), #add lines separating bubbles
                        colour = 'white', 
                        size = .7, 
                        alpha=0.8) +
    ggplot2::geom_vline(xintercept = (2:length(unique(x_var2))-0.5), #add lines separating bubbles
                        colour = 'white', 
                        size = .7, 
                        alpha=0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = clr_var, #add bubbles
                                     size = n)) +
    ggplot2::scale_color_brewer(palette = palette, #colour bubbles
                                #guide = FALSE,
                                name = clr_legend) +
    ggplot2::scale_size(range = c(.1, 6), #scale bubbles by size of n
                        #guide = FALSE,
                        name = bub_legend
                        ) + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), #remove grid lines
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = bg_col, #colour plot background
                                                            size = 2, 
                                                            linetype = "solid"),
                   axis.text.x = ggplot2::element_text(size = 8, #adjust angle of axis labels
                                                       angle = 45,
                                                       hjust=0.95,
                                                       vjust=0.95), 
                   axis.text.y = ggplot2::element_text(size = 8, 
                                                       angle = 45,
                                                       hjust=0.8,
                                                       vjust=0.95)) +
    ggplot2::labs(title = title, #specify title, subtitle, and axis titles
                  subtitle = subtitle, 
                  x = x_title, 
                  y = y_title) +
    ggplot2::scale_y_continuous(labels = ylabels, #add axis labels
                                breaks = 1:length(ylabels), 
                                limits = c(0.8,(length(ylabels)+0.2))) +
    ggplot2::scale_x_continuous(labels = xlabels, 
                                breaks = 1:length(xlabels), 
                                limits = c(0.8,(length(xlabels)+0.2)))
  
  if(interactive == TRUE){
    x <- plotly::ggplotly(x, 
                          tooltip = c('label1', 'label2', 'label3', 'label4', 'text')) #make interactive plot
    plotly::layout(x, #add hoverover labels and plot
                   hovermode = 'closest')
  }
  
  return(x)
}
