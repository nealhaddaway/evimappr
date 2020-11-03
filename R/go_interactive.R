#' Interactive bubble and grid evidence map
#' 
#' @description Convert a static bubble or grid plot to an interactive, 
#' HTML version with embedded hyperlinks.
#' @param plot A ggplot2 object produced from bubble().
#' @param type The type of plot, either 'bubble' or 'grid' (heatmap). 
#' @param input1 If a bubble plot is provided, this should correspond to 
#' the dataframe containing the full plot data. If a grid or heatmap is 
#' provided, this should correspond to a vector of URLs, one for each 
#' plotted datapoint, listed in the same order as the data provided in 
#' the plotted dataframe.
#' @param input2 If a bubble plot is provided, this should correspond to 
#' the clr_var categorical variable used to colour bubbles. If a grid or 
#' heatmap is plotted, this should be left empty.
#' @return An interactive plot object.
#' @examples 
#' \donttest{
#' go_interactive(plot, type = 'grid', data$url)
#' go_interactive(plot, type = 'bubble', input1 = data, input2 = loc)
#' }
#' @export
go_interactive <- function (plot,
                            type,
                            input1,
                            input2){
  
  if (type == 'bubble'){
    data <- input1
    clr_var <- input2

    interact1 <- function(plot, data, clr_var) {
      ply <- plotly::ggplotly(plot)
      #the following code adds the location link for the new window - here specified to 'iframe2', but works in a new window if no iframe named
      javascript <- htmltools::HTML(paste("
                           var myPlot = document.getElementsByClassName('js-plotly-plot')[0];
                           myPlot.on('plotly_click', function(data){
                           var url = ", jsonlite::toJSON(split(data, clr_var)), ";
                           window.open(url[data.points[0].data.name][data.points[0].pointNumber]['url'],'iframe2');
                           });", sep=''))  
      htmlwidgets::prependContent(ply, htmlwidgets::onStaticRenderComplete(javascript))
    }

    x <- interact1(plot, data, clr_var)
    
  } else if (type == 'grid'){
    url <- input1

    
    interact2 <- function(plot, url) {
      ply <- plotly::ggplotly(plot)
      #the following code adds the location link for the new window - here specified to 'iframe2', but works in a new window if no iframe named
      javascript <- htmltools::HTML(paste("
                           var myPlot = document.getElementsByClassName('js-plotly-plot')[0];
                           myPlot.on('plotly_click', function(data){
                           var url = ['", paste(url, collapse = "', '"), "'];
                           window.open(url[data.points[0].pointNumber],'iframe2');
                           });", sep=''))  
      htmlwidgets::prependContent(ply, htmlwidgets::onStaticRenderComplete(javascript))
    }
    
    x <- interact2(plot, url)
    
  } else {
    
    stop("The provided plot must be of type 'bubble' or 'grid'")
    
  }

  return(x)
  
}
