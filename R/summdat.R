#' Summarise dataframe by counting number of rows
#' 
#' @description Counts the number of rows across up to three variables of interest.
#' @param data Dataframe containing individuals rows corresponding to studies in a 
#' systematic map, for example.
#' @param col1 String corresponding to the column name of one of up to three 
#' factors (categorical variables) for which counts will be made of the number of 
#' rows corresponding to each level of the factor. 
#' @param col2 String corresponding to the column name of one of up to three 
#' factors (categorical variables) for which counts will be made of the number of 
#' rows corresponding to each level of the factor. 
#' @param col3 String corresponding to the column name of one of up to three 
#' factors (categorical variables) for which counts will be made of the number of 
#' rows corresponding to each level of the factor. 
#' @return A dataframe containing the summary counts of the number of rows across 
#' each level of each of up to three variables.
#' @importFrom magrittr "%>%"
#' @examples
#' col1 <- 'outcome'
#' col2 <- 'inst'
#' col3 <- 'loc'
#' text <- RCurl::getURL("https://raw.githubusercontent.com/Cuan-crypto/EGM_RCT/main/EDRCT.csv")
#' data <- read.csv(text = text)
#' summary <- summdat(data, col1, col2, col3)
#' summary;
#' @export 
summdat <- function(data, 
                    col1,
                    col2,
                    col3){
  
  data <- data %>% 
    dplyr::count(get(col1), get(col2), get(col3)) %>%
    na.omit()
  names(data)[1] <- col1
  names(data)[2] <- col2
  names(data)[3] <- col3
  
  return(data)
}
