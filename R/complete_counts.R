#' Fill missing x-axis values with zero.
#'
#' @param raw_data Data frame
#' @param outname If export = TRUE, output the resulting .
#' @param xaxis Name of the column with X-axis values
#' @param yaxis Name of the column with Y-axis values
#' @import dplyr
#' @import tidyr
#' @import lazyeval
#' @return Data frame with added rows
#' @export
#' @examples
#' complete_counts(test_data,xaxis='FractionID',yaxis='PeptideCount')

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(lazyeval))

complete_counts <- function(raw_data,xaxis,yaxis){

    data_spread <- raw_data %>% spread_(xaxis, yaxis)
    post_data <- data_spread %>% gather_(xaxis, yaxis, names(.)[!names(.)%in%colnames(raw_data)[!colnames(raw_data)%in%c(xaxis,yaxis)]])

    post_data[is.na(post_data)] <- 0
    post_data<-unique(post_data)


    if("Length"%in%colnames(post_data) ){             #Don't want proteins that are too long :(
        post_data <- post_data %>% filter(Length < 1500)
    }

    return(post_data)
}
