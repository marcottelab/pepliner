#' Fill missing x-axis values with zero.
#'
#' @param raw_data Data frame
## @param outname If export = TRUE, output the resulting data frame named after this argument.
#' @param xaxis Name of the column with X-axis values.
#' @param yaxis Name of the column with Y-axis values.
#' @importFrom dplyr filter
#' @importFrom tidyr spread_ gather_
#' @importFrom purrr %>%
## @import lazyeval
#' @return Data frame with added rows
#' @export
#' @examples
#' test_data <- read.csv(paste0(system.file('extdata',package='pepliner'),'/msdata.csv'))
#' complete_counts(test_data,xaxis='FractionID',yaxis='PeptideCount')

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(lazyeval))

complete_counts <- function(raw_data,xaxis,yaxis){

    #spread and gather leaving every other column than xaxis and yaxis intact.
    data_spread <- raw_data %>% tidyr::spread_(xaxis, yaxis)
    post_data <- data_spread %>% tidyr::gather_(xaxis, yaxis, names(.)[!names(.)%in%colnames(raw_data)[!colnames(raw_data)%in%c(xaxis,yaxis)]])
    #fill missing combinations in the data with 0.
    post_data[is.na(post_data)] <- 0
    #this should be redundant.
    post_data<-unique(post_data)

    #Only filter long proteins if there is a length column
    if("Length"%in%colnames(post_data) ){             #Don't want proteins that are too long :(
        post_data <- post_data %>% dplyr::filter(Length < 1500)
    }

    return(post_data)
}
