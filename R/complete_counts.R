#' Fill missing x-axis values with zero. Very similar to tidyr::complete.
#'
#' @param raw_data Tidy dataframe with columns for x axis, y axis, and observations.
#' @param xaxis Name of the column with X-axis values.
#' @param yaxis Name of the column with Y-axis values.
#' @importFrom dplyr filter
#' @importFrom tidyr spread_ gather_
#' @importFrom purrr %>%
#' @import lazyeval
#' @return Data frame with added rows for missing x axis values
#' @examples
#' complete_counts(test_data,xaxis='FractionID',yaxis='PeptideCount')
#' @export
complete_counts <- function(raw_data,xaxis,yaxis){

    #spread and gather leaving every other column than xaxis and yaxis intact.
    data_spread <- raw_data %>% tidyr::spread_(xaxis, yaxis)
    post_data <- data_spread %>% tidyr::gather_(xaxis, yaxis, names(.)[!names(.)%in%colnames(raw_data)[!colnames(raw_data)%in%c(xaxis,yaxis)]])
    #fill missing combinations in the data with 0.
    post_data[is.na(post_data)] <- 0
    #this should be redundant.
    post_data<-unique(post_data)

    #Only filter long proteins if there is a length column
    #if("Length"%in%colnames(post_data) ){             #Don't want proteins that are too long :(
    #    post_data <- post_data %>% dplyr::filter(Length < 1500)
    #}

    return(post_data)
}
