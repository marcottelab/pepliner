<<<<<<< HEAD

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(cowplot))
# suppressPackageStartupMessages(library(lazyeval))


covplot_row <- function(row,elementid){
    # row1 = data[1:12,]
    # row2
    row$dummy_y <- 20

    rect <- ggplot(data = row, aes(x=max(row$Length), y = dummy_y)) +
        theme(axis.text = element_text(size=8)) +
        geom_blank() +
        theme(
            axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.line.y=element_blank(),
            axis.line.x=element_blank()) +
        geom_rect(aes(xmin=0, xmax=max(row$Length), ymin=0, ymax=20,fill=I('grey95'))) +
        geom_vline(xintercept=seq(0, max(row$Length), by=100),color='grey56') +
        geom_rect(aes(xmin=row$Start, xmax=row$End, ymin=0, ymax=20,fill=I('#f46d43'))) +
        geom_text(aes(label=as.character(head(select_(row,elementid)[,1],1))),size=4, position = position_nudge(x=-0.5*(max(row$Length)),y=-10))#as.character(head(row$Peptide,1)))

    rect <- ggplotGrob(rect)


    #remove unnecessary plot elements
    rect <- gtable_remove_grobs(rect, c('title', 'xlab-b', 'ylab-l', 'axis-b','axis-l','spacer'))
    #print(rect)
    #compress unused plot space
    rect <- gtable_squash_rows(rect, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    rect <- gtable_squash_cols(rect, c(1, 2, 3, 5, 6, 7))
    return(rect)

    #facet_grid(Peptide ~ .)
}
#' Make a coverage plot for all the peptides retrieved for a single protein.
#'
#' @param input_data Data frame to which the coverage columns will be appended.
#' @param groupid Data frame column (factor) corresponding to the names of the groups of elements in the data.
#' @param group_name (Optional) If specified, make plots from only those rows whose ID matched this argument.
#' @param elementid Data frame column (factor) corresponding to the elements for each of which a row of the final plot will be produced.
#' @param sort_column (Optional) If specified, rows in the final plot will be sorted according to this (numeric) column.
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @import cowplot
#' @import lazyeval
#' @return ggplot item to be plotted using cowplot::ggdraw().
## @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' 'ms_data.csv' %>% complete_counts() %>% covplot(groupid='ID',elementid='Peptide',sort_column='Start') %>% ggdraw

covplot <- function(input_data,group_name='',sort_column='Start',groupid='',elementid){


    uniqueness_test <- input_data %>% group_by(element_id, groupid) %>%
                       summarize(groupsize = n()) %>%
                       filter(groupsize > 1)

    if(nrow(uniqueness_test) > 0){
    print(paste("Warning, ", nrow(uniqueness_test), "elements are not unique to groups\nEx. peptides match multiple proteins", sep=""))
    }



    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,'Start','End','Length')]

    if(groupid!=''&group_name!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    }else{
        data_group <- pre_data
    }

    cov_data <- data_group
    print(head(cov_data))
    #Feed peptides into cov_row function
    cov_data = droplevels(cov_data)

    cov_data[,colnames(cov_data)==elementid] <- reorder(cov_data[,colnames(cov_data)==elementid],cov_data[,colnames(cov_data)==sort_column])
    cov_data %>% split(select_(cov_data,elementid)[,1]) %>% map(covplot_row,elementid=elementid) -> cov_list

    #Plot ggplot objects
    clusterplot <- plot_grid(plotlist = cov_list, ncol=1, align = "v")

}
=======

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(cowplot))
# suppressPackageStartupMessages(library(lazyeval))


covplot_row <- function(row,elementid){
    # row1 = data[1:12,]
    # row2
    row$dummy_y <- 20

    rect <- ggplot(data = row, aes(x=max(row$Length), y = dummy_y)) +
        theme(axis.text = element_text(size=8)) +
        geom_blank() +
        theme(
            axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.line.y=element_blank(),
            axis.line.x=element_blank()) +
        geom_rect(aes(xmin=0, xmax=max(row$Length), ymin=0, ymax=20,fill=I('grey95'))) +
        geom_vline(xintercept=seq(0, max(row$Length), by=100),color='grey56') +
        geom_rect(aes(xmin=row$Start, xmax=row$End, ymin=0, ymax=20,fill=I('#f46d43'))) +
        geom_text(aes(label=as.character(head(select_(row,elementid)[,1],1))),size=4, position = position_nudge(x=-0.5*(max(row$Length)),y=-10))#as.character(head(row$Peptide,1)))

    rect <- ggplotGrob(rect)


    #remove unnecessary plot elements
    rect <- gtable_remove_grobs(rect, c('title', 'xlab-b', 'ylab-l', 'axis-b','axis-l','spacer'))
    #print(rect)
    #compress unused plot space
    rect <- gtable_squash_rows(rect, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    rect <- gtable_squash_cols(rect, c(1, 2, 3, 5, 6, 7))
    return(rect)

    #facet_grid(Peptide ~ .)
}
#' Make a coverage plot for all the peptides retrieved for a single protein.
#'
#' @param input_data Data frame to which the coverage columns will be appended.
#' @param groupid Data frame column (factor) corresponding to the names of the groups of elements in the data.
#' @param group_name (Optional) If specified, make plots from only those rows whose ID matched this argument.
#' @param elementid Data frame column (factor) corresponding to the elements for each of which a row of the final plot will be produced.
#' @param sort_column (Optional) If specified, rows in the final plot will be sorted according to this (numeric) column.
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @import cowplot
#' @import lazyeval
#' @return ggplot item to be plotted using cowplot::ggdraw().
## @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' 'ms_data.csv' %>% complete_counts() %>% covplot(groupid='ID',elementid='Peptide',sort_column='Start') %>% ggdraw

covplot <- function(input_data,group_name='',sort_column='Start',groupid='',elementid){

    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,'Start','End','Length')]

    if(groupid!=''&group_name!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    }else{
        data_group <- pre_data
    }

    cov_data <- data_group

    #Feed peptides into cov_row function
    cov_data = droplevels(cov_data)

    cov_data[,colnames(cov_data)==elementid] <- reorder(cov_data[,colnames(cov_data)==elementid],cov_data[,colnames(cov_data)==sort_column])
    cov_data %>% split(select_(cov_data,elementid)[,1]) %>% map(covplot_row,elementid=elementid) -> cov_list

    #Plot ggplot objects
    clusterplot <- plot_grid(plotlist = cov_list, ncol=1, align = "v")

}
>>>>>>> parent of d56b055... Move optional arguments to end of function args
