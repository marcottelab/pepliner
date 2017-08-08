
# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(cowplot))
# suppressPackageStartupMessages(library(lazyeval))


covplot_row <- function(row,elementid){

    # Dummy variable height of each rectangle. Not very important since plots are resized afterwards according to number of rows.
    row$dummy_y <- 20

    #auxiliary function to plot each row of the covplots
    #the x-value for each row is always the length of the protein
    rect <- ggplot2::ggplot(data = row, ggplot2::aes(x=max(row$Length), y = dummy_y)) +
        #get rid of the grey grid in the background set by default in ggplot2
        cowplot::theme_cowplot() +
        #set font size
        ggplot2::theme(axis.text = ggplot2::element_text(size=8)) +
        #start off each plot as a blank element
        ggplot2::geom_blank() +
        #get rid of all axes, titles, ticks and labels
        ggplot2::theme(
            axis.text.y=ggplot2::element_blank(),
            axis.text.x=ggplot2::element_blank(),
            axis.title.x=ggplot2::element_blank(),
            axis.title.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            axis.ticks.x=ggplot2::element_blank(),
            axis.line.y=ggplot2::element_blank(),
            axis.line.x=ggplot2::element_blank()) +
        #plot a grey rectangle that represents the full protein structure.
        ggplot2::geom_rect(ggplot2::aes(xmin=0, xmax=max(row$Length), ymin=0, ymax=20,fill=I('grey95'))) +
        #plot slightly darker vertical lines that represent intervals of 100 residues.
        ggplot2::geom_vline(xintercept=seq(0, max(row$Length), by=100),color='grey56') +
        #plot orange rectangles that represent the span of each peptide with respect to the full protein sequence, according to the Start and End columns.
        ggplot2::geom_rect(ggplot2::aes(xmin=row$Start, xmax=row$End, ymin=0, ymax=20,fill=I('#f46d43'))) +
        #Label each row according to the peptide being represented: plot a geom_text with the sequence of the peptide (head of the elementid column). Position them according to the length of the row.
        ggplot2::geom_text(ggplot2::aes(label=as.character(utils::head(dplyr::select_(row,elementid)[,1],1))),size=4, position = ggplot2::position_nudge(x=-0.5*(max(row$Length)),y=-10))#as.character(head(row$Peptide,1)))

    #generate a ggplot2 plot grob from the previous ggplot object
    rect <- ggplot2::ggplotGrob(rect)


    #remove unnecessary plot elements
    rect <- cowplot::gtable_remove_grobs(rect, c('title', 'xlab-b', 'ylab-l', 'axis-b','axis-l','spacer'))
    #print(rect)
    #compress unused plot space
    rect <- cowplot::gtable_squash_rows(rect, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    rect <- cowplot::gtable_squash_cols(rect, c(1, 2, 3, 5, 6, 7))
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
#' @importFrom dplyr select_ group_by_ summarize n
## @import tidyr
#' @importFrom purrr map %>%
#' @importFrom ggplot2 ggplot aes aes_ theme geom_rect geom_vline geom_text geom_blank position_nudge element_blank ggplotGrob element_text
#' @importFrom cowplot gtable_remove_grobs gtable_squash_rows gtable_squash_cols plot_grid ggdraw theme_cowplot
#' @importFrom utils head
## @import lazyeval
#' @return ggplot item to be plotted using cowplot::ggdraw().
## @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' 'ms_data.csv' %>% complete_counts() %>% covplot(groupid='ID',elementid='Peptide',sort_column='Start') %>% ggdraw

#default sort column is Start
covplot <- function(input_data,elementid,groupid='',group_name='',sort_column='Start'){

    #test if there are elements that appear under different groups.
    uniqueness_test <- input_data %>% dplyr::group_by_(elementid, groupid) %>%
    dplyr::summarize(groupsize = dplyr::n()) %>%
    dplyr::filter(groupsize > 1)
    if(nrow(uniqueness_test) > 0){
        cat(paste0("Warning, ", nrow(uniqueness_test), " elements are not unique to groups.\nEx. peptides match multiple proteins."))
    }

    #pre-filter columns that pertain to the plot
    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,'Start','End','Length')]

    #if there is a specified group name but no ID column, stop running and show error.
    if(groupid=='' & group_name!=''){
        stop('Group ID column required to filter by group.')
    }
    #if there is a specified group Id column and a specified group name, filter data that whose Id value is equal to group_name
    if(groupid!=''&group_name!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    #otherwise, don't do anything
    }else{
        data_group <- pre_data
    }
    #unnecessary step to be purged. Pending.
    cov_data <- data_group

    #Feed peptides into cov_row function
    cov_data = droplevels(cov_data)

    #reorder elementid column by sort_column. Default: Start.
    cov_data[,colnames(cov_data)==elementid] <- stats::reorder(cov_data[,colnames(cov_data)==elementid],cov_data[,colnames(cov_data)==sort_column])
    #Feed peptides into covplot_row function
    cov_data %>% split(select_(cov_data,elementid)[,1]) %>% purrr::map(covplot_row,elementid=elementid) -> cov_list

    #Aggregate ggplot objects
    clusterplot <- cowplot::plot_grid(plotlist = cov_list, ncol=1, align = "v")

}
