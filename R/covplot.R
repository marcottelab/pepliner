
# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(cowplot))
# suppressPackageStartupMessages(library(lazyeval))


covplot_row <- function(row,elementid){
    # row1 = data[1:12,]
    # row2
    row$dummy_y <- 20

    rect <- ggplot2::ggplot(data = row, ggplot2::aes(x=max(row$Length), y = dummy_y)) +
        cowplot::theme_cowplot() +
        ggplot2::theme(axis.text = ggplot2::element_text(size=8)) +
        ggplot2::geom_blank() +
        ggplot2::theme(
            axis.text.y=ggplot2::element_blank(),
            axis.text.x=ggplot2::element_blank(),
            axis.title.x=ggplot2::element_blank(),
            axis.title.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            axis.ticks.x=ggplot2::element_blank(),
            axis.line.y=ggplot2::element_blank(),
            axis.line.x=ggplot2::element_blank()) +
        ggplot2::geom_rect(ggplot2::aes(xmin=0, xmax=max(row$Length), ymin=0, ymax=20,fill=I('grey95'))) +
        ggplot2::geom_vline(xintercept=seq(0, max(row$Length), by=100),color='grey56') +
        ggplot2::geom_rect(ggplot2::aes(xmin=row$Start, xmax=row$End, ymin=0, ymax=20,fill=I('#f46d43'))) +
        ggplot2::geom_text(ggplot2::aes(label=as.character(head(dplyr::select_(row,elementid)[,1],1))),size=4, position = ggplot2::position_nudge(x=-0.5*(max(row$Length)),y=-10))#as.character(head(row$Peptide,1)))

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
#' @importFrom dplyr select_ group_by_ summarize
## @import tidyr
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes aes_ theme geom_rect geom_vline geom_text geom_blank position_nudge element_blank ggplotGrob element_text
#' @importFrom cowplot gtable_remove_grobs gtable_squash_rows gtable_squash_cols plot_grid ggdraw theme_cowplot
## @import lazyeval
#' @return ggplot item to be plotted using cowplot::ggdraw().
## @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' 'ms_data.csv' %>% complete_counts() %>% covplot(groupid='ID',elementid='Peptide',sort_column='Start') %>% ggdraw

covplot <- function(input_data,elementid,groupid='',group_name='',sort_column='Start'){
    uniqueness_test <- input_data %>% dplyr::group_by_(elementid, groupid) %>%
    dplyr::summarize(groupsize = n()) %>%
    dplyr::filter(groupsize > 1)
    if(nrow(uniqueness_test) > 0){
        cat(paste0("Warning, ", nrow(uniqueness_test), " elements are not unique to groups.\nEx. peptides match multiple proteins."))
    }

    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,'Start','End','Length')]
    if(groupid=='' & group_name!=''){
        stop('Group ID column required to filter by group.')
    }
    if(groupid!=''&group_name!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    }else{
        data_group <- pre_data
    }

    cov_data <- data_group

    #Feed peptides into cov_row function
    cov_data = droplevels(cov_data)

    cov_data[,colnames(cov_data)==elementid] <- stats::reorder(cov_data[,colnames(cov_data)==elementid],cov_data[,colnames(cov_data)==sort_column])
    cov_data %>% split(select_(cov_data,elementid)[,1]) %>% purrr::map(covplot_row,elementid=elementid) -> cov_list

    #Plot ggplot objects
    clusterplot <- cowplot::plot_grid(plotlist = cov_list, ncol=1, align = "v")

}
