
#' Normalize values in a vector. Auxiliary function.
#'
#'  Each sparkline row should have values scaled from 0 to 1.
#' @param m A vector
#' @export
normalit<-function(m){
    (m - min(m))/(max(m)-min(m))
}

#auxiliary function to normalize and plot each row of the line plots
sparkplot_row <- function(z,elementid,xaxis,yaxis,condit){
    #convert string variable into a name to be used for Standard Evaluation
    xaxis <- as.name(xaxis)
    #if there is a condition column, point the variable 'condition' to it. Otherwise, make each element's Id its own condition.
    if(condit!=''){
        condition <- as.name(condit)
    }else{condition<-as.name(elementid)}

    #group the data by condition
    z2 <- z %>% dplyr::group_by_(condition)
    #normalize yaxis column (lazy evaluation), make a list of the normalized values, replace original yaxis with the list of normalized values through SE mutate (mutate_)
    z2 <- z2 %>% dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~normalit(var),var=as_name(yaxis))),yaxis))
    #arrange the rows by yaxis value. (Nico: Why?)
    z2 <- z2 %>% dplyr::arrange_(.dots=yaxis)
    #fill NAs with 0. This step should not be necessary if data went through complete_counts()
    z2[yaxis][is.na(z2[yaxis])] <- 0

    #ggplot object creation for a single row
    plt <- ggplot2::ggplot(z2, ggplot2::aes_string(x=xaxis, y=yaxis, group=condition, color=condition, linetype=condition)) +
        #get rid of the grey grid in the background set by default in ggplot2
        cowplot::theme_cowplot() +
        #create individual line plot
        ggplot2::geom_line(size=0.8, position=ggplot2::position_dodge(width=0.25), alpha=0.9) +

        #to be used when there are different data conditions
        #set 100 color formats as a repetition of a 5-color colorblind-friendly palette extracted from colorbrewer2 (http://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=10)
        ggplot2::scale_color_manual(values=rep(c("#74add1","#a50026","#fee090","#f46d43","#4575b4"),20)) +
        #set 100 linetype formats as a repetition of 5 different formats.
        ggplot2::scale_linetype_manual(values=rep(c("solid","twodash","F1","dotted","dotdash"),20)) +
        #no legend
        ggplot2::theme(legend.position="None")

    #generate a ggplot2 plot grob from the previous ggplot object
    final_plt <- ggplot2::ggplotGrob(plt)



    #remove unnecessary plot elements
    final_plt <- cowplot::gtable_remove_grobs(final_plt, c('title','subtitle','caption','spacer', 'xlab-b','ylab-l', 'axis-b','axis-l','spacer'))

    #print(final_plt)
    #compress unused plot space
    #print(unique(z$Peptide))
    final_plt <- cowplot::gtable_squash_rows(final_plt, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    final_plt <- cowplot::gtable_squash_cols(final_plt, c(1, 2, 3, 5, 6, 7))


    #print(plot_grid(final_plt))
    return(final_plt)
}



#' Make a line plot for each element of a group.
#'
#' @param input_data Data frame to which the coverage columns will be appended.
#' @param groupid Data frame column (factor) corresponding to the names of the groups of elements in the data.
#' @param group_name (Optional) If specified, make plots from only those rows whose ID matched this argument.
#' @param elementid Data frame column (factor) corresponding to the elements for each of which a row of the final plot will be produced.
#' @param xaxis X-axis values for each sub-plot. Must match across elements.
#' @param yaxis Y-axis values for each sub-plot. The final plot normalizes these values for each row.
#' @param condit (Optional) Name of the condition column from the data frame. Data points corresponding to different values of condit will be plotted with a different color/linetype.
#' @param sort_column (Optional) If specified, rows in the final plot will be sorted according to this (numeric) column.
#' @importFrom dplyr group_by_ mutate_ arrange_ select_
#' @importFrom tidyr spread_ gather_
#' @importFrom purrr map %>%
#' @importFrom ggplot2 ggplot aes_string position_dodge geom_line scale_color_manual scale_linetype_manual theme ggplotGrob
#' @importFrom cowplot gtable_remove_grobs gtable_squash_rows gtable_squash_cols plot_grid ggdraw theme_cowplot
#' @importFrom lazyeval interp as_name
#' @return ggplot item to be plotted using cowplot::ggdraw().
## @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' library(purrr)
#' test_data <- read.csv(system.file('extdata/msdata.csv',package='pepliner'))
#' sequences <- system.file('extdata/proteome.fasta',package='pepliner')
#' cov_columns(test_data,sequences,groupid='ID',elementid='Peptide') %>%
#' complete_counts('FractionID','PeptideCount') %>%
#' sparkplot('Peptide','FractionID','PeptideCount','ID',
#'     group_name='sp|P55036|PSMD4_HUMAN',sort_column='Start') %>%
#' cowplot::ggdraw()

sparkplot <- function(input_data,elementid,xaxis,yaxis,groupid='',group_name='',condit='',sort_column=''){
    #pre-filter columns that pertain to the plot
    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,xaxis,yaxis,condit,'Start')]

    #if there is a specified group name but no ID column, stop running and show error.
    if(groupid=='' & group_name!=''){
        stop('Group ID column required to filter by group.')
    }
    #if there is a specified group Id column and a specified group name, filter data that whose Id value is equal to group_name
    if(group_name!=''&groupid!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    #otherwise, don't do anything
    }else{
        data_group <- pre_data
    }

    #filter for a single protein in a list
    #To do: order n term to c term using start and end
    #Make a wide matrix from the tidy data

    data_wide <- tidyr::spread_(data_group, xaxis, yaxis)

    #Fill in NA with 0's to com plete the matrix
    data_wide[is.na(data_wide)] <- 0

    #Turn the wide matrix tidy.
    spark_data <- data_wide %>% tidyr::gather_(xaxis, yaxis, names(.)[!names(.)%in%c(groupid,elementid,'Start','End','Length','Appearance',condit,sort_column)])

    #Nico: why?
    spark_data = droplevels(spark_data)

    #if there is a specified column to sort the plots by, reorder elementid column by it (which will affect the way the rows are fed into the plot)
    if(sort_column!=''){
        spark_data[,colnames(spark_data)==elementid] <- stats::reorder(spark_data[,colnames(spark_data)==elementid],spark_data[,colnames(spark_data)==sort_column])
    }

    #Feed peptides into sparkplot_row function
    spark_data %>% split(dplyr::select_(spark_data,elementid)[,1]) %>% purrr::map(sparkplot_row,elementid=elementid,xaxis=xaxis,yaxis=yaxis,condit=condit) -> spark_list

    #Aggregate ggplot objects
    clusterplot <- cowplot::plot_grid(plotlist = spark_list, ncol=1, align = "v")

}

