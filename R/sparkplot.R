

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(cowplot))
# suppressPackageStartupMessages(library(lazyeval))
# suppressPackageStartupMessages(library(seqinr))


# options(tibble.width=Inf)


normalit<-function(m){
    (m - min(m))/(max(m)-min(m))
}

sparkplot_row <- function(z,elementid,xaxis,yaxis,condit){
    xaxis <- as.name(xaxis)

    if(condit!=''){
        condition <- as.name(condit)
    }else{condition<-as.name(elementid)}

    z2 <- z %>% group_by_(condition)
    z2 <- z2 %>% mutate_(.dots = setNames(list(interp(~normalit(var),var=as_name(yaxis))),yaxis))
    z2 <- z2 %>% arrange_(.dots=yaxis)
    z2[yaxis][is.na(z2[yaxis])] <- 0

    #If there is one group

    plt <- ggplot(z2, aes_string(x=xaxis, y=yaxis, group=condition, color=condition, linetype=condition)) +
        geom_line(size=0.8, position=position_dodge(width=0.25), alpha=0.9) +
        scale_color_manual(values=rep(c("#74add1","#a50026","#fee090","#f46d43","#4575b4"),20)) +
        scale_linetype_manual(values=rep(c("solid","twodash","F1","dotted","dotdash"),20)) +
        theme(legend.position="None")

    final_plt <- ggplotGrob(plt)



    #remove unnecessary plot elements
    #print(final_plt)

    final_plt <- gtable_remove_grobs(final_plt, c('title','subtitle','caption','spacer', 'xlab-b','ylab-l', 'axis-b','axis-l','spacer'))

    #print(final_plt)
    #compress unused plot space
    #print(unique(z$Peptide))
    final_plt <- gtable_squash_rows(final_plt, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    final_plt <- gtable_squash_cols(final_plt, c(1, 2, 3, 5, 6, 7))


    #print(plot_grid(final_plt))
    return(final_plt)
}


#Tcell_2502_Cyto_sequence_annotated.csv

#' Make a line plot for each element of a group
#'
#' @param input_data Data frame to which the coverage columns will be appended.
#' @param groupid Data frame column (factor) corresponding to the names of the groups of elements in the data.
#' @param group_name (Optional) If specified, make plots from only those rows whose ID matched this argument.
#' @param elementid Data frame column (factor) corresponding to the elements for each of which a row of the final plot will be produced.
#' @param xaxis X-axis values for each sub-plot. Must match across elements.
#' @param yaxis Y-axis values for each sub-plot. The final plot normalizes these values for each row.
#' @param condit (Optional) Name of the condition column from the data frame. Data points corresponding to different values of condit will be plotted with a different color/linetype.
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
#' 'ms_data.csv' %>% complete_counts() %>% sparkplot(groupid='ID',elementid='Peptide',xaxis='FractionID',yaxis='PeptideCount')

sparkplot <- function(input_data,groupid='',group_name='',elementid,xaxis,yaxis,condit='',sort_column=''){

    pre_data <- input_data[colnames(input_data)%in%c(groupid,elementid,xaxis,yaxis,condit,'Start')]

    if(group_name!=''&groupid!=''){
        data_group <- pre_data[pre_data[colnames(pre_data)==groupid]==group_name,] #not elegant, but hey! {base}
    }else{
        data_group <- pre_data
    }

    #filter for a single protein in a list
    #To do: order n term to c term using start and end
    #Make a wide matrix from the tidy data

    data_wide <- spread_(data_group, xaxis, yaxis)

    #Fill in NA with 0's to com plete the matrix
    data_wide[is.na(data_wide)] <- 0

    #Turn the wide matrix tidy.
    spark_data <- data_wide %>% gather_(xaxis, yaxis, names(.)[!names(.)%in%c(groupid,elementid,'Start','End','Length','Appearance',condit,sort_column)])

    #Feed peptides into spark_row function
    spark_data = droplevels(spark_data)

    if(sort_column!=''){
        spark_data[,colnames(spark_data)==elementid] <- reorder(spark_data[,colnames(spark_data)==elementid],spark_data[,colnames(spark_data)==sort_column])
    }

    spark_data %>% split(select_(spark_data,elementid)[,1]) %>% map(sparkplot_row,elementid=elementid,xaxis=xaxis,yaxis=yaxis,condit=condit) -> spark_list

    #Plot ggplot objects
    clusterplot <- plot_grid(plotlist = spark_list, ncol=1, align = "v")

}

