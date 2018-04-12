## ==================================================================================== ##
# Pepliner: App for visualizing protein elution data
# Modified 2018 from the original GNUpl3 by Claire D. McWhite <claire.mcwhite@utexas.edu>
# Original Copyright (C) 2016 Jessica Minnier, START Shiny Transcriptome Analysis Tool
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 

## ==================================================================================== ##

library(shiny) #devtools::install_github("rstudio/shiny"); devtools::install_github("rstudio/shinyapps")
library(reshape2)
library(ggplot2)
library(ggthemes)
library(pepliner)
library(cowplot)
library(forcats)
library(purrr)
library(colourpicker)

#library(shinyIncubator) #devtools::install_github("rstudio/shinyIncubator")
#library(gplots)
#library(rjson)
#library(base64enc)
library(ggvis)
library(dplyr)
library(tidyr)
library(DT) #devtools::install_github('ramnathv/htmlwidgets'); devtools::install_github('rstudio/DT')
#library(DESeq2)
#library(edgeR)
library(RColorBrewer)
#library(pheatmap)
library(shinyBS)
library(plotly)
library(markdown)
library(NMF)
library(scales)
#library(heatmaply)
library(readr)

##================================================================================##

source("fun/fun-peplines.R")
source("fun/fun-pepcov.R")
source("fun/fun-protlines.R")
#source("fun-heatmap.R")
#source("fun-analyzecounts.R")
#source("fun-analysisres.R")
#source("fun-groupplots.R")

#troubleshooting
#if(FALSE) {
#  seqdata <- read.csv("data/mousecounts_example.csv",stringsAsFactors = FALSE)
#  load('data/mousecounts_example_analysis_results.RData')
#  load('data/mousecounts_example_analyzed.RData') #example_data_results
#  data_analyzed = list('group_names'=group_names,'sampledata'=sampledata,
#                       "results"=results,"data_long"=data_long, "geneids"=geneids,
#                       "expr_data"=expr_data,"data_results_table"=example_data_results)
#  
#  data_results = data_analyzed$results
#  
#  test_sel = "group2/group1"
#  sel_test = test_sel
#  fdrcut = 0.05
#  absFCcut = 1
#  group_sel = c("group1","group2")
#  valuename = "log2cpm"
#  yname="log2cpm"
#  maxgenes = 200
#  view_group=NULL
#  filter_by_go=FALSE
#  filter_fdr=FALSE
#  filter_maxgene=TRUE
#  filter_cpm=FALSE
#  filter_fc=FALSE
#  fold_change_range=NULL
#  fold_change_groups=NULL
#  group_filter_range =NULL
#  fixed_genes_list=NULL
#  orderby="variance"
#  
#  tmpdat = heatmap_subdat(data_analyzed,yname,orderby="variance",maxgenes=100)
#  heatmap_render(data_analyzed,yname,orderby="variance",maxgenes=100)
#  
#  mydat = heatmap_ggvis_data(
#    data_analyzed = data_analyzed,
#    yname = yname,
#    orderby = "variance",
#    maxgenes=100)
#
#}
