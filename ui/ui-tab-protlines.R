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


## ==================================================================================== ##
## GENE DATA TAB 
## ==================================================================================== ## 

tabPanel("Protein plots",
         fluidRow(column(4,wellPanel(
           tags$head( tags$style("body {background-color: white; }")),
           selectizeInput("protsel_id",
                          label="ID (Select 1 or more)",#or Ensembl ID",
                          choices = NULL,
                          multiple=TRUE,
                          options = list(
                            placeholder = 
                              'Start typing to search for an ID'
                          ) #,
           ), 

#,#end selectInput
           #h5(htmlOutput("geneurl")),
           #checkboxGroupInput("sel_group",
           #                   label="Select Group",
           #                   choices="", selected=""
           #),	
           #checkboxInput("facet",label="Facet by ExperimentID column?", FALSE),
           checkboxInput("min_graphic_details",label="Minimal graphic details?", FALSE),
           checkboxGroupInput('facets', 'Column(s) to facet by', choices = list("ExperimentID", "condition")),
           checkboxInput("condition",label="Group lines by condition?", FALSE),

           #checkboxInput("fillUnder",label="Fill color under line?", TRUE),
           colourInput("fill", "Fill color", "#E69F00", allowTransparent = TRUE),    
           numericInput("linewidth", "Line size", min = 0, value = 0.3, step = 0.2), 
           numericInput("lineoverlap", "Line overlap", min = 0, value = 0.9, step = 0.2), 


           radioButtons("device", label = "File type?", choices = c("pdf", "png"), selected="pdf"), 
           numericInput("plotwidth", "Width", min = 0, value = 600, step = 50), 
           numericInput("plotheight", "Height", min = 0, value = 600, step = 50), 
           downloadButton('downloadProtPlot', 'Download Plot')
           #radioButtons("ytype","Y axis:",choices="")
                        #c("fitted cpm"="cpm","count"="count")),
#            radioButtons("log2cpm_checked","Y axis transformation:",
#                         c("log2"=TRUE,"raw value"=FALSE))
         ) #end WellPanel #,#br(),br(),br(),br(),br(),br(),br(),br(), 
         ), #end column, 
         ## ==================================================================================== ##
         ## GENE DATA: DOT PLOT
         ## Search gene name to view dotplot(s) of expression    
         ## ==================================================================================== ## 
         column(8,
                tabsetPanel(
                  tabPanel(title="ProtLinePlot",
                               plotOutput('protlineplot', height=600)
                           
                  ),#end tabPanel
                  tabPanel(title="Data Output",
                           h5("Download data for selected proteins"), downloadButton('Download Data Sutbset as CSV File'), DT::dataTableOutput("dat_protlineplot")
                           #br(),br(),br(),br(),br(),br(),br(),br(), br(),br(),br(), 
                           #img(src="KCardio_CMYK_4C_pos_small.jpg",height=150,width= 275,align="right")	
                  )#,#end tabPanel
                   #==================================================================================== ##
                   #Protein data: Selected proteins
                   #==================================================================================== ## 
                  #tabPanel(title="Data Output",
                    #       downloadButton('downloadSubsetData', 'Download Data Subset as CSV File'),
                           #DT::dataTableOutput("dat_protlineplot")
                  #)# end tabPanel



                ))#column
         
         )#sidebarLayout	
) #end tabPanel Gene Data
