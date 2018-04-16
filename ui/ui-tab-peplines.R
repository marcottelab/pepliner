## ==================================================================================== ##
# Pepliner: App for visualizing peptide elution data
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


tabPanel("Peptide plots",
         fluidRow(column(4,wellPanel(
           #tags$head( tags$style("body {background-color: white; }")),


           selectizeInput("pepsel_id",
                          label="ID (Select 1 or more)",#or Ensembl ID",
                          choices = NULL,
                          multiple=TRUE,
                          options = list(
                            placeholder = 
                              'Start typing to search for an ID'
                          )
           ), 

           actionButton("go", "Go"),

           checkboxInput("pep_min_graphic_details",label="Minimal graphic details?", FALSE),
           checkboxInput("pep_pubtheme",label="Publication theme?", FALSE),
           checkboxGroupInput('pep_facets', 'Column(s) to facet by', choices = list("ExperimentID", "condition")),
           checkboxInput("pep_condition",label="Group lines by condition?", FALSE),
           uiOutput("pep_ExperimentControls"),
           uiOutput("pep_ConditionControls"),

#           #checkboxInput("fillUnder",label="Fill color under line?", TRUE),
           colourInput("pep_fill", "Fill color", "#E69F00", allowTransparent = TRUE),    
           numericInput("pep_linewidth", "Line size", min = 0, value = 0.3, step = 0.2), 
           numericInput("pep_lineoverlap", "Line overlap", min = 0, value = 0.9, step = 0.2), 
           numericInput("pep_legend_tick_spacing", "Tick spacing", min = 0, value = 100, step = 20), 

#
#
           radioButtons("pep_device", label = "File type?", choices = c("pdf", "png"), selected="pdf"), 
           numericInput("pep_plotratio", "Plot size ratio, left:right, 1:?", min = 0, value = 1, step = 0.5), 
           numericInput("pep_plotwidth", "Width", min = 0, value = 600, step = 50), 
           numericInput("pep_plotheight", "Height", min = 0, value = 600, step = 50),
 
           downloadButton('downloadPepPlot', 'Download Plot')
         ) #end WellPanel  
         ),
         #, #end column, 
          
        

         column(8,
                tabsetPanel(
                  #tabPanel(title="PepLinePlot",
                  #             plotOutput('peplineplot', height=600)
                  #         
                  #),#end tabPanel

                  tabPanel(title="PepLinePlot",
                           fluidRow(
 
                             #column(10,plotOutput('pepplots', height=600)),
                             column(12,plotOutput('peplineplot', height=600))
                           )
                  ),


                  tabPanel(title="Data Output",
                           h5("Download data for selected peptides")#, downloadButton('Download Data Sutbset as CSV File')#, DT::dataTableOutput("dat_peplineplot")
                  )#,#end tabPanel



                )
                )#column



         
         )#sidebarLayout	
) #end tabPanel Gene Data
