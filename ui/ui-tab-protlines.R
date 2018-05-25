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
                          )
           ), 

           radioButtons("prot_height_normalization", label = "Height normalization?", choices = list("None", "Protein"), selected= "Protein"),
           radioButtons("prot_group_normalization", label = "Group normalization?", choices = list("None", "Experiment"), selected= "Experiment"),

           checkboxInput("min_graphic_details",label="Minimal graphic details?", FALSE),
           checkboxGroupInput('facets', 'Column(s) to facet by', choices = list("ExperimentID", "condition")),
           checkboxInput("condition",label="Group lines by condition?", FALSE),
           uiOutput("ExperimentControls"),
           #checkboxInput("fillUnder",label="Fill color under line?", TRUE),
           colourInput("fill", "Fill color", "#E69F00", allowTransparent = TRUE),    
           numericInput("linewidth", "Line size", min = 0, value = 0.3, step = 0.2), 
           numericInput("lineoverlap", "Line overlap", min = 0, value = 0.9, step = 0.2), 


           numericInput("plotwidth", "Width", min = 0, value = 600, step = 50), 
           numericInput("plotheight", "Height", min = 0, value = 600, step = 50), 

           radioButtons("device", label = "File type?", choices = c("pdf", "png"), selected="pdf"), 
           numericInput("prot_trueplotwidth", "Download plot Width (inches)", min = 0, value = 5, step = 1),
           numericInput("prot_trueplotheight", "Download plot Height (inches)", min = 0, value = 3, step = 1),


           downloadButton('downloadProtPlot', 'Download Plot')
         ) #end WellPanel  
         ), #end column, 
         

         column(8,
                tabsetPanel(
                  tabPanel(title="ProtLinePlot",
                               plotOutput('protlineplot', height=600)
                           
                  ),#end tabPanel
                  tabPanel(title="Data Output",
                           h5("Download data for selected proteins"), downloadButton('downloadProtLineSubsetData','Download Data Subset as CSV File'), DT::dataTableOutput("dat_protlineplot")
                  )#,#end tabPanel



                ))#column
         
         )#sidebarLayout	
) #end tabPanel Gene Data
