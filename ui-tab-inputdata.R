## ==================================================================================== ##
# Pepliner: App for visualizing protein elution data
#
# Modified 2018 from the original GNUpl3 by Claire D. McWhitei <claire.mcwhite@utexas.edu>

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
## 
## 
## # This tab is used to input the count or normalized data files

tabPanel("Input Data", 
         fluidRow(column(4,wellPanel(
           downloadLink("instructionspdf",label="Download Instructions (pdf)"),
           radioButtons('data_file_type','Use example file or upload your own data',
                        c('Upload Data'="upload",
                          'Pepliner RData file'="previousrdata",
                          'Example Data'="examplecounts"
                        ),selected = "examplecounts"),
           conditionalPanel(condition="input.data_file_type=='previousrdata'",
                            fileInput('rdatafile','Upload Pepliner App Generated RData File'),
                            conditionalPanel("output.fileUploaded",h4(strong("Check data contents then click:")))
           ),
           conditionalPanel(condition="input.data_file_type=='upload'",
                            radioButtons("inputdat_type","Input data type:",
                                         c("Peptides"="peps",
                                           "Proteins"="prots")),
                            conditionalPanel(condition="input.data_file_type=='upload'",
                                             radioButtons("inputdat_format", "Input data format:", 
                                                         c("Tidy"="tidy", 
                                                          "Wide matrix"="wide")),

                            

                            conditionalPanel(condition="input.inputdat_type=='peps' && input.inputdat_format=='wide'",
                                             downloadLink("example_peptide_analysis_file",label="Download Example Peptide Count file"),
                                             p(""),
                                             img(src="exampleanalysisdata.png",width="100%"),
                                             tags$ul(
                                               tags$li("File must have a header row."), 
                                               tags$li("First/Left-hand column must be peptide sequence"),
                                               tags$li("Second column must be protein ID"),
                                               tags$li("Fraction names in right hand columns")
                                             )
                            ),


                            conditionalPanel(condition="input.inputdat_type=='peps' && input.inputdat_format=='tidy'",
                                             downloadLink("example_peptide_counts_matrix_file",label="Download Example Peptide Count file"),
                                             p(""),
                                             img(src="example_peptide_counts_tidy.png",width="100%"),
                                             tags$ul(
                                               tags$li("File must have a header row."), 
                                               tags$li("First/Left-hand column must be peptide sequences."), 
                                               tags$li("Second column must be protein IDs"),
                                               tags$li("Third column must be fraction name"),
                                               tags$li("Fourth column must be value (ex. spectral counts)")
                                             )
                            ),
                            conditionalPanel(condition="input.inputdat_type=='prots' && input.inputdat_format=='wide'",
                                             downloadLink("example_protein_analysis_file",label="Download Example Protein Count file"),
                                             p(""),
                                             img(src="exampleanalysisdata.png",width="100%"),
                                             tags$ul(
                                               tags$li("File must have a header row."), 
                                               tags$li("First/Left-hand column must be protein name/identifier"),
                                               tags$li("Fraction names in right hand columns")
                                             )
                            ),

                            conditionalPanel(condition="input.inputdat_type=='prots_tidy' && input.inputdat_format=='wide'",
                                             downloadLink("example_protein_counts_matrix_file",label="Download Example Peptide Count file"),
                                             p(""),
                                             img(src="example_protein_counts_tidy.png",width="100%"),
                                             tags$ul(
                                               tags$li("File must have a header row."), 
                                               tags$li("First/Left-hand column must be protein sequences."), 
                                               tags$li("Third column must be fractionID"),
                                               tags$li("Fourth column must be value (ex. spectral counts)")
                                             )
                            )
 
                            ),




                            fileInput('datafile', 'Choose File Containing Data (.CSV)',
                                      accept=c('text/csv', 
                                               'text/comma-separated-values,text/plain', 
                                               '.csv'))#,
                            #conditionalPanel(condition="input.inputdat_type=='analyzed'",
                             #                selectInput("c_geneid1",label="First column # with gene IDs",choices=NULL),
                              #               selectInput("c_geneid2",label="Last column # with gene IDs",choices=NULL),
                               #              selectInput("c_expr1",label="First column # with expression values",choices=NULL),
                                #             selectInput("c_expr2",label="Last column # with expression values",choices=NULL),
                                 #            selectInput("c_fc1",label="First column # with fold changes",choices=NULL),
                                  #           selectInput("c_fc2",label="Last column # with fold changes",choices=NULL),
                                   #          radioButtons("isfclogged",label="Is FC logged? (if false, expression values will be log2-transformed for visualization)",choices=c("Yes (Leave it alone)","No (Log my data please)"),selected="No (Log my data please)"),
                                    #         selectInput("c_pval1",label="First column # with p-values",choices=NULL),
                                     #        selectInput("c_pval2",label="Last column # with p-values",choices=NULL)
                            #)
           ),
           conditionalPanel("output.fileUploaded",
                            actionButton("upload_data","Submit Data",
                                         style="color: #fff; background-color: #BF5700; border-color: #9E0000"))
         )#,
         # add reference group selection
         # add instructions
         # missing value character?
         ),#column
         column(8,
                bsCollapse(id="input_collapse_panel",open="data_panel",multiple = FALSE,
                           bsCollapsePanel(title="Data Contents: Wait for upload and check Before `Submit`",value="data_panel",
                                           dataTableOutput('countdataDT')                       
                           ),
                           bsCollapsePanel(title="Analysis Results: Ready to View Other Tabs",value="analysis_panel",
                                           downloadButton('downloadResults_CSV','Save Results as CSV File'),
                                           downloadButton('downloadResults_RData',
                                                          'Save Results as START RData File for Future Upload',
                                                          class="mybuttonclass"),
                                           dataTableOutput('analysisoutput'),
                                           tags$head(tags$style(".mybuttonclass{background-color:#BF5700;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"))
                           )
                )#bscollapse
         )#column
         )#fluidrow
)#tabpanel
