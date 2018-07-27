## ==================================================================================== ##
# pepliner Shiny App for view protein elution profiles.
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


observe({
  # Check if example selected, or if not then ask to upload a file.
  # Automatically load example file?
  validate(
    need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))),
         message = "Please select a file")
  )

  inFile <- input$datafile
  inProteome <- input$proteomefile
})


inputDataReactive <- reactive({

  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  print("inputting data")
  # Check if example selected, or if not then ask to upload a file.
  validate(
    need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))),
         message = "Please select a file")
  )
  inFile <- input$datafile
  inRFile <- input$rdatafile
 # browser()

  #This is for handling preloaded data
  #How will this work if the working directory isn't pepliner?
  if(input$data_file_type=="examplecounts") {
    # upload example data
    seqdata <- read_csv("data/Hs_CB660_1105_peptide_elution_human_protein_minimal.csv")
    print("uploaded peptide data")
    proteomedata <- read_fasta("data/uniprot-proteome_human_reviewed_minimal.fasta")
    return(list('data'=seqdata))#, 'proteomedata'=proteomedata))
  }else if(input$data_file_type == "previousrdata"){
    if (!is.null(inRFile)) {
      load(inRFile$datapath,envir = environment())
      return(list("data" = data_results_table)) # this is so something shows in data upload window
    }else{return(NULL)}

  #This is for handling uploaded data
  }else { # if uploading data

    if (!is.null(inFile)) {
      seqdata <- read_csv(inFile$datapath)
      print('uploaded seqdata')
         if(ncol(seqdata)==1) { # if file appears not to work as csv try tsv
            seqdata <- read_tsv(inFile$datapath)
            print('changed to tsv, uploaded seqdata')
         }
      validate(need(ncol(seqdata) > 1,
                    message="File appears to be one column. Check that it is a comma-separated (.csv) file."))


      return(list('data' = seqdata))



      }else{return(NULL)}
  }
})


inputProteomeDataReactive <- reactive({
  print("PROTEOME DATA")

  inProteome <- input$proteomefile
  if(input$data_file_type=="examplecounts") {
    # upload example data
    # Need to get some better example data
    proteomedata <- read_fasta("data/uniprot-proteome_human_reviewed_minimal.fasta")
    return(list('proteomedata'=proteomedata))

  }

  #This is for handling uploaded proteomedata
  else if (input$inputdat_type == "peps"){ # if uploading data
    
       if (!is.null(inProteome)) {
            print(inProteome)
            proteomedata <- read_fasta(inProteome$datapath)
      print('uploaded Proteome')
      validate(need(ncol(proteomedata) == 2,
                    message="Check that input file is in FASTA format"))

      return(list('proteomedata' = proteomedata))

      }else{return(NULL)}
  }

})

# check if a file has been uploaded and create output variable to report this
output$fileUploaded <- reactive({
  return(!is.null(inputDataReactive()))
})
outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)


# check if a file has been uploaded and create output variable to report this
output$ProteomeFileUploaded <- reactive({
  return(!is.null(inputProteomeDataReactive()))
})
outputOptions(output, 'ProteomeFileUploaded', suspendWhenHidden=FALSE)





# after the data is uploaded or example data is selected, analyze the data
analyzeDataReactive <-
  eventReactive(input$upload_data,
                ignoreNULL = FALSE, {
                  withProgress(message = "Formatting, please wait",{

                    print("analysisCountDataReactive")


                    #if uploading own data:

                    #if(input$data_file_type=="previousrdata"){
                    #  inRfile <- input$rdatafile
                    #  load(inRfile$datapath,envir=environment())
                    #
                    #  return(list('group_names'=group_names,'sampledata'=sampledata,
                    #              "results"=results,"data_long"=data_long,
                    #              "geneids"=geneids, "expr_data"=expr_data,
                    #              "data_results_table"=data_results_table))
                    #}
 
                    #if(input$data_file_type == "examplecounts" | input$inputdat_type == "peps" | input$inputdat_format == "tidy"){
                    ############################# DATA PROCESSING ###############################

                    alldata <- inputDataReactive()$data

                    # Check for numeric columns
                    not_numeric <- function(input) {
                      if(sum(unlist(lapply(input,function(k) class(k) %in% c("numeric","integer"))))==0) {
                        "Your data does not appear to be formatted correctly (no numeric columns).
                        Please check your input file."
                      } else if (input == "") {
                        FALSE
                      } else {
                        NULL
                      }
                    }

                    validate(
                      not_numeric(alldata)
                    )

                    # remove empty columns
                    alldata = alldata[,colMeans(is.na(alldata)) < 1]
                    ids <- alldata %>% select(ID) %>% unique
                    # Add missing columns if not provided
                    if(!"ExperimentID" %in% names(alldata)){
                           alldata$ExperimentID <- "Experiment1"
                      }
                            
                    if(!"condition" %in% names(alldata)){
                           alldata$condition <- "Condition1"
                    }
 

                    if(input$inputdat_type == "peps" | input$data_file_type == "examplecounts") {
                          proteomedata <- inputProteomeDataReactive()$proteomedata

                          df_peps <- alldata %>% select(Peptide, ID) %>% unique
                        
                          print("mapping peptides to proteome")
                          df_seq <- dplyr::left_join(df_peps, proteomedata, by = "ID")
                          df_cov <- cov_columns(df_seq)
         
                        if(input$inputdat_format=="tidy" | input$data_file_type == "examplecounts"){
                            print("Completing missing counts")

                            #This is NOT working. Troubleshoot later
                            alldata <- alldata %>% select(Peptide, ID,FractionID,PeptideCount, ExperimentID, condition)
                            #df_comp <- alldata %>% group_by(ExperimentID) %>%
                            #    spread(FractionID, PeptideCount, fill=0) %>% 
                            #    gather(FractionID, PeptideCount, unique(alldata$FractionID)) %>% ungroup
                            df_comp <- alldata


                        } 
                        if(input$inputdat_format=="wide"){
                            df_comp <- alldata %>% gather(FractionID, PeptideCount, -condition, -ExperimentID, -ID)

                        }
 
                        print("Normalizing counts")
                        df_norm <- df_comp %>% group_by(Peptide, ExperimentID, ID) %>%
                            mutate(ExpNormPeptideCount = normalit(PeptideCount)) %>% ungroup %>%
                            group_by(Peptide, ID) %>% 
                            mutate(PepNormPeptideCount = normalit(PeptideCount)) %>% ungroup
                         

                        print("Final join")
                        df_full <- left_join(df_norm, df_cov, by = c("Peptide", "ID"))
                               
                        #df_prot <- df_comp %>% group_by(ID, FractionID, ExperimentID) %>% 
                        #        summarize(ProteinCount = sum(PeptideCount))
                        
                        df_prot <- df_comp %>% group_by_at(vars(-PeptideCount, -Peptide)) %>% 
                                summarize(ProteinCount = sum(PeptideCount))
  

                        print('analyze peptide data: done')
    
                    }
                    if(input$inputdat_type=="prots") {

                         if(input$inputdat_format == "tidy"){
                            
                           alldata_fractions <- alldata %>% select(ExperimentID, FractionID) %>% unique 
                           df_prot <- left_join(alldata_fractions, alldata) 
                           df_prot$ProteinCount <- df_prot$ProteinCount %>% replace_na(0.001)
                           test <- df_prot %>% filter(is.na(ProteinCount))
                           print(test) 
                           

                           #df_prot <- alldata %>% group_by(ExperimentID) %>%
                           #     spread(FractionID, ProteinCount, fill=0)
                           #print(df_prot)
 
                           #df_prot <- df_prot %>% gather(FractionID, ProteinCount, 4:n)#, -ID, -ExperimentID, -condition)#, unique(alldata$FractionID))
                           #nested <- SEC_filt_pos %>% nest(-ExperimentID)
                           


                         }

                         if(input$inputdat_format == "wide"){

                            df_prot <- alldata %>% 
                                             gather(FractionID, ProteinCount, -condition, -ExperimentID, -ID) 
                         } 

                         print('analyze protein data: done')
                         #There's no peptide data if only proteins provided
                         df_full=NULL                    
                    }

                        df_norm_prot <- df_prot %>% group_by(ID, ExperimentID) %>%
                                   mutate(ExpNormProteinCount = normalit(ProteinCount)) %>% ungroup %>%
                                   group_by(ID) %>%
                                   mutate(ProtNormProteinCount = normalit(ProteinCount)) %>% ungroup

                     
                        return(list('df_full'=df_full, 'ids'=ids$ID, 'df_norm_prot'=df_norm_prot))
                  })
                })

output$countdataDT <- renderDataTable({
  tmp <- inputDataReactive()
  if(!is.null(tmp)) tmp$data
})
output$proteomeDT <- renderDataTable({
  tmp2 <- inputProteomeDataReactive()
  if(!is.null(tmp2)) tmp2$proteomedata
})




observeEvent(input$upload_data, ({
  updateCollapse(session,id =  "input_collapse_panel", open="analysis_panel",
                 style = list("analysis_panel" = "success",
                              "data_panel"="primary"))
}))

observeEvent(inputDataReactive(),({
  updateCollapse(session,id =  "input_collapse_panel", open="data_panel",
                 style = list("analysis_panel" = "default",
                              "data_panel"="success"))
})
)

output$analysisoutput <- DT::renderDataTable({
  print("output$analysisoutput")
  getresults <- analyzeDataReactive()
  if(input$inputdat_type=="peps" | input$data_file_type == "examplecounts") {

      res = getresults$df_full
  }
  if(input$inputdat_type=="prots") {

      res = getresults$df_norm_prot
  }


  res[,sapply(res,is.numeric)] <- signif(res[,sapply(res,is.numeric)],3)
  DT::datatable(head(res, 10000))
})


output$downloadResults_CSV <- downloadHandler(filename = paste0("pepliner_results_",Sys.Date(),".csv"),
                                              content = function(file) {
                                                write_csv(analyzeDataReactive()$data_results_table, file)})

output$downloadResults_RData <- downloadHandler(filename= paste0("pepliner_results_",Sys.Date(),".RData"),
                                                content=function(file){
                                                  tmp = analyzeDataReactive()

                                                  df_full = tmp$df_full
                                                  ids = tmp$ids

                                                  save(df_full, ids,,file=file)
                                                })


output$example_counts_file <- downloadHandler(filename="examplecounts_short.csv",
                                              content=function(file){
                                                file.copy("data/examplecounts_short.csv",file)
                                              })

output$example_analysis_file <- downloadHandler(filename="exampleanalysisres_short.csv",
                                                content=function(file){
                                                  file.copy("data/exampleanalysisres_short.csv",file)
                                                })



output$instructionspdf <- downloadHandler(filename="Instructions.pdf",
                                          content=function(file){
                                            file.copy("instructions/Instructions.pdf",file)
                                          })




