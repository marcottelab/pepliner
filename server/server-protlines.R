
#observe({

#  print("server-protlineplot-update")
#
#  data_analyzed = analyzeDataReactive()
#  tmpids = data_analyzed$ids
#  data_analyzedids = as.character(unlist(tmpids))
  
#  updateSelectizeInput(session,'protsel_id',
#                       choices= data_analyzedids,
#                       server=TRUE)

#})

observe({

  print("server-protlineplot-update")

  data_analyzed = analyzeDataReactive()
  tmpids = data_analyzed$ids
  data_analyzedids = as.character(unlist(tmpids))
  
  updateSelectizeInput(session,'protsel_id',
                       choices= data_analyzedids,
                       server=TRUE)


})

output$ExperimentControls <- renderUI({
  data_analyzed = analyzeDataReactive()
  df_norm_prot = data_analyzed$df_norm_prot
  experiments = unique(df_norm_prot$ExperimentID)
  checkboxGroupInput("experiments", "Choose Experiments to show", experiments, selected = experiments)
})

print("server-protlines")

plotInput <- reactive({

  print("drawing lines")

  validate(need(length(input$protsel_id) > 0,"Please select proteins to plot."))

  data_analyzed = analyzeDataReactive()
  df_norm_prot = data_analyzed$df_norm_prot
  ids = data_analyzed$ids
  print(ids)
  print(input$experiments)
  df_norm_prot = df_norm_prot %>% filter(ExperimentID %in% input$experiments)
  #CDM not sure what this if for# 
  #pdf(NULL)
  #print(input$fill, input$facet, input$lineoverlap)
  p=protlineplot_fun(df_norm_prot = df_norm_prot,ids = ids,
              protsel_id = input$protsel_id, fill=input$fill, size=input$linewidth, scale = input$lineoverlap, min_graphic_details = input$min_graphic_details, condition=input$condition, facets=input$facets
              )



 
  #p <-ggplot(df, aes_string(x=names(df)[1], y=names(df)[2])) +
  #    geom_point()
  #print(is.na(p))
  #return(p)
  })

observe({

output$protlineplot <- renderPlot({

  #if (names(dev.cur()) != "null device") {dev.off()}
  print(plotInput())
  #if (names(dev.cur()) != "null device") {dev.off()}

}, width = input$plotwidth, height = input$plotheight) #renderPlot
})


DataProtLineplotReactive <- reactive({
  print("DataProtLineplotReactive")
  data_analyzed = analyzeDataReactive()

  subdat = protlineplot_dat(df_norm_prot = data_analyzed$df_norm_prot, ids = data_analyzed$ids,
                       protsel_id = input$protsel_id)
  return(subdat)
})


output$dat_protlineplot <- DT::renderDataTable({
  tmpdat = DataProtLineplotReactive()
  tmpdat[,sapply(tmpdat,is.numeric)] <- signif(tmpdat[,sapply(tmpdat,is.numeric)],3)
  DT::datatable(tmpdat)
}
)

output$downloadProtPlot <- downloadHandler(
    filename = function() { paste('protlineplot_', paste(input$protsel_id, collapse="_"), '.', input$device, sep='') },
    content = function(file) {
        #ggsave(file, plot = plotInput(), device = "pdf")
        #file.copy(paste('protlineplot_', paste(input$protsel_id, collapse="_"), '.pdf', sep='') , file, overwrite=TRUE)
        ggsave(file, plot = plotInput(), device = input$device, width=0.08*input$plotwidth, height=0.08*input$plotheight, units='cm')
    }
)


##
#
##CDM not sure if this works yet
#output$downloadSubsetData <- downloadHandler(
##  #To do: make filename from selected IDs
#  protsel_id_str <- paste(input$protsel_id,collapse="_")
#  filename = paste('protlineplot', protsel_id_str, '.csv', sep='')
#  content = function(file) {write.csv(DataLineplotReactive(), file, row.names=FALSE)}
#)
#
#
#

