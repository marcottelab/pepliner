
observe({

  print("server-protlineplot-update")

  data_analyzed = analyzeDataReactive()
  tmpids = data_analyzed$ids
  data_analyzedids = as.character(unlist(tmpids))
  
  updateSelectizeInput(session,'protsel_id',
                       choices= data_analyzedids,
                       server=TRUE)

})

print("server-protlines")
output$protlineplot <- renderPlot({

  print("drawing lines")

  validate(need(length(input$protsel_id) > 0,"Please select proteins to plot."))

  data_analyzed = analyzeDataReactive()
  df_norm_prot = data_analyzed$df_norm_prot
  ids = data_analyzed$ids
  #CDM not sure what this if for# if (names(dev.cur()) != "null device") dev.off()
  #pdf(NULL)
  p=protlineplot_fun(df_norm_prot = df_norm_prot,ids = ids,
              protsel_id = input$protsel_id
              )
  #ggsave(p, filename="test2.png")
  plot(p)
}) #renderPlot



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
#

