
observe({

  print("server-lineplot-update")

  data_analyzed = analyzeDataReactive()
  tmpids = data_analyzed$ids
  data_analyzedids = as.character(unlist(tmpids))
  #data_analyzedgenes = c("a","b","c")
  #
  #tmpynames = data_analyzed$data_long%>%select(-unique_id,-sampleid,-group)%>%colnames()
  updateSelectizeInput(session,'sel_id',
                       choices= data_analyzedids,
                       server=TRUE)

})



print("server-lines")
output$lineplot <- renderPlot({

  print("drawing lines")

  validate(need(length(input$sel_id) == 1,"Please select one gene."))

  data_analyzed = analyzeDataReactive()
  df_full = data_analyzed$df_full
  print(names(df_full))
  ids = data_analyzed$ids
  print(head(ids))
  #CDM not sure what this if for# if (names(dev.cur()) != "null device") dev.off()
  #pdf(NULL)
  p=lineplot_fun(df_full = df_full,ids = ids,
              sel_id=input$sel_id
              )
  #ggsave(p, filename="test2.png")
  plot(p)
}) #renderPlot

output$pepplot <- renderPlot({

  print("drawing lines")

  validate(need(length(input$sel_id) == 1,"Please select one gene."))

  data_analyzed = analyzeDataReactive()
  df_full = data_analyzed$df_full
  print(names(df_full))
  ids = data_analyzed$ids
  print(head(ids))
  #CDM not sure what this if for# if (names(dev.cur()) != "null device") dev.off()
  #pdf(NULL)
  p=pepplot_fun(df_full = df_full,ids = ids,
              sel_id=input$sel_id
              )
  #ggsave(p, filename="test2.png")
  plot(p)
}) #renderPlot



DataLineplotReactive <- reactive({
  print("DataLineplotReactive")
  data_analyzed = analyzeDataReactive()

  subdat = lineplot_dat(df_full = data_analyzed$df_full, ids = data_analyzed$ids,
                       sel_id=input$sel_id)
  return(subdat)
})

output$dat_lineplot <- DT::renderDataTable({
  tmpdat = DataLineplotReactive()
  tmpdat[,sapply(tmpdat,is.numeric)] <- signif(tmpdat[,sapply(tmpdat,is.numeric)],3)
  DT::datatable(tmpdat)
}
)

output$downloadSubsetData <- downloadHandler(
  filename = c('lineplot_data.csv'),
  content = function(file) {write.csv(DataLineplotReactive(), file, row.names=FALSE)}
)
