
observe({

  print("server-peplineplot-update")

  data_analyzed = analyzeDataReactive()
  tmpids = data_analyzed$ids
  data_analyzedids = as.character(unlist(tmpids))
  #data_analyzedgenes = c("a","b","c")
  #
  #tmpynames = data_analyzed$data_long%>%select(-unique_id,-sampleid,-group)%>%colnames()
  updateSelectizeInput(session,'pepsel_id',
                       choices= data_analyzedids,
                       server=TRUE)

})

output$pep_ExperimentControls <- renderUI({
  data_analyzed = analyzeDataReactive()
  df_full = data_analyzed$df_full
  pep_experiments = unique(df_full$ExperimentID)
  checkboxGroupInput("pep_experiments", "Choose Experiments to show", pep_experiments, selected = pep_experiments)
})


output$pep_ConditionControls <- renderUI({
  data_analyzed = analyzeDataReactive()
  df_full = data_analyzed$df_full
  pep_conditions = unique(df_full$condition)
  checkboxGroupInput("pep_conditions", "Choose Conditions to show", pep_conditions, selected = pep_conditions)
})


#df_full <- eventReactive(input$go, {
df_full <- reactive({
#pepplotInput <- reactive({

  data_analyzed = analyzeDataReactive()
  df_full = data_analyzed$df_full
  print("names")
  print(names(df_full))
  ids = data_analyzed$ids
  print("ids")
  print(head(ids))
  df_full = df_full %>% filter(ExperimentID %in% input$pep_experiments)
  
  if(!is.null(input$pep_conditions)){
  print("filtering by condition")
  df_full = df_full %>% filter(condition %in% input$pep_conditions)
  }
  print(df_full)

 })

#print("server-peplines")

pepplotInput <- reactive({
#observe(input$go, {


  validate(need(length(input$pepsel_id) == 1,"Please select one gene, and then wait."))

 #CDM not sure what this if for# if (names(dev.cur()) != "null device") dev.off()
  #pdf(NULL)
  p1=peplineplot_fun(df_full = df_full(),
                    ids = ids,
                    pepsel_id=input$pepsel_id,
                    fill=input$pep_fill,
                    size=input$pep_linewidth,
                    scale = input$pep_lineoverlap,
                    min_graphic_details = input$pep_min_graphic_details,
                    condition=input$pep_condition,
                    facets=input$pep_facets,
                    pepnormscale=input$pep_height_normalization,
                    pepnormgroup=input$pep_group_normalization
             )
  #ggsave(p, filename="test2.png")
#  plot(p)
#}) #renderPlot

#output$pepcovplot <- renderPlot({


  #validate(need(length(input$pepsel_id) == 1,"Please select one gene."))

  #data_analyzed = analyzeDataReactive()
  #df_full = data_analyzed$df_full
  #print(names(df_full))
  #ids = data_analyzed$ids
  #print(head(ids))
  #CDM not sure what this if for# if (names(dev.cur()) != "null device") dev.off()
  #pdf(NULL)
  p2=pepcovplot_fun(df_full = df_full(),
                   ids = ids,
                   pepsel_id=input$pepsel_id,
                   min_graphic_details = input$pep_min_graphic_details,
                   pubtheme=input$pep_pubtheme,
                   axis_tick_spacing = input$pep_legend_tick_spacing
              )
  #ggsave(p, filename="test2.png")
  #Alignment axes make not much sense because of coord_flip
  #fudging for scales
 
  plot_grid(p1, p2, rel_widths =c(1, input$pep_plotratio), align = "h", axis = "tblr")
})


observe({
output$peplineplot <- renderPlot({

   print(pepplotInput())
},  width = input$pep_plotwidth, height = input$pep_plotheight)
})

DataLineplotReactive <- reactive({
  print("DataLineplotReactive")
  data_analyzed = analyzeDataReactive()

  subdat = peplineplot_dat(df_full = data_analyzed$df_full, ids = data_analyzed$ids,
                       pepsel_id=input$pepsel_id)
  return(subdat)
})

output$dat_peplineplot <- DT::renderDataTable({
  tmpdat = DataLineplotReactive()
  tmpdat[,sapply(tmpdat,is.numeric)] <- signif(tmpdat[,sapply(tmpdat,is.numeric)],3)
  DT::datatable(tmpdat)
}
)

output$downloadSubsetData <- downloadHandler(
  filename = c('peplineplot_data.csv'),
  content = function(file) {write.csv(DataLineplotReactive(), file, row.names=FALSE)}
)

output$downloadPepPlot <- downloadHandler(
    filename = function() { paste('peplineplot_', paste(input$pepsel_id, collapse="_"), '.', input$device, sep='') },
    content = function(file) {
        #ggsave(file, plot = plotInput(), device = "pdf")
        #file.copy(paste('peplineplot_', paste(input$pepsel_id, collapse="_"), '.pdf', sep='') , file, overwrite=TRUE)
        ggsave(file, plot = pepplotInput(), device = input$device, width=input$pep_trueplotwidth, height=input$pep_trueplotheight, units='in')
    }
)

