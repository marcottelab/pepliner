protlineplot_dat <- function(df_norm_prot, ids, protsel_id){
   print("protlines_dat")
   df_sel <- df_norm_prot %>% filter(ID %in% protsel_id)
   #print(names(df_sel))
   return(df_sel)
}

protlineplot_fun <- function(df_norm_prot, 
                             ids,
                             protsel_id,
                             fill="#E69F00FF",
                             size = 0.3,
                             scale = 0.9,
                             min_graphic_details = FALSE,
                             condition = FALSE,
                             facets = NULL
                             )
{
   print("lines_fun")
   #print(names(df_norm_prot))
   #print(protsel_id)
   df_sel <- df_norm_prot %>% filter(ID %in% protsel_id)
   

   protlines <- ggplot(data = df_sel, aes(x = FractionID, y = ID, height = ProteinCount)) +#, fill=Condition)) +
    #geom_ridgeline(stat = "identity", alpha = 0.75, scale = 0.9, fill="white", size = 0.3) +
    #scale_y_discrete(c(0,1), expand = c(0, 0)) +
    #scale_x_continuous(breaks = seq(0, 10000, 20), expand = c(0,0)) + 

    #facet_wrap(~ExperimentNum + Compartment + Column, nrow = 1, scales = "free_x") +
    theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size=8, margin = c(0.0)),
    axis.text.x= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#,
    #legend.position = "none"
    ) 

  if(condition == TRUE){
      protlines <- protlines + geom_ridgeline(stat = "identity", scale = scale, size = size, alpha = 0, aes(color = condition, group=paste(condition, ID))) 
  }
  else{
      protlines <- protlines + geom_ridgeline(stat = "identity", scale = scale, fill = fill, size = size, aes(group=ID)) 
  }



  if(!is.null(facets)){
  
      facet_str <- paste(facets, collapse = '+')
      facet_str <- paste("~", facet_str, sep="")
      #Final construct needs to be ~A + B + C + D.
      #If only one facet variable, will get  A

      #Facet_grid takes a string. Facet_wrap does not. 
      protlines <- protlines + facet_grid(facet_str, scales = "free_x") 

  } 

  if(min_graphic_details == TRUE){
      protlines <- protlines + theme_nothing()
 
  }



  #g <- ggplotly(lines)
  #ggsave(cov, filename = "testing.png")
  return(protlines)
  #g <- ggplotly()
  #htmlwidgets::saveWidget(g, "test.html")
  #return(g)
  #g %>% layout()   
  
}
