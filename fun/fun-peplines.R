peplineplot_dat <- function(df_full, ids, pepsel_id){
   print("lines_dat")
   df_sel <- df_full %>% filter(ID == pepsel_id)
   return(df_sel)
}

peplineplot_fun <- function(df_full, 
                            ids, 
                            pepsel_id,
                            fill="#E69F00FF",
                            size=0.3,
                            scale = 0.9,
                            min_graphic_details = FALSE,
                            condition=FALSE,
                            facets=NULL,
                            pepnormscale="Peptide",
                            pepnormgroup="Experiment"
                           )
{

#   PepNormPeptideCount 
    print(pepnormscale)

    print(names(df_full))
    df_sel <- df_full %>% filter(ID == pepsel_id)
    print(head(df_sel))
    print("marker0")

    #These ifs can be replaced after tidyeval introduced to ggplot2

    if(pepnormscale == "None") {
       peplines <- ggplot(data = df_sel, aes(x = FractionID, y = fct_reorder(Peptide, desc(Start)), height = PeptideCount)) #, fill=Condition)) 
    }
    if(pepnormscale == "Peptide" && pepnormgroup == "Experiment"){
       peplines <- ggplot(data = df_sel, aes(x = FractionID, y = fct_reorder(Peptide, desc(Start)), height = ExpNormPeptideCount)) #, fill=Condition)) 

    }

    if(pepnormscale == "Peptide" && pepnormgroup == "None"){
       peplines <- ggplot(data = df_sel, aes(x = FractionID, y = fct_reorder(Peptide, desc(Start)), height = PepNormPeptideCount)) #, fill=Condition)) 

    }

    peplines <- peplines  + 
    #geom_ridgeline(stat = "identity", alpha = 0.75, scale = 0.9, fill = "#E69F00", size = 0.3) +
    #geom_ridgeline(stat = "identity", alpha = 0.75, scale = 0.9, fill="white", size = 0.3) +
    ylab("Peptide") +
    scale_y_discrete(c(0,1), expand = c(0, 0)) +


    #facet_wrap(~ExperimentNum + Compartment + Column, nrow = 1, scales = "free_x") +
    theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size=8, margin = c(0.0)),
    axis.text.x= element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "none"
    ) 

    print("marker1")
    print(scale)
    print(fill)
    print(size) 
    if(condition == TRUE){
      peplines <- peplines + geom_ridgeline(stat = "identity", scale = scale, size = size, alpha = 0, aes(color = condition, group=paste(condition, Peptide)))
    }
    else{
      peplines <- peplines + geom_ridgeline(stat = "identity", scale = scale, fill = fill, size =size, aes(group=Peptide))
    }

    if(!is.null(facets)){

      facet_str <- paste(facets, collapse = '+')
      facet_str <- paste("~", facet_str, sep="")
      peplines <- peplines + facet_grid(facet_str, scales = "free_x")

    }
  
    if(min_graphic_details == TRUE){
      peplines <- peplines + theme_nothing()

    }







  #g <- ggplotly(lines)
  #ggsave(cov, filename = "testing.png")
  return(peplines)
  #g <- ggplotly()
  #htmlwidgets::saveWidget(g, "test.html")
  #return(g)
  #g %>% layout()   
  
}
