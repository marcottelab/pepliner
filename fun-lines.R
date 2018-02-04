lineplot_dat <- function(df_full, ids, sel_id){
   print("lines_dat")
   df_sel <- df_full %>% filter(ID == sel_id)
   return(df_sel)
}

lineplot_fun <- function(df_full, 
                      ids, sel_id){
   print("lines_fun")
   print(names(df_full))
   df_sel <- df_full %>% filter(ID == sel_id)
   print(head(df_sel))
   

  lines <- ggplot(data = df_sel, aes(x = FractionID, y = fct_reorder(Peptide, desc(Start)), height = PeptideCount, group = Peptide)) +#, fill=Condition)) +
    #geom_ridgeline(stat = "identity", alpha = 0.75, scale = 0.9, fill = "#E69F00", size = 0.3) +
    geom_ridgeline(stat = "identity", alpha = 0.75, scale = 0.9, fill="white", size = 0.3) +
    ylab("Peptide") +
    scale_y_discrete(c(0,1), expand = c(0, 0)) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position = "none") +


    #facet_wrap(~ExperimentNum + Compartment + Column, nrow = 1, scales = "free_x") +
    theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size=8, margin = c(0.0)),
    axis.text.x= element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()

    ) 


  #g <- ggplotly(lines)
  #ggsave(cov, filename = "testing.png")
  return(lines)
  #g <- ggplotly()
  #htmlwidgets::saveWidget(g, "test.html")
  #return(g)
  #g %>% layout()   
  
}
