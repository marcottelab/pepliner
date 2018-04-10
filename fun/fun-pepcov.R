
pepcovplot_fun <- function(df_full, 
                           ids, 
                           pepsel_id,
                           min_graphic_details=NULL,
                           pubtheme=NULL,
                           axis_tick_spacing=100
                          ){
   print("pepplot_fun")
   print(names(df_full))
   df_sel <- df_full %>% filter(ID == pepsel_id)
   print(head(df_sel))
   pepcov <- ggplot(data = df_sel, aes(x = fct_reorder(Peptide, desc(Start)))) +
          geom_boxplot(stat = "identity", fill="black", aes(ymin = Start, ymax = End, lower = Start, upper = End, middle = Start), size = 0) +
          theme_ridges() +
          scale_x_discrete(position = "top") + #Really y right
          coord_flip()  +
          background_grid(major = "xy", minor = "none", size.major = 0.4) + # Really x
          scale_y_continuous(breaks = seq(0, 100000, axis_tick_spacing)) + #, expand = c(0,0)) + # Really x
          expand_limits(y = 0) + #Really x
        

  theme(axis.text.y = element_text(size = 6), # Really y
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_blank()) +
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

  if(min_graphic_details == TRUE){
      pepcov <- pepcov + theme_nothing()

  } 

  if(pubtheme == TRUE){
      pepcov <- pepcov + theme(axis.text.y = element_blank(), # Really y
                #axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                #axis.ticks.x = element_blank(),
                legend.position = 'none') +
                background_grid(major = "x", minor = "none", size.major = 0.4, colour.major="black")  # Really x
 
  } 

 


  #ggsave(cov, filename = "testing.png")
  return(pepcov)
  #g <- ggplotly()
  #htmlwidgets::saveWidget(g, "test.html")
  #return(g)
  #g %>% layout()   
  
}
