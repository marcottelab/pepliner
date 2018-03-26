
pepplot_fun <- function(df_full, 
                      ids, sel_id){
   print("pepplot_fun")
   print(names(df_full))
   df_sel <- df_full %>% filter(ID == sel_id)
   print(head(df_sel))
   cov <- ggplot(data = df_sel, aes(x = fct_reorder(Peptide, desc(Start)))
) +
  geom_boxplot(stat = "identity", fill="black", aes(ymin = Start, ymax = End, lower =
 Start, upper = End, middle = Start), size = 0) +
  theme_ridges() +
  scale_x_discrete(position = "top") + #Really y right
  coord_flip()  +
  background_grid(major = "xy", minor = "none", size.major = 0.4) + # Really x
  scale_y_continuous(breaks = seq(0, 100000, 100), expand = c(0,0)) + # Really x
  expand_limits(y = 0) + #Really x
  theme(axis.text.y = element_text(size = 6), # Really y
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
        #axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  #ggsave(cov, filename = "testing.png")
  return(cov)
  #g <- ggplotly()
  #htmlwidgets::saveWidget(g, "test.html")
  #return(g)
  #g %>% layout()   
  
}
