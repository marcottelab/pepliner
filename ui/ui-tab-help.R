tabPanel("Instructions",
         fluidRow(
           column(4,wellPanel(
             h4("Instructions"),
             a("Input Data", href = "#inputdata"), br(),
             a("Data Formats", href = "#dataformat"), br(),
             a("Save Data For Future Sessions", href="#rdata"), br(),
             a("Visualizations", href="#vis"), br()
           )
           ),#column
           column(8,
                  includeMarkdown("instructions/Instructions.md"))
         ))
