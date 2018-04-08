## ==================================================================================== ##
# Pepliner: App for visualizing protein elution data
# Modified 2018 from the original GNUpl3 by Claire D. McWhitei <claire.mcwhite@utexas.edu>

# Original Copyright (C) 2016 Jessica Minnier, START Shiny Transcriptome Analysis Tool
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
## ==================================================================================== ##
## 
## 
# ui.R
source("helpers.R")
customHeaderPanel <- function(title,windowTitle=title){
  tagList(
    tags$head(
      tags$title(windowTitle),
      tags$link(rel="stylesheet", type="text/css",
                href="app.css"),
      tags$h1(a(href="http://www.ohsu.edu/xd/health/services/heart-vascular/"))
    )
  )
}

# collects all of the tab UIs

tagList(
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: red; } ")),
    tags$style(".mybuttonclass{background-color:#BF5700;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}")
  ),
  navbarPage(
    
    theme = "bootstrap.min.united.updated.css",
    #United theme from http://bootswatch.com/
    title = "Pepliner: Protein elution viewer",
    source("ui/ui-tab-landing.R",local=TRUE)$value,
    ## =========================================================================== ##
    ## DOWNLOAD DATA TABS
    ## =========================================================================== ##
    source("ui/ui-tab-inputdata.R",local=TRUE)$value,
    ## =========================================================================== ##
    ## Visualization TABS
    ## =========================================================================== ##
    source("ui/ui-tab-peplines.R",local=TRUE)$value,

    source("ui/ui-tab-protlines.R",local=TRUE)$value,
    source("ui/ui-tab-help.R",local=TRUE)$value,
    source("ui/ui-tab-news.R",local=TRUE)$value,
    source("ui/ui-tab-terms.R",local=TRUE)$value,
    #end definitions of tabs, now footer
    ## ============================================================================ ##
    ## INFO TAB
    ## ============================================================================ ##   
    
    ## ==================================================================================== ##
    ## FOOTER
    ## ==================================================================================== ##              
    footer=p(hr(),p(("ShinyApp created by Claire McWhite of UT Austin"),align="center", width=4),
             #p(("Code available on Github:"),a("https://github.com/marcottelab/pepliner",href="https://github.com/marcottelab/pepliner"),align="center",width=4),
              p(("modified from START app by {Jessica Minnier + Jiri Sklenar + Jonathan Nelson} of "),align="center",width=4),
             p(("Knight Cardiovascular Institute, Oregon Health & Science University"),align="center",width=4),
             p(("Copyright (C) 2016, code licensed under GPLv3"),align="center",width=4),
             p(("Original code available on Github:"),a("https://github.com/jminnier/STARTapp",href="https://github.com/jminnier/STARTapp"),align="center",width=4),
             p(a("Nelson JW, Sklenar J, Barnes AP, Minnier J. (2016) `The START App: A Web-Based RNAseq Analysis and Visualization Resource.` Bioinformatics.  doi: 10.1093/bioinformatics/btw624.",href="http://bioinformatics.oxfordjournals.org/content/early/2016/09/27/bioinformatics.btw624.abstract"),align="center",width=4)
    )
    
    ## ==================================================================================== ##
    ## end
    ## ==================================================================================== ## 
    #Not going online
    #tags$head(includeScript("google-analytics.js"))
  ) #end navbarpage
) #end taglist

