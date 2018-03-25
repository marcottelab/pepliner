## ==================================================================================== ##
# Pepliner: App for visualizing protein elution data
# Modified 2018 from the original GNUpl3 by Claire D. McWhite <claire.mcwhite@utexas.edu>
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


tabPanel("Terms & Conditions",
         fluidRow(
           column(4,wellPanel(
             h4("Shinyapps.io Terms & Conditions")
           )
           ),#column
           column(8,
                  includeMarkdown("instructions/terms.md"))
         ))
