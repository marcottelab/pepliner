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


options(shiny.maxRequestSize = 100*1024^2)

source("helpers.R")
print(sessionInfo())

shinyServer(function(input, output,session) {
  ## Server functions are divided by tab
  ## 
  source("server-inputdata.R",local = TRUE)
  source("server-lines.R",local = TRUE)
  source("server-protlines.R",local = TRUE)
  #source("server-heatmap.R",local = TRUE)
  #source("server-samplegroupplots.R",local=TRUE)
  #source("server-analysisres.R",local = TRUE)
  #source("server-data.R",local = TRUE)
  
})
