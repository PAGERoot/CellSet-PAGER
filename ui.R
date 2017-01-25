# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.

# This is the user interface of the Shiny app to transform CellSet output files (XLS)
# into one single RSML file that can be read by PAGE-Root

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("--| CellSet to PAGE-Root |--"),
  
  fluidRow(
    column(3, 
      helpText("Transform data coming from CellSet into a single RSML file"),
        wellPanel(
        h4("1. Where are your xlsx files"),
        textInput('dirPath', "Enter the name of folder containing the reporter xlsx files", f1, placeholder = "Select folder"),
        actionButton(inputId = "load_data", label="Load your data",icon("upload"))
        )
      ),
    
    column(3,
           h4("3. View your data"),
           tableOutput('table_results'),
           tags$hr(),
           h5("Lines in the file"),
           textOutput("lines"),
           tags$hr(),
           textOutput("obs")
           
    ), 
  
    column(3,wellPanel(
      h4("4. Set your cell types"),
      selectInput("type1", label = cell_types[1], choices = "Load files"),
      selectInput("type2", label = cell_types[2], choices = "Load files"),
      selectInput("type3", label = cell_types[3], choices = "Load files"),
      selectInput("type4", label = cell_types[4], choices = "Load files"),
      selectInput("type5", label = cell_types[5], choices = "Load files"),
      selectInput("type6", label = cell_types[6], choices = "Load files"),
      selectInput("type7", label = cell_types[7], choices = "Load files"),
      h4(textOutput("more")),
      actionButton(inputId = "update_data", label="Update your data")
      
    )),
    
    column(3, 
           div(htmlOutput("check"), style="color:green"),
           div(htmlOutput("notcheck"), style="color:red"),
           tags$hr(),
           wellPanel(
             h4("5. Save the data file?"),
             downloadButton("save_data", "Download RSML")
           )
      
      )
    

    
    )
))