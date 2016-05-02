# Guillaume Lobet - University of Liege


library(shiny)
library(shinyFiles)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("--| CellSet to PAGER |--")),
  
  fluidRow(
    column(3, wellPanel(
      helpText("Small app to transform data coming from CellSet into a format the PAGER will understand"),

      h3("1. Where are your xlsx files"),
      tags$hr(),      
      
      
      textInput('dirPath', "Enter the name of folder containing the reporter xlsx files", placeholder = "Select folder"),
      tags$hr(), 
      
      h3("2. Where to save the data file?"),
      
      textInput('dirSave', "Enter the name of folder where to save the data file", placeholder = "Select folder"),

      actionButton(inputId = "load_data", label="Load and transform your data"),
      
      tags$hr(), 
      
      h4(textOutput("results"))
      
      
      ))  
    )
))