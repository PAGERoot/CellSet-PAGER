
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)



shinyServer(
  function(input, output, clientData, session) {

    observe({
            
      if(input$load_data == 0){return()}


        #------------------------------------------------------
        # LOAD THE USER DATA
        #------------------------------------------------------

        # Load datafiles
        pathData <- paste0(input$dirPath, "/")
        
        if (is.null(pathData)) return(NULL)
        
        # Attach the reporter informations
        list.f <- list.files(pathData)
        rs <- NULL
        for(f in list.f){
          name <- gsub(".xlsx", "", f)
          for(i in 1:20){
            tryCatch({
              temp <- read_excel(paste0(pathData, f), sheet = i)
              temp <- temp[!is.na(temp[,1]),]
              # Normalize the fluorescence data
              fluo <- scale(temp[["Average flourescence"]])
              rs <- rbind(rs, data.frame(line=name, root=i, cell_type=temp$Label, value=fluo))
            },warning = function(w) {
            }, error = function(e) {
            })
          }
        }
        remove(temp, i, f, list.f, name)
        
        write.csv(rs, paste0(input$dirSave, "/reporter-data.csv"))
        
    })
    
    
    output$results <- renderText({ 
      if(input$load_data == 0){return()}
      "Conversion done"
    })
})