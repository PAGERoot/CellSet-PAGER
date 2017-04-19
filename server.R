#
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


# This is the server side of the Shiny app to transform CellSet output files (XLS)
# into one single RSML file that can be read by PAGE-Root



library(shiny)

shinyServer(
  
  function(input, output, clientData, session) {  
    
    dataset <- reactiveValues(rep = NULL, raw = NULL)
    
    # For the root paramerers
    observe({
      if(is.null(dataset$rep)){return()}
      
      dt <- dataset$rep
      ts <- as.character(unique(dt$cell_type))
      message(ts)
      
      for(i in c(1:length(cell_types))){
        updateSelectInput(session, paste0("type",i), choices = ts, selected = ts[pmatch(substr(cell_types[i], 0, 3), ts)])
      }
      
    })
    # 
    observe({
      if(is.null(dataset$raw)){return()}

      rs <- dataset$raw

      # rs$id <- rep(c(1:input$tokeep), ceiling(nrow(rs)/input$tokeep))[1:nrow(rs)]
      #
      # if(input$method == "Normalize") rs$value <- ddply(rs, .(line, root), summarize, value=range01(value))$value
      # if(input$method == "Standardize") rs$value <- ddply(rs, .(line, root), summarize, value=scale(value))$value
      #
      # if(input$method2 == "Mean") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=mean(value))
      # if(input$method2 == "Median") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=median(value))
      # if(input$method2 == "Min") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=min(value))
      # if(input$method2 == "Max") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=max(value))

      dataset$rep <- rs

    })
    #     
    
    observeEvent(input$load_data, {
            
        #------------------------------------------------------
        # LOAD THE USER DATA
        #------------------------------------------------------

        # Load datafiles
        pathData <- paste0(input$dirPath, "/")
        
        # if (is.null(pathData)) return(NULL)

        # Attach the reporter informations
        # pathData <- "../test_data/small_dataset/"
        list.f <- list.files(pathData)
        nfiles <<- length(list.f)
        
        withProgress(message = 'Loading data', {
          rs <- NULL
          j <- 1
          for(f in list.f){
            j <- j+1
            incProgress(j/length(list.f), detail = paste0("Loading the file ",f))
            name <- gsub(".xlsx", "", f)
            for(i in 1:20){
              tryCatch({
                temp <- read_excel(paste0(pathData, f), sheet = i)
                temp <- temp[!is.na(temp[,1]),]
                
                # Normalize the fluorescence data (if required)
                
                rs <- rbind(rs, data.frame(line=name, root=i, cell_type=temp$Label, value=temp[["Average flourescence"]]))
              },warning = function(w) {
              }, error = function(e) {
              })
            }
          }
        })
        

        remove(temp, i, f, list.f, name)
        rs <- rs[!is.na(rs$value),]
        
        #log2 transofmr the data
        # if(input$log2) rs$value <- log2(rs$value)
        
        dataset$raw <- rs
        

        # rs$id <- rep(c(1:input$tokeep), ceiling(nrow(rs)/input$tokeep))[1:nrow(rs)]
        # 
        # if(input$method == "Normalize") rs$value <- ddply(rs, .(line, root), summarize, value=range01(value))$value
        # if(input$method == "Standardize") rs$value <- ddply(rs, .(line, root), summarize, value=scale(value))$value
        # 
        # if(input$method2 == "Mean") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=mean(value))
        # if(input$method2 == "Median") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=median(value))
        # if(input$method2 == "Min") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=min(value))
        # if(input$method2 == "Max") rs <- ddply(rs, .(line, root, cell_type, id), summarize, value=max(value))

        dataset$rep <- rs
        
    })
        
    observeEvent(input$update_data, {
      rs <- dataset$raw
      rs$cell_type[rs$cell_type == input$type1] <- cell_types[1]
      rs$cell_type[rs$cell_type == input$type2] <- cell_types[2]
      rs$cell_type[rs$cell_type == input$type3] <- cell_types[3]
      rs$cell_type[rs$cell_type == input$type4] <- cell_types[4]
      rs$cell_type[rs$cell_type == input$type5] <- cell_types[5]
      rs$cell_type[rs$cell_type == input$type6] <- cell_types[6]
      rs$cell_type[rs$cell_type == input$type7] <- cell_types[7]
      dataset$raw <- rs
    })
    
    # ----------------------------------------------------------------
    # Downlaod the data
    # ----------------------------------------------------------------
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$save_data <- downloadHandler(
      

      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        if(is.null(dataset$rep)) return()
        if(length(unique(dataset$rep$cell_type)) != length(cell_types)) return()
        "reporter.rsml"
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        
        withProgress(message = 'Saving data', {
          
          rs <- dataset$rep
          
          message(unique(rs$cell_type))
          
          rs$cell_type[rs$cell_type == input$type1] <- cell_types[1]
          rs$cell_type[rs$cell_type == input$type2] <- cell_types[2]
          rs$cell_type[rs$cell_type == input$type3] <- cell_types[3]
          rs$cell_type[rs$cell_type == input$type4] <- cell_types[4]
          rs$cell_type[rs$cell_type == input$type5] <- cell_types[5]
          rs$cell_type[rs$cell_type == input$type6] <- cell_types[6]
          rs$cell_type[rs$cell_type == input$type7] <- cell_types[7]
          
          # Write down th RSML file to store the data
          rsmlText <- "<?xml version='1.0' encoding='UTF-8'?>\n"
          rsmlText <- paste0(rsmlText, "\t<rsml xmlns:po='http://www.plantontology.org/xml-dtd/po.dtd'>\n")
          
          # Fill in the metadata
          rsmlText <- paste0(rsmlText, "\t\t<metadata>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<version>1</version>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<unit>inch</unit>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<resolution>300.0</resolution>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<last-modified>",Sys.Date(),"</last-modified>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<software>PAGE-Root</software>\n")
          rsmlText <- paste0(rsmlText, "\t\t\t<file-key>",sha1(rs),"</file-key>\n")
          
          # Get the properties (in this case, we set a property for each cell type
          rsmlText <- paste0(rsmlText, "\t\t\t<property-definitions>\n")
          for(cell in unique(rs$cell_type)){
            rsmlText <- paste0(rsmlText, "\t\t\t\t<property-definition>\n")
            rsmlText <- paste0(rsmlText, "\t\t\t\t\t<label>",cell,"</label>\n")
            rsmlText <- paste0(rsmlText, "\t\t\t\t\t<type>numeric</type>\n")
            rsmlText <- paste0(rsmlText, "\t\t\t\t\t<unit>-</unit>\n")
            rsmlText <- paste0(rsmlText, "\t\t\t\t</property-definition>\n")
          }
          
          rsmlText <- paste0(rsmlText, "\t\t\t</property-definitions>\n")
          rsmlText <- paste0(rsmlText, "\t\t</metadata>\n")
          
          # Let's start wrting the scene object, that will contain all the information fo all the roots
          rsmlText <- paste0(rsmlText, "\t\t<scene>\n")
          i <- 0
          for(l in unique(rs$line)){  
            i <- i+1
            incProgress(round(1/length(unique(rs$line)), 2), detail = paste0("Saving the line ",l))
            
            # We create one "plant" per line / genotype present in the original file
            rsmlText <- paste0(rsmlText, "\t\t\t<plant id='", i, "' label = '", l, "' >\n")
            j <- 0
            for(r in unique(rs$root[rs$line == l])){
              j <- j+1 
              # We create one root object for each root. This root object wll the contain the fluorescence data
              rsmlText <- paste0(rsmlText, "\t\t\t\t<root id='",i,"-",j,"' label='",r,"' po:accession='PO:0009005'>\n")
              rsmlText <- paste0(rsmlText, "\t\t\t\t\t<geometry></geometry>\n")
              rsmlText <- paste0(rsmlText, "\t\t\t\t\t<functions></functions>\n")
              rsmlText <- paste0(rsmlText, "\t\t\t\t\t<annotations>\n")
              temp <- rs[rs$line == l & rs$root == r, ]
              # We store the fluorescence data as annotations for the root, since they
              # are not associated with any root geometry. Here, we use one type of annotation by
              # of cell line
              for(cell in unique(temp$cell_type)){
                rsmlText <- paste0(rsmlText, "\t\t\t\t\t\t<annotation name='",cell,"'>\n")
                n <- nrow(temp[temp$cell_type == cell,])
                fluo <- paste0(rep("\t\t\t\t\t\t\t<value>", n),
                                     round(temp$value[temp$cell_type == cell], 5),
                                     rep("</value>\n", n), collapse="")
                rsmlText <- paste0(rsmlText, fluo)
                rsmlText <- paste0(rsmlText, "\t\t\t\t\t\t</annotation>\n")
              }
              rsmlText <- paste0(rsmlText, "\t\t\t\t\t</annotations>\n")
              # }
              rsmlText <- paste0(rsmlText, "\t\t\t\t</root>\n")
            }
            rsmlText <- paste0(rsmlText, "\t\t\t</plant>\n")
          }
          rsmlText <- paste0(rsmlText, "\t\t</scene>\n")
          rsmlText <- paste0(rsmlText, "\t</rsml>")
          cat(rsmlText, file="~/Desktop/test.rsml")
          
          message(paste0(nrow(rs), " observations \n", nfiles, " data files"))
          remove(temp, cell, fluo, i, j, l, n, r)
  
          cat(rsmlText, file=file)
        })
    })
        
    
    
    output$obs <- renderText({ 
      if (is.null(dataset$rep)) { return()}
      paste0(nrow(dataset$rep), " observations were uploaded")
    })
    
    output$check <- renderText({ 
      if (is.null(dataset$rep)) { return()}
      if(length(unique(dataset$rep$cell_type)) == length(cell_types)){
        "Number of cell types is OK"
      }else{
        ""
      }
    }) 
    output$notcheck <- renderText({ 
      if (is.null(dataset$rep)) { return()}
      if(length(unique(dataset$rep$cell_type)) == length(cell_types)){
        ""
      }else{
        "More cell types in the dataset than allowed. Please update your datafile"
      }
    })    
    
    
    output$lines <- renderText({ 
      if (is.null(dataset$rep)) { return()}
        paste(unique(dataset$rep$line), sep=", ", collapse =", ")
    })
    
    output$table_results <- renderTable({
      if (is.null(dataset$rep)) { return()}
      dataset$rep[1:(min(10, nrow(dataset$rep))),]
    })  
    
    
})
