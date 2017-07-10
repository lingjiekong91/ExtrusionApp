#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
#Change the search bar size to show the entire cell.


#Creating variables across all sessions
path <- "C:/Users/kongl5/Desktop/Shiny/ExtrusionApp/Extrusion Application/PPS_document_contents/ListOfSheets/GTE"
single_pps_file <- "Single PPS Data Filled GTE.csv"
single_tari_file <- "Single Tari Data.csv"
multi_pps_file <- "Multi-Layered PPS Data Filled GTE.csv"
tapered_pps_file <- "Tapered PPS Data Filled GTE.csv"
resin_file <- "Resin Information.csv"
screw_file <- "Screw Properties.csv"

single_pps_pathfile <- paste(path, single_pps_file, sep = "/")
single_tari_pathfile <- paste(path, single_tari_file, sep = "/")
multi_pps_pathfile <- paste(path, multi_pps_file, sep = "/")
tapered_pps_pathfile <- paste(path, tapered_pps_file, sep = "/")
resin_pathfile <- paste(path, resin_file, sep = "/")
screw_pathfile <- paste(path, screw_file, sep = "/")

single_pps_data <- read.csv(single_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                            check.names = FALSE)
single_tari_data <- read.csv(single_tari_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
multi_pps_data <- read.csv(multi_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                           check.names = FALSE)
tapered_pps_data <- read.csv(tapered_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
resin_data <- read.csv(resin_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
screw_data <- read.csv(screw_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)


# Define UI for application that draws a histogram


server <- function(input, output, session) {
  
  e1 <- new.env(
    #variables to contain:
    # vectorofAttributes - contains the attributes that were previously selected by the user
  ) #creates a new environment to store instance variables
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(single_pps_data[, input$show_vars1], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(multi_pps_data[, input$show_vars2], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(tapered_pps_data[, input$show_vars3], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(single_tari_data[, input$show_vars4], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(resin_data[, input$show_vars5], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(screw_data[, input$show_vars6], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)

