
library(shiny)
library(editbl)
library(tidyverse)
library(yaml)

library(RSQLite)
library(DBI)


init_sqlite_database <- function(overwrite = FALSE){
  
  # Continuing usage
  if(!overwrite & file.exists("etl_manifest.sqlite")) return(invisible(NULL))
  
  # Force fresh set-up
  if(overwrite & file.exists("etl_manifest.sqlite")) file.remove("etl_manifest.sqlite")
  
  # (Forced fresh | first-time) set-up:
  
  file.create("etl_manifest.sqlite")
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), "etl_manifest.sqlite")
  
  on.exit(DBI::dbDisconnect(conn))
  
  # Defaults:
  
  ## File target table
  
  DBI::dbCreateTable(conn, name = "file_targets", fields = tibble(
    file_path = character(),
    reading_function = character(),
    target_dataset = character()
  ))
  
  ## ...
  
  # Return quietly
  return(invisible(TRUE))
  
}


etl_manifest <- function(){
  
  init_sqlite_database(overwrite = FALSE)
  
  # print("new DB conn")
  
  return(
    DBI::dbConnect(
      RSQLite::SQLite(),
      "etl_manifest.sqlite"
    )
  )
  
}



myUI_input <- function(id) {
  ns <- NS(id)
  tagList(
    
    actionButton(inputId = ns("dummy_btn"),
                 label = "Does nothing"),
    
    checkboxInput(inputId = ns("dummy_chx"),
                  label = "Does nothing",
                  value = FALSE)
    
  )
}

myUI_output <- function(id) {
  ns <- NS(id)
  tagList(
    
    editbl::eDTOutput(id = ns("editbl"))
    
  )
}


myServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      conn <- etl_manifest()
      
      base_data <- tbl(conn, "file_targets")
      
      modified_data <- eDT(
        id = "editbl",
        data = base_data,
        in_place = TRUE
      )
      
      
      session$onSessionEnded(function() {
        
        DBI::dbDisconnect(conn)
        
      })
      
      
      return(invisible(NULL))
      
    }
  )
}


myServer2 <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      session$onSessionEnded(function() {
        
        print("goodbye!")
        
      })
      
      return(invisible(NULL)) # Default to calling for "side effects"
      
    }
  )
}



ui <- sidebarLayout(
  
  sidebarPanel(
    myUI_input("app")
  ),
  
  mainPanel(
    myUI_output("app")
  )
  
)

server <- function(input, output, session){
  
  myServer(id = "app")
  
  myServer2(id = "app")
  
}

shinyApp(ui, server)
