
library(shiny)
library(visNetwork)
library(tidyverse)
library(assertthat)



import::from("R/variables-bus-matrix.R", .all = TRUE)

my_files <- c("autowrangle/Test Survey - 2019.csv",
              "autowrangle/Test Survey - 2020.csv",
              "autowrangle/Test Survey - 2021.csv")

my_data <- set_names(my_files,
                     basename(my_files)) |> 
  
  map(read_csv) 


my_bus_network <- generate_variable_bus_matrix(my_data)



# TODO: adapt the bus_network to more machine-friendly format for this.



d_columns <- my_files |> set_names() |> map(\(x){suppressMessages(read_csv(x))}) |> 
  
  fetch_columns()


my_bus_network_long <- my_bus_network |> 
  
  pivot_longer(names_to = "source",
               values_to = "col_id",
               starts_with("Test Survey")) |> 
  
  drop_na() |> 
  
  # NEW! Logic to start guessing, so you have a starting point:
  mutate(target_column = columns) |> 
  
  group_by(target_column) |> 
  
  mutate(
    group_id = cur_group_id(),
    average_col_id = mean(col_id, na.rm = TRUE),
    estimated_col_sorting = average_col_id + 0.1*rank(group_id)
  ) |> 
  
  ungroup() |> 
  
  arrange(estimated_col_sorting)



all_nodes <- bind_rows(
    
  my_bus_network_long |> 
    select(source, col_name = columns) |> 
    distinct(),
    
  my_bus_network_long |> 
    distinct(col_name = target_column) |> 
      mutate(source = "cleaned")
  
  ) |> 
  
  
  # TODO: better approach to arranging sources
  arrange(source) |> 
  group_by(source) |> 
  mutate(vertical_order = cur_group_id()) |> 
  ungroup() |> 
  
  # TODO: make sure arranging columns works as expected
  left_join(
    my_bus_network_long |> 
      select(columns, source, average_col_id),
    by = c("col_name" = "columns",
           "source")
  ) |> 
  rename(horizontal_order = average_col_id) |> 
  
  # TODO: build logic for placing the "cleaned" versions
  replace_na(list(horizontal_order = 1)) |> 
  
  # Make sure we have an ID to distinguish source-column combos:
  mutate(id = 1:n())
  

all_edges <- my_bus_network_long |> 
  
  left_join(
    all_nodes |> 
      select(source, 
             col_name, 
             source_id = id),
    by = c("source", 
           "columns" = "col_name")
  ) |> 
  
  left_join(
    all_nodes |> 
      filter(source == "cleaned") |> 
      select(col_name, 
             target_id = id),
    by = c("target_column" = "col_name")
  ) |> 
  
  select(source_id, target_id)

  

# TODO NEXT: arrange the graph like we want, lock down the layout, and off we go


server <- function(input, output) {
  
  my_nodes <- reactive({
    
    all_nodes |> 
      rename(
        label = col_name,
        # level = vertical_order,
        x = horizontal_order,
        y = vertical_order
      ) |> 
      
      # All of these options are documented in visNodes()
      
      mutate(across(c(x,y), ~(. * 100))) |> 
      
      
      # # lock movement in the vertical plane - manual "levels"
      # mutate(
      #   fixed = map(1:n(), \(x) list("y" = TRUE, "x" = FALSE))
      # ) |> 
    
      identity()
    
  })
  
  my_edges <- reactive({
    
    all_edges |> 
      rename(
        from = source_id,
        to = target_id
      )
    
  })
  
  output$autowrangle_network <- renderVisNetwork({

    visNetwork(my_nodes(), my_edges()) |> 
      
      visOptions(manipulation = TRUE) |> 
      
      visIgraphLayout() |> # For manual x / y placement
      
      visNodes(
        
        # Can just do this here rather than in the my_nodes() as we want it
        # blanket configured like this for all nodes.
        fixed = list("y" = TRUE, "x" = FALSE)
        
      ) |>
      
      visInteraction(hover = TRUE)

  })
  
  observe({
    
    visNetworkProxy("autowrangle_network") |> 
      visNodes(color = input$color)
    
  })
  
}

ui <- fluidPage(
  
  sidebarPanel(
    
    selectInput(
      "color", 
      "Color :",
      c("blue", "red", "green")
    )
    
  ),
  
  mainPanel(
    visNetworkOutput("autowrangle_network", height = "800px")
  )
  
)

shinyApp(ui = ui, server = server)
