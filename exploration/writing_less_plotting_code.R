
# TODO: think about interactivity with ggplot in e.g. Shiny


# I do love plot_ly but it would be *super* nice to be able to write less
# bespoke plotting code - ideally, write it once for both static and interactive
# contexts (i.e. - and yes, that's an i.e., not an e.g., let's be honest -
# Powerpoint and Shiny respectively) and just a *little* adaptation code.

# Could plotly::ggplotly be a reliable 80%-of-the-way-there option?

library(...) # Load a custom theme library - e.g. Grattan Institute's


p <- mtcars |> 
  
  as_tibble() |> 
  
  ggplot(aes(x = cyl, y = wt)) +
  
  geom_point() +
  
  geom_hline(yintercept = 3)



plotly::ggplotly(p) |> 
  
  # https://plotly.com/r/creating-and-updating-figures/#updating-traces
  plotly::style(

    name = "",
    
    # This approach seemed finicky vis-a-vis using the
    #   text = ... 
    #   hoverinfo = 'text' 
    # approach... maybe a nuance of updating traces post hoc.
    
    hovertemplate = paste('<i>Weight</i>: $%{wt}',
                          '<br><b>Cylinder</b>: %{cyl}<br>'),
    
    # It *seems* like the plotly traces are reliably in the (zero-indexed) order
    # they come up in the ggplot code... but I'm not 100% convinced yet.
    traces = c(0)
    
  )

