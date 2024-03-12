

gg_left_align_titles <- function(...){
  
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
  
}


gg_legend_fill <- function(.title,
                           ...){
  
  guides(
    fill = guide_legend(title = .title)
  )
  
}


mtcars |>

  ggplot(
    aes(
      x = cyl,
      y = disp,
      fill = wt
    )
  ) +

  geom_col() +

  gg_legend_fill("Wubba") +
  
  labs(title = "Test") +
  gg_left_align_titles()
  
