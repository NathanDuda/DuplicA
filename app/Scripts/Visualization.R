

source(paste0(here_duplica, '/app/Scripts/Visualization_functions.R'))


# every time an input is changed by the user, rerun the function
generate_figure(
  data = all_data,
  figure_type = input$figure_type,
  x = input$x_axis,
  y = input$y_axis,
  separate_plot = input$separate_plot,
  title = input$title,
  x_label = input$x_label,
  y_label = input$y_label,
  custom_theme = input$custom_theme, 
  y_log = input$y_log,
  x_log = input$x_log,
  color_set = input$color_set,
  point_size = input$point_size
)




# example usage
generate_figure(
  data = all_data,
  
  figure_type = "scatterplot",
  x = "cpg_count_dup_1",
  y = "cpg_count_dup_2",
  color_groups = 'Functional_Category', 
  separate_figure = NULL,
  
  title = "title title",
  x_label = NULL,
  y_label = NULL,
  legend_label = NULL,
  point_size = 2,
  
  custom_theme = 'bw', 
  y_log = T,
  x_log = T,
  color_set = 'Accent'
)




