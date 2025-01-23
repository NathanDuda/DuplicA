

source(paste0(here_duplica, '/app/Scripts/Visualization_functions.R'))


# run multispecies workflow 
main_run_workflow(selected_models, input) 

# read in all results from the (default input) Results directory
all_data <- get_all_results()

# get options for visualization page 
vis_options <- get_visualization_button_options_lists(all_data)

x = vis_options$x_and_y_axis_options
y = vis_options$x_and_y_axis_options

color_groups = vis_options$group_options
separate_figure = vis_options$group_options


# all other visualization options are CONSTANT
figure_type = c('scatterplot', 'boxplot', 'violin') # default: 'scatterplot'
custom_theme = c('classic', 'bw', 'minimal', 'linedraw') # default: 'classic'
color_set = c('Spectral', 'Accent', 'Pastel1', 'Paired', 'Dark2') # default: 'Spectral'


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
  x = "PS_dup_1",
  y = "N_dup_1",
  color_groups = NULL, 
  separate_figure = NULL,
  
  title = "title title",
  x_label = 'x label',
  y_label = 'y label',
  legend_label = 'legend label',
  point_size = 2,
  
  custom_theme = 'bw', 
  y_log = F,
  x_log = F,
  color_set = 'Accent',
)


