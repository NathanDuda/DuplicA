
curDir <- getwd()
curDir <- dirname(curDir)
#curDir <- dirname(curDir)
source(paste0(curDir, '/app/Scripts/Visualization_functions.R'))

here_results <- paste0(curDir, "/app/Results")

#all_data <- get_all_results(here_results)
#all_data <- all_data %>%
#  mutate(Functional_Category = gsub('pseudo_dup_1', 'Pseudo(Dup1)', Functional_Category),
#         Functional_Category = gsub('pseudo_dup_2', 'Pseudo(Dup2)', Functional_Category),
#         Functional_Category = gsub('pseudo_both', 'Pseudo(both)', Functional_Category))









# main function to make the figure 
generate_figure <- function(data,
                            
                            figure_type = NULL,
                            x = NULL,
                            y = NULL,
                            color_groups = NULL,
                            separate_figure = NULL,
                            
                            title = NULL,
                            x_label = NULL,
                            y_label = NULL,
                            legend_label = NULL,
                            point_size = 3,
                            
                            custom_theme = NULL,
                            x_log = FALSE,
                            y_log = FALSE,
                            color_set = NULL
) {
  
  
  # test for proper inputs 
  if (x_log && !is.numeric(data[[x]])) {
    stop("Log scale can only be applied to numeric variables for x.")
  }
  if (y_log && !is.numeric(data[[y]])) {
    stop("Log scale can only be applied to numeric variables for y.")
  }
  
  # Ensure plot type matches the variable types
  if (figure_type == "boxplot" | figure_type == 'violin') {
    if (!is.factor(data[[x]]) && !is.character(data[[x]]) &&
        !is.factor(data[[y]]) && !is.character(data[[y]])) {
      stop("Boxplot requires at least one of x or y to be categorical.")
    }
  }
  
  # remove NA values for x and y
  data <- data %>% filter(if_any(c(x, y), ~ !is.na(.)))
  if (!is.null(color_groups)) {data <- data %>% filter(if_any(c(color_groups), ~ !is.na(.)))}
  if (!is.null(separate_figure)) {data <- data %>% filter(if_any(c(separate_figure), ~ !is.na(.)))}
  
  # start building the plot
  p <- ggplot(data, aes_string(x = x, y = y, color = color_groups))
  
  # add plot type
  if (figure_type == "scatterplot") {p <- p + geom_point(size = point_size)}
  if (figure_type == "boxplot") {p <- p + geom_boxplot()}
  if (figure_type == "violin") {p <- p + geom_violin()}
  
  # apply log scaling if chosen
  if (x_log) {p <- p + scale_x_log10()}
  if (y_log) {p <- p + scale_y_log10()}
  
  # add custom color set 
  if (!is.null(color_set)) {
    p <- p + scale_color_manual(values = brewer.pal(length(unique(data[[color_groups]])), color_set)
    )
  }
  
  # add faceting if chosen
  if (!is.null(separate_figure)) {
    p <- p + facet_wrap(as.formula(paste("~", separate_figure)))
  }
  
  # add custom labels
  if (!is.null(title)) {p <- p + ggtitle(title)}
  if (!is.null(x_label)) {p <- p + xlab(x_label)}
  if (!is.null(y_label)) {p <- p + ylab(y_label)}
  
  # add custom theme
  if(is.null(custom_theme)) {p <- p + theme_classic()}
  if(!is.null(custom_theme)) {
    if(custom_theme == 'classic') {p <- p + theme_classic()}
    if(custom_theme == 'bw') {p <- p + theme_bw()}
    if(custom_theme == 'minimal') {p <- p + theme_minimal()}
    if(custom_theme == 'linedraw') {p <- p + theme_linedraw()}
  }
  
  ggsave(p, filename = paste0(here_duplica, '/app/Front_end/vite-project/public/Figure.png'), height = 5, width = 7, dpi = 400)
  
  return(p)
}







# example usage




















# every time a parameter is changed by the user, rerun the function
main_generate_figure <- function(parameters) {
  generate_figure(
    data = all_data,
    figure_type = parameters$figure_type,
    x = parameters$x_axis,
    y = parameters$y_axis,
    color_groups = parameters$color_groups,
    separate_figure = parameters$separate_plot,
    title = parameters$title,
    x_label = parameters$x_axis_label,
    y_label = parameters$y_axis_label,
    custom_theme = parameters$custom_theme, 
    y_log = parameters$log_scale_y,
    x_log = parameters$log_scale_x,
    color_set = parameters$color_set,
    point_size = parameters$point_size
  )
}




