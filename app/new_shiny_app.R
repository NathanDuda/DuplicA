library(shiny)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#007bc2", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the head of the data ----
  output$table <- renderTable({
    head(data.frame(x = d()))
  })
  
}

# Build the Shiny app using the HTML template ----
shinyApp(ui = htmlTemplate("www/index.html"), server)
