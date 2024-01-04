library(shiny)
library(ggplot2)
library(sn)


# UI definition
ui <- fluidPage(
  titlePanel("Bootstrap Resampling with Skewed Distributions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("skewness", "Skewness", min = -10, max = 10, value = 0),
      sliderInput("n", "Sample Size", min = 10, max = 500, value = 100),
      sliderInput("k", "Number of Bootstrap Samples", min = 100, max = 10000, value = 1000)
    ),
    mainPanel(
      plotOutput("populationPlot"),
      plotOutput("samplePlot"),
      plotOutput("bootstrapPlot")
    )
  )
)

density_plot <- function(data, title, fill) {
  # Create a density plot with labeled mean and median
  mean_val <- mean(data)
  median_val <- median(data)
  
  plot <- ggplot(data.frame(Value = data), aes(x = Value)) +
    geom_density(fill = fill, alpha = 0.5) +
    geom_vline(xintercept = mean_val, color = "black", linetype = "dashed") +
    geom_vline(xintercept = median_val, color = "grey", linetype = "dotted") +
    labs(title = title, x = "", y = "Density") +
    annotate("text", x = mean_val, y = 0, label = "Mean", vjust = -1) +
    annotate("text", x = median_val, y = 0, label = "Median", vjust = -1) +
    theme_bw()
  
  return(plot)
}

# Server logic
server <- function(input, output) {
  output$populationPlot <- renderPlot({
    
    # Generate population data
    set.seed(2024)
    population_data <- rsn(n = 10000, xi = 0, omega = 1, alpha = input$skewness)
    
    density_plot(population_data, "Population Distribution", "red") 
  })
  
  output$samplePlot <- renderPlot({
    
    # Generate sample data
    set.seed(2024) 
    sample_data <- rsn(n = input$n, xi = 0, omega = 1, alpha = input$skewness)
    
    density_plot(sample_data, "Sample Distribution", "blue") 
  })
  
  output$bootstrapPlot <- renderPlot({
    
    set.seed(2024)
    sample_data <- rsn(n = input$n, xi = 0, omega = 1, alpha = input$skewness)
    
    # Bootstrap resampling to calculate bootstrap means
    bootstrap_means <- sapply(1:input$k, function(x) {
      resampled_data <- sample(sample_data, replace = TRUE)
      mean(resampled_data)
    })
    
    # Bootstrap distribution of sample means
    density_plot(bootstrap_means, "Bootstrap Sample Mean Distribution", "green") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)