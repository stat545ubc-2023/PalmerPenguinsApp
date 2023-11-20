# Load required libraries
library(shiny)
library(tidyverse)
library(DT)

# Load palmerpenguins data
penguins <- palmerpenguins::penguins

# Define UI for application
ui <- fluidPage(
  titlePanel("Palmer Penguins Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:",
                  choices = names(penguins %>% select_if(is.numeric)),
                  selected = "body_mass_g"),
      sliderInput("binwidth", "Bin Width:",
                  min = 1, max = 50, value = 10),
      selectInput("species", "Select Penguin Species:",
                  choices = unique(penguins$species),
                  selected = unique(penguins$species)[1])
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", 
                 plotOutput("histogram"),
                 h4("Total Number of Penguins:"),
                 textOutput("total_results")),
        tabPanel("Summary Table",
                 h4("Summary Table"),
                 verbatimTextOutput("summary")),
        tabPanel("Data Table",
                 h4("Selected Species Data Table"),
                 DTOutput("data_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression for the selected data
  selected_data <- reactive({
    penguins %>%
      filter(species == input$species)
  })
  
  # Generate a histogram
  output$histogram <- renderPlot({
    ggplot(selected_data(), aes(x = !!sym(input$variable))) +
      geom_histogram(binwidth = input$binwidth, fill = 'darkgrey') +
      labs(x = input$variable,
           title = paste('Histogram of', input$variable, 'for', input$species)) +
      theme_bw()
  })
  
  # Display total number of results
  output$total_results <- renderText({
    paste(nrow(selected_data()), "penguins")
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    summary(selected_data() %>% pull(!!sym(input$variable)))
  })
  
  # Display data table with DT
  output$data_table <- renderDT({
    datatable(selected_data(), options = list(pageLength = 10)) # You can adjust page length as needed
  })
}

# Run the application
shinyApp(ui = ui, server = server)