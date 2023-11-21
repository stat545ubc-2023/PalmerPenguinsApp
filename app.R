# Load required libraries
library(shiny)
library(tidyverse)
library(DT)

# Load palmerpenguins data
penguins <- palmerpenguins::penguins

# Define UI for application
ui <- fluidPage(
  
  # Title of Shiny app
  titlePanel("Palmer Penguins Explorer"),
  
  # Sidebar added
  sidebarLayout(
    sidebarPanel(

      # Feature 1: Drop down menu to select variable for histogram
      # Allows users to select a specific variable of their choice to plot a histogram and view distribution
      selectInput("variable", "Select Variable:",
                  choices = names(penguins %>% select_if(is.numeric)),
                  selected = "body_mass_g"),
      
      # Feature 2: Slider to select bin width for histogram
      # Allows users to adjust bin width for the histogram, which can make the histogram easier to understand, depending on the spread of the data per variable
      sliderInput("binwidth", "Bin Width:",
                  min = 1, max = 50, value = 10),
      
      #Allows user to select penguin species
      selectInput("species", "Select Penguin Species:",
                  choices = unique(penguins$species),
                  selected = unique(penguins$species)[1])
    ),
    
    mainPanel(
      
      # Feature 3: Separates various information via tabs
      # Visually more appealing, as it prevents cluttering of various information that users may not want to see all at once
      tabsetPanel(
        
        #First tab shows histogram and its respective n value (number of penguins in the selected species)
        tabPanel("Histogram", 
                 plotOutput("histogram"),
                 h4("Total Number of Penguins:"),
                 textOutput("total_results"),
                 
                 # Feature 4: Download button
                 # Allows users to download the histogram as an image file
                 downloadButton("downloadHist", "Download Histogram")),
        
        # Feature 5: Summary table for selected species
        # Displays various summaries, such as min, max, median, etc., in an organized table
        tabPanel("Summary Table",
                 h4("Summary Table"),
                 verbatimTextOutput("summary")),
        
        # Displays data table for the selected penguin species and adds download button
        tabPanel("Data Table",
                 h4("Selected Species Data Table"),
                 DTOutput("data_table"),
                 downloadButton("downloadTable", "Download Data Table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filtered data based on user selection
  selected_data <- reactive({
    penguins %>%
      filter(species == input$species)
  })
  
  # Generate a histogram that changes x variable and bin width based on user selection
  output$histogram <- renderPlot({
    ggplot(selected_data(), aes(x = !!sym(input$variable))) +
      geom_histogram(binwidth = input$binwidth, fill = 'darkgrey') +
      labs(x = input$variable,
           title = paste('Histogram of', input$variable, 'for', input$species)) +
      theme_bw()
  })
  
  # Display total number of results (n-value for histogram)
  output$total_results <- renderText({
    paste(nrow(selected_data()), "penguins")
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    summary(selected_data() %>% pull(!!sym(input$variable)))
  })
  
  # Feature 6: Drop down menu to select number of entries shown for data table
  # Display data table and allows users to select how many entries at once they want to view
  output$data_table <- renderDT({
    datatable(selected_data(), options = list(pageLength = 10)) # You can adjust page length as needed
  })
  
  # Download handler for histogram
  output$downloadHist <- downloadHandler(
    filename = function() {
      paste('histogram_', input$variable, '_', input$species, '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        ggplot(selected_data(), aes(x = !!sym(input$variable))) +
          geom_histogram(binwidth = input$binwidth, fill = 'darkgrey') +
          labs(x = input$variable,
               title = paste('Histogram of', input$variable, 'for', input$species)) +
          theme_bw()
      }, device = "png", width = 10, height = 8)
    }
  )
  
  # Download handler for data table
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('datatable_', input$species, '.csv', sep = '')
    },
    content = function(file) {
      write.csv(selected_data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)