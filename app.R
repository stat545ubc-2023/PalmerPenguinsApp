# Load required libraries
library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(shinydashboard)

# Load palmerpenguins data
penguins <- palmerpenguins::penguins

# Rename variables to make labels more readable in histogram (Addressing feedback)
penguins <- penguins %>%
  rename(`Body Mass (g)` = body_mass_g,
         `Bill Length (mm)` = bill_length_mm,
         `Bill Depth (mm)` = bill_depth_mm,
         `Flipper Length (mm)` = flipper_length_mm,
         Year = year)

# Define UI for application
ui <- dashboardPage( # Use of dashboardPage from shinydashboard, instead of fluidPage to allow for more features and flexibility
  
  # Title of Shiny app
  dashboardHeader(title = "Palmer Penguins Explorer"),
  
  # Sidebar added
  dashboardSidebar(
    
    # Feature 1: Drop down menu to select variable for histogram
    # Allows users to select a specific variable of their choice to plot a histogram and view distribution
    selectInput("variable", "Select Variable:",
                choices = names(penguins %>% select_if(is.numeric)),
                selected = "Flipper Length (mm)"),
    
    # Feature 2: Slider to select bin width for histogram
    # Allows users to adjust bin width for the histogram, which can make the histogram easier to understand, depending on the spread of the data per variable
    sliderInput("binwidth", "Bin Width:",
                min = 1, max = 50, value = 1),
    
    # NEW Feature 1: Selector for multiple species
    # Allows user to select multiple species with check boxes to allow for histogram of multiple species separated by color (Addressing feedback)
    checkboxGroupInput("species", "Select Penguin Species:",
                       choices = unique(penguins$species),
                       selected = unique(penguins$species)),
    
    # NEW Feature 2: Combine or separate histogram
    # Allows users to either combine the histograms of selected species into one sorted by color, or facet them by species
    checkboxInput("combineSpecies", "Combine Histogram", TRUE)
  ),
  
  dashboardBody(
    
    # Feature 3: Separates various information via tabs
    # Visually more appealing
    tabsetPanel(
      
      # First tab shows an interactive histogram and its respective n value (number of penguins in the selected species)
      tabPanel(
        "Histogram",
        fluidRow(
          plotlyOutput("histogram"),
          h4("Total Number of Penguins:"),
          textOutput("total_results"),
          
          # Feature 4: Download button
          # Allows users to download the histogram as an image file
          downloadButton("downloadHist", "Download Histogram")
        )
      ),
      
      # Feature 5: Summary table for selected species
      # Displays various summaries, such as min, max, median, etc., in an organized table
      tabPanel(
        "Summary Table",
        verbatimTextOutput("summary")
      ),
      
      # Displays data table for the selected penguin species and adds download button
      tabPanel(
        "Data Table",
        fluidRow(
          DTOutput("data_table"),
          downloadButton("downloadTable", "Download Data Table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filtered data based on user selection
  selected_data <- reactive({
    penguins %>%
      filter(species %in% input$species)
  })
  
  # Generate an interactive histogram that changes x variable and bin width based on user selection
  # If else statement for combining or separating histogram
  output$histogram <- renderPlotly({
    if (input$combineSpecies) {
      
      # Combined histogram separated by color
      p <- ggplot(selected_data(), aes(x = !!sym(input$variable), fill = species)) +
        geom_histogram(binwidth = input$binwidth, position = "identity", alpha = 0.7) +
        labs(x = input$variable,
             title = paste('Histogram of', input$variable, 'for Selected Penguin Species')) +
        theme_bw()
      
      # NEW Feature 3: Interactive histogram where users can hover over the bars to see information of x-value, count, and species. Can also snip an area to zoom in on the part of the graph.
      # Allows users to get a detailed overview of the data presented in the histogram
      ggplotly(p, tooltip = c("x", "count", "species")) %>%
        config(displayModeBar = FALSE)  # Disable the toolbar
    } else {
      
      # Separate (faceted) histograms
      p <- ggplot(selected_data(), aes(x = !!sym(input$variable))) +
        geom_histogram(binwidth = input$binwidth) +
        labs(x = input$variable,
             title = paste('Histogram of', input$variable)) +
        facet_wrap(~species, scales = "free") +
        theme_bw()
    }
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
      paste('histogram_', input$variable, '_selected_species.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        ggplot(selected_data(), aes(x = !!sym(input$variable), fill = species)) +
          geom_histogram(binwidth = input$binwidth, position = "identity", alpha = 0.7) +
          labs(x = input$variable,
               title = paste('Histogram of', input$variable, 'for Selected Penguin Species')) +
          theme_bw()
      }, device = "png", width = 10, height = 8)
    }
  )
  
  # Download handler for data table
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('datatable_selected_species.csv', sep = '')
    },
    content = function(file) {
      write.csv(selected_data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
