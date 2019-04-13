library(shiny)
#library(plyr) # Must load plyr before dplyr
library(dplyr)
library(tidyr)
library(plotly)
library(sp)
library(rworldmap)
library(leaflet)
library(DT)

source("Helper.R")

# Process data: filter out to get only data entries for earthquake with columns that provide interesting information
earthquake <- read.csv("https://raw.githubusercontent.com/qishuyi/RShiny-App/master/database.csv?token=AWoyYe3Zdp8DlJbOoENn9X5C8mpdfqhjks5ctS91wA%3D%3D", header = TRUE)
earthquake <- filter(earthquake, Type == "Earthquake")
earthquake <- select(earthquake, 1:4, 6, 9:10, 17)
# We will make a bar chart showing the number of significant earthquakes for the selected years
earthquake <- separate(data = earthquake, col = Date, into = c("Month", "Day", "Year"), sep = "/", remove = FALSE)
earthquake$Depth <- as.numeric(earthquake$Depth)
earthquake$Magnitude <- as.numeric(earthquake$Magnitude)
head(earthquake)

# Make a data frame where the first column is longitude in degrees and the second column is latitude in degrees
latlon.df <- select(earthquake, 7, 6)
# Add a continent column based on latitude/longitude information
earthquake$Continent <- coords2continent(latlon.df)

# Format hover text for dot plot
earthquake$dotplot.hover <- with(earthquake, paste("Date: ", Date, "<br>", "Magnitude: ", Magnitude))
earthquake$map.hover <- with(earthquake, paste(Date, " ", Time, "<br>", "Depth: ", Depth, "km",
                                              "<br>", "Magnitude: ", Magnitude))

ui <- navbarPage("Earthquake Data Visualizer",
  tabPanel("Explore Dataset",
           fluidPage(
              sidebarLayout(
                sidebarPanel(
                  # Slider bar for year
                  sliderInput("YearRange", "Timeline:",
                              min = 1965, max = 2016,
                              value = c(1998, 2016)),
                  # Slider bar for magnitude
                  sliderInput("Magnitude", "Magnitude:",
                              min = min(earthquake$Magnitude), max = max(earthquake$Magnitude),
                              value = c(min(earthquake$Magnitude), max(earthquake$Magnitude))),
                  width = 3
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  tabsetPanel(
                    tabPanel("Data", DT::dataTableOutput("dataframe")),
                    tabPanel("Visualization", br(), leafletOutput("worldmap"),
                                              br(), plotlyOutput("barchart_number_year"), 
                                              br(), plotlyOutput("dotplot_magnitude_year"),
                                              br(), plotlyOutput("barchart_continent"))
                  )
                )
              )
           )),
  tabPanel("References",
           fluidPage(
             h2("Refereces:"), br(),
             h5("Layout: Lego Database Set Explorer in Shiny App Gallery"), br(),
             h5("Function that turns latitutde and longitude into region: StackOverflow(https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r)"), br()
           ))
)

# Server logic
server <- function(input, output) {
  output$barchart_number_year <- renderPlotly({
    # Make a bar chart for the number of significant earthquakes for the users' selected years
    using.data <- filter(earthquake, Magnitude >= input$Magnitude[1] & Magnitude <= input$Magnitude[2] & Year >= input$YearRange[1] & Year <= input$YearRange[2])
    
    # Create a bar chart with year and the number of significant earthquakes that happened that year
    plot_ly(data = using.data, x= ~Year, type = "histogram", color = I("cornflowerblue")) %>%
    layout(title = "Number of Significant Earthquakes by Year",
             titlefont = list(size = 16),
             xaxis = list(title = "Year", tickangle = 45),
             yaxis = list(title = "Number of Earthquakes")
      )
  })
  
  output$dotplot_magnitude_year <- renderPlotly({
    # Create a dot plot of year against earthquake magnitudes
    plot_ly(data = filter(earthquake, Magnitude >= input$Magnitude[1] & Magnitude <= input$Magnitude[2] & Year >= input$YearRange[1] & Year <= input$YearRange[2]), 
            x = ~Year, y = ~Magnitude, type = "scatter", text = ~dotplot.hover, color = "Dark Orange") %>%
      layout(title = "Earthquake Magnitude by Year",
             titlefont = list(size = 16),
             xaxis = list(title = "Year", tickangle = 45),
             yaxis = list(title = "Magnitude of Earthquake", hoverformat = ~dotplot.hover)
      )
  })
  
  output$worldmap <- renderLeaflet({
    using.data <- filter(earthquake, Magnitude >= input$Magnitude[1] & Magnitude <= input$Magnitude[2] & Year >= input$YearRange[1] & Year <= input$YearRange[2])
    # Define a color palette
    pal <- colorNumeric(palette = "YlGnBu", using.data$Depth)
    leaflet(using.data) %>% addTiles() %>% 
      addCircles(~Longitude, ~Latitude, popup = ~map.hover, color = ~pal(Depth)) %>%
      addLegend("bottomright", pal = pal, values = ~Depth, title = "Depth(km)", opacity = 1)
  })
  
  output$barchart_continent <- renderPlotly({
    using.data <- filter(earthquake, Magnitude >= input$Magnitude[1] & Magnitude <= input$Magnitude[2] & Year >= input$YearRange[1] & Year <= input$YearRange[2])
    
    # Create a bar chart of region against the number of earthquakes that happened in that region
    plot_ly(data = using.data, x = ~Continent, type = "histogram", color = I("Light Salmon")) %>%
      layout(title = "Number of Earthquakes by Region",
             titlefont = list (size = 16),
             xaxis = list(title = "Region"), 
             yaxis = list(title = "Number of Earthquakes")
             )
  })
  
  output$dataframe <- DT::renderDataTable({
    using.data <- filter(earthquake, Magnitude >= input$Magnitude[1] & Magnitude <= input$Magnitude[2] & Year >= input$YearRange[1] & Year <= input$YearRange[2])
    select(using.data, 1, 5:9, 11)
  })
}

# Complete app with UI and server components
shinyApp(ui, server)

