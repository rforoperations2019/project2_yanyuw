#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(rgeos)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tools)
library(shinyjs)
library(DT)
library(readxl)

#  Read US states geodata
geodata.load <- readOGR("../tl_2016_us_uac10/tl_2016_us_uac10.shp", layer = "tl_2016_us_uac10", GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE)
geoname <- strsplit(geodata.load@data$NAME10, split = ", ")
geoname <- do.call(rbind, geoname)
colnames(geoname) <- c("City", "StateCode")
geodata.load@data <- data.frame(geodata.load@data[,-3], geoname)

# Read vehicle ownership data
vehicle_ownership <- read.csv("../vehicle_ownership.csv",
                              col.names = c("Area", "house_no_vehicle2015", "house_no_vehicle2016", "vehicle_per_house2015", "vehicle_per_house2016"), 
                              stringsAsFactors = FALSE)
# Split the Area column to city and state
city_list <- str_split(vehicle_ownership$Area, ", ")
city_list <- do.call(rbind, city_list)
colnames(city_list) <- c("City", "State")
vehicle_ownership <- data.frame(vehicle_ownership[,-1],city_list)

# Read State name and its code spreadsheet
state_abbr <- read.csv("../state_code_list.csv", stringsAsFactors = FALSE)
# Merge the spreadsheet with ownership data with state name
vehicle_ownership <- merge(vehicle_ownership, state_abbr, sort = FALSE, by.x = "State", by.y = "State")
# Merge the new ownership data with geodata
geodata.load@data <- merge(geodata.load@data, vehicle_ownership, by.x = c("StateCode","City"), by.y =c("Code", "City"))

# Read excel data of states CO2 emission
emission_data <- read_excel("../sectors_2016.xlsx", skip = 3)
colnames(emission_data) <- c("State", "commercial_mmt", "electric_power_mmt", "residential_mmt", "industrial_mmt", "transportation_mmt", "total_mmt",
                 "commercial_share", "electric_power_share", "residential_share", "industrial_share", "transportation_share")
emission_data$total_share <- 1

# Define UI for application
ui <- navbarPage("Vehicles And Emission Facts", 
                 tabPanel("Vehicle Household Conditions",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "year",
                                              label = "Select Interest Year:",
                                              choices = c("2015", "2016"),
                                              selected =  "2016"),
                                  disabled(sliderInput(inputId = "car_number",
                                              label = "Choose the vehicle numbers range:",
                                              min = 0.6, 
                                              max = 2.4, 
                                              step = 0.1, 
                                              value = c(1,1.5))
                                           ),
                                  checkboxInput(inputId = "show_data",
                                                label = "Show Data Table",
                                                value = TRUE)
                              ),
                              mainPanel(
                                  shinyjs::useShinyjs(),
                                  # Style the background and change the page
                                  tags$style(type = "text/css", "#household_map {height: calc(70vh - 90px) !important;}"),
                                  leafletOutput(outputId = "household_map"),
                                  tags$br(),
                                  DT::dataTableOutput(outputId = "data_table"),
                                  downloadButton(outputId = "downloadFile", label = "Download the Raw File")
                                  )
                              )
                          ),
                 tabPanel("US CO2 Emission Facts",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId = "result_type",
                                               label = "Select the Result Type:",
                                               choices = c("Statisctical Values" = "mmt", "Shares" = "share"),
                                               selected = "mmt"),
                                  selectInput(inputId = "emission_type",
                                                     label = "Choose the Emission Source:",
                                                     choices = c("Commercial" = "commercial", "Electric power" = "electric_power", "Residential" = "residential", "Industrial" = "industrial", "Transportation" = "transportation", "Total" = "total"),
                                                     selected = "total"),
                                  sliderInput(inputId = "bin_num",
                                              label = "Number of bins in histogram (approximate):",
                                              min = 5, max = nrow(emission_data)-2, value = 10, step = 5)
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Histogram", plotlyOutput("histogram")),
                                      tabPanel("Bar Chart", plotlyOutput("horizontal_bar")),
                                      tabPanel("Table", dataTableOutput("test_table")),
                                      useShinyjs(),
                                      tags$style(type = "text/css", "#horizontal_bar {height: calc(80vh - 90px) !important;}
                                                 #histogram {height: calc(70vh - 90px) !important;}")
                                      )
                                  )
                              )
                          )
)

# Define server logic
# Tab: Map with polygon layer
server <- function(input, output, session) {
    observeEvent(input$year,{
        enable("car_number")
    })
    # update car number range of the selected year
    observe({
        var <- input$year
        update_col_name <- paste0("vehicle_per_house",var)
        col <- geodata.load@data[, update_col_name]
        updateSliderInput(session, "car_number", min = min(col), max = max(col), value = c(min(col), max(col)))
    })
    # basemap
    output$household_map <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles("Esri.WorldStreetMap", group = "Esri World") %>%
            setView(-87.6500523, 41.850033, zoom = 5) %>%
            addLayersControl(baseGroups = c("Google", "Esri World"))
    })
    # create layer by year
    observe({
        year_subdata <- geodata.load
        year <- input$year
        col_name <- paste0("vehicle_per_house",year)
        pal_polygon <- colorBin("PuRd", domain = year_subdata@data[, col_name], bins = 4)
        leafletProxy("household_map", data = year_subdata) %>%
            clearGroup(group = "year_select") %>%
            addPolygons(  
                fillColor = ~pal_polygon(year_subdata@data[, col_name]),
                popup = ~paste0("<b>", StateCode, ":</b> ", year_subdata@data[, col_name], "cars per house"),
                weight = 1,
                opacity = 1,
                color = "blue",
                dashArray = "3",
                fillOpacity = 0.8,
                highlight = highlightOptions(
                    weight = 3,
                    color = "#FFFFFF",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                group = "year_select") 
    })
    # create dataset between selected range
    vehicle_subdata <- reactive({
        req(input$car_number)
        vehicle_data <- geodata.load
        year <- input$year
        col_name <- paste0("vehicle_per_house", year)
        vehicle_data <- vehicle_data[vehicle_data@data[, col_name] >= input$car_number[1] & vehicle_data@data[, col_name] <= input$car_number[2],]
        return(vehicle_data)
    })
    # create layer by number range
    observe({
        vehicle <- vehicle_subdata()
        year <- input$year
        col_name <- paste0("vehicle_per_house", year)
        pal_polygon <- colorBin("PuRd", domain = vehicle@data[, col_name], bins = 4)
        leafletProxy("household_map", data = vehicle) %>%
            clearGroup(group = "range_select")%>%
            addPolygons(  
                fillColor = ~pal_polygon(vehicle@data[, col_name]),
                popup = ~paste0("<b>", StateCode, ":</b> ", col_name, "cars per house"),
                weight = 3,
                opacity = 1,
                color = "blue",
                dashArray = "3",
                fillOpacity = 0.8,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#FFFFFF",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                group = "range_select") %>%
            addLegend(
                position = "topright",
                opacity = 0.8,
                pal = pal_polygon, 
                values = vehicle@data[, col_name], 
                title = "Number of Cars ownen by every house",
                group = "range_select")
    })
    # show the datatable with new range
    output$data_table <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = vehicle_subdata()@data[,-3:-14], 
                          options = list(pageLength = 10, scrollX = TRUE), 
                          rownames = FALSE)
        }
    )
    # allow users to download the complete, raw dataset
    output$downloadFile <- downloadHandler(
        filename = function(){
            paste("major_cities_vehicle_ownership.csv")
        },
        content = function(file){
            write.csv(geodata.load@data, file, row.names = FALSE)
        }
    )
    
    # Tab: Plots
    emission_sub <- reactive({
        req(input$result_type)
        type <- input$result_type
        if (type == "mmt"){
            emission_sub <- emission_data[-52:-53, 1:7]
        }else{
            emission_sub <- emission_data[-52:-53, -2:-7]
        }
        return(emission_sub)
    })

    emission_type_subset <- reactive({
        req(input$result_type)
        req(input$emission_type)
        type_col <- paste(input$emission_type, input$result_type, sep = "_")
        emission_type_subset <- emission_sub()[, type_col]
        emission_type_subset <- cbind(emission_sub()[,1], emission_type_subset)
        return(emission_type_subset)
    })
    output$test_table <- DT::renderDataTable(
        DT::datatable(data = emission_type_subset(),
                      rownames = FALSE
        )
    )
    his_plot_title <- reactive({
        if (input$result_type == "mmt"){
            his_plot_title = "Million Metric tons of CO2"
        }else{
            his_plot_title = "Shares"
        }
    })
    output$histogram <- renderPlotly({
        ggplotly(
            ggplot(data = emission_type_subset(), aes(x = emission_type_subset()[,2]), fill = '#8abaae') +
                geom_histogram(bins = input$bin_num) + 
                ggtitle(paste('Histogram of', his_plot_title(), "of", input$emission_type, sep = " ")) + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                xlab(his_plot_title()), 
            tooltip = 'text')
    })
    output$horizontal_bar <- renderPlotly({
        ggplotly(
            ggplot(data = emission_type_subset(), aes(x = emission_type_subset()$State, y = emission_type_subset()[, 2]), fill = "#50c878") + 
                geom_bar(stat = "identity") + 
                xlab("State") +
                ylab(input$emission_type) + 
                coord_flip(),
            tooltip = 'text')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
