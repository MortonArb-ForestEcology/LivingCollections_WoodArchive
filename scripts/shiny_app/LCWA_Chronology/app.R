##----------------------------------------------------------------------------------------------------------------------------------#
#Script 4 of 4 
#Script by: Jocelyn Garcia
#Project: Living Collections Wood Archive 
#Purpose:Create a web application housing the master chronology from "2.5_Build_chronology.R."
#         Web app should allow for isolation of certain time period inputed by the user.
#Helpful Resources: https://mastering-shiny.org/basic-app.html
#                 https://shiny.rstudio.com/tutorial/
#                 https://bookdown.org/loankimrobinson/rshinybook/basic-front.html


#install.packages("shiny") #might need to download if first time

library(shiny)
library(bslib)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr) # might need install.packages("ggpubr")
library(shinyBS) # might need install.packages("shinyBS")
library(plotly) #install.packages("plotly")
library(DT) #install.packages("DT")



#importing data we need, should update as 2.5_Build_chronology is run
RingData <- read_csv("data/RingData.csv")
RingData$std <- RingData$std-1
RingData <- rename(RingData,c("rwi" = "std")) #rename so it shows up in 'Data Review' tab more accurately
str(RingData)  

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("LCWA Chronology - Created 2024"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput("yearRange", "Select Year Range:",
                               min = min(RingData$years),
                               max = max(RingData$years),
                               value = c(min(RingData$years), max(RingData$years)),
                               sep = "",
                  ),
        h4(" Marker Rings For Inputted Range:"),
        
      tags$div(
        style = "overflow-y: auto; max-height: 800px; font-size: 12px;",
        DTOutput("DataTableMY")
              ),
      layout_columns(
        card(
          card_header("Input Desired Quantile % (2 decimal places)"),
          numericInput("quantile_value", "Input Quantile (e.g., 0.10 for 10%):", 
                       value = 0.10, min = 0, max = 1, step = 0.01)
        )
      )
        ),
      # Show a plot of the generated distribution
      mainPanel(
       tabsetPanel(
         id = "tab_id",
         
         #first tab for data 
         tabPanel(
           "Bar Plot",
           value = "plot_tab",
           br(),
           plotlyOutput("Barchart", width = "107%", height = "600px"),
           br(),
           
         ),
         #second tab for plot
         tabPanel(
           "Data Review",
           value = "data_tab",
           br(),
           dataTableOutput("DataTableRD")
         ),
         
         tabPanel(
           "Considerations",
           h3("Specifics"),
           helpText("All data collected is from trees extracted from The Morton Arboretum (Lisle, IL). 
                    Samples were first visually dated then crossdated before being analyzed in R. Processing
                    included various statistical methods such as detrending and spline fitting in order to lessen
                    the effects on tree growth that weren't being analyzed."),
           h3("Bar Plot"),
           h5("Note: RWI's (Ring Width Index) were standarized around 1."),
           helpText(
             HTML("<li>Dark Slate Gray - RWI's that were positive (ie above 0) were considered big rings <br><br>"
                  ,"<li>Tan - RWI's that were negative (ie below 0) were considered small rings, but not marker rings<br><br>"
                  ,"<li>Light Slate Gray - Marker Rings that were determined by a quartile value, which was calculed using one the RWI data from the 
                   small rings, default is set to capture the lowest 10% of the small ring data. <br><br>")),
                  
          h5("Other things to note:"),
                  
          helpText(
             HTML( "<li>The years along center are color coded accrodingly to match the year's RWI classification<br>"
                   ,"<li>Barchart adjusts to focus only on years specified by range slider on the right<br>"
                   ,"<li>Marker years will be listed on left for range specified by range slider<br>"
                   ,"<li> Data (rwi, samp.depth = how many samples we had data for in that year, and year)
                   for specified years in range will be under the 'Data Review' tab<br><br><br><br><br><br>Created November 2024
                   <br>As part of the Research Technician Fellowship Program: Forest Ecology<br>Purpose: Display chronology data
                   in a way that is accessible, easy to understand, and efficient for all users including <br>")),
           value = "considerations_tab",
           br(),
           
         )
         
        
       ),
      )
    ),  
)

 
  # Define server logic
  server <- function(input, output) {
    
  #Start of necessary calculations needed for functions including quantile calculations
    
    #Below Calculations needed for Marker Year Table in Sidebar
    # Reactive filtered dataset for RWI < 0
    SRData <- reactive({
      RingData %>% filter(rwi < 0)
    })
    
    # Reactive quantile value based on the filtered dataset
    quantile_value <- reactive({
      quantile(SRData()$rwi, probs = input$quantile_value, na.rm = TRUE)
    })
    
    # Reactive marker years based on quantile filtering
    marker_years <- reactive({
      sr_data <- SRData()  # Access reactive SRData()
      
      # Apply quantile filtering
      rows_under_quantile <- sr_data[sr_data$rwi < quantile_value() & !is.na(sr_data$rwi), ]
      
      # Apply year range filtering on the already filtered data
      filtered_data <- rows_under_quantile[
        rows_under_quantile$years >= input$yearRange[1] & 
          rows_under_quantile$years <= input$yearRange[2], 
      ]
      
      # Return the final filtered data
      filtered_data[, c("years", "rwi")]
    })
    
  #Below calculations needed for table in Data Review
    filtered_RingData <-reactive({
      RingData %>%
        filter(years >= input$yearRange[1] & years <= input$yearRange[2])
    })
    
    #turned filtered_RingData to nonreactive data put into storedData
    observe({
      # Get the filtered data
      filtered_Rdata <- filtered_RingData()
      
      # Assign it to the global environment as a regular R object
      assign("storedData", filtered_Rdata, envir = .GlobalEnv)
      
      # Now print the stored value correctly by accessing the global object
      print(storedData)  # This will now print the stored data to the console
    })

  #end of calculations ------------------------------------------------------
   
    # Render table output for Marker Years in Sidebar
    SRData <- reactive({
      RingData %>% filter(rwi < 0)
    })
    
    # Marker Year Table
    output$DataTableMY <- renderDT({
      datatable(marker_years(),
                options = list(
                  pageLength = 10,      # Show only 10 rows at a time
                  lengthChange = FALSE, # Disable the option to change page length
                  searching = FALSE,    # Disable searching (optional)
                  columnDefs = list(
                    list(targets = 0, visible = FALSE, width = "0px") # Hide the row index (first column)
                  )
                ),
                # Additional style options to remove padding or extra space
                style = "bootstrap4", 
                escape = FALSE)
    })
    
    
    # Render table for Ring Data in Main Panel under Data Review Tab
    output$DataTableRD <- renderDataTable({
      filtered_RingData()
    })
    
  #Render 2 sided bar graph so it updates with user input
  output$Barchart <-renderPlotly({
    
    filtered_data <- filtered_RingData()
    
    Ring <- filtered_data %>%
      mutate(big_rings = ifelse(rwi >= 0, rwi, NA),
             small_rings = ifelse(rwi < 0, rwi, NA),
             rwi = round(rwi, 3))
    
    # Remove NAs to calculate quantile
    small_rings_no_NA <- na.omit(Ring$small_rings)
    
    # Quantile for small rings
    quantile_value <- quantile(small_rings_no_NA, probs = input$quantile_value, na.rm = TRUE)
    
    Ring <- Ring %>%
      mutate(
        category = case_when(
          is.na(small_rings) ~ "Big Rings",
          small_rings < quantile_value ~ "Marker Rings",
          small_rings < 0 ~ "Small Rings"
        )
      )
    
    get_color <- function(value) {
      ifelse(is.na(value), "darkslategray", 
             ifelse(value < quantile_value, "darkslategray4", "tan4"))
    }
    
    text_colors <- get_color(Ring$small_rings)
    
  #Simplifying plot w/ plotly
    
    Ring$hover <- paste0("Year: ", Ring$years, "<br>RWI: ", Ring$rwi,"<br>Category: ", Ring$category)
    
    Ring$years <- factor(Ring$years)
    
    plot <- ggplot(Ring, aes(x = years, y = rwi, text = hover, fill = category)) +
      geom_col(width = 0.8) +
      scale_fill_manual(
        values = c(
          "Big Rings" = "darkslategray",
          "Marker Rings" = "tan4",
          "Small Rings" = "darkslategray4"
        ),
        name = "Ring Categories"
      ) +
      scale_y_continuous(expand = c(0, 0)) +  # Ensures the bars are at the bottom (y=0)
      scale_x_discrete(expand = c(0, 0)) +  # Ensures the x-axis labels are at the base of the bars
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),  # Optionally remove x-axis title
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(b = -2)),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        plot.margin = margin(t = 10, b = 50)
      )
    
    
    # Avoids duplicate in tooltips
    ggplotly(plot, tooltip = c("text")) %>%
      layout(legend = list(orientation = 'h', y = -0.1)) 

})

  }

# Run the application 
shinyApp(ui = ui, server = server)

