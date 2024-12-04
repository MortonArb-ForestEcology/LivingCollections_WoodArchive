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
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr) # might need install.packages("ggpubr")


#importing data we need, should update as 2.5_Build_chronology is run
RingData <- read_csv("data/RingData.csv")
RingData$std <- RingData$std-1
RingData <- rename(RingData,c("rwi" = "std")) #rename so it shows up in 'Data Review' tab more accurately
str(RingData)  
MarkerYears <-read_csv("data/MarkerRingYears.csv")
str(MarkerYears) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("LCWA Chronology - Created 2024"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("yearRange", "Select Year Range:",
                               min = min(RingData$years),
                               max = max(RingData$years),
                               value = c(min(RingData$years), max(RingData$years)),
                               sep = "",
                  ),
        h4(" Marker Rings For Inputted Range:"),
        
      tags$div(
        style = "overflow-y: auto; max-height: 800px; font-size: 12px;",
        dataTableOutput("DataTableMY")
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
           plotOutput("Barchart", width = "100%", height = "800px"),
           br()
    
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

 
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    # Reactive filtered data
    filtered_data <- reactive({
      MarkerYears[MarkerYears$years >= input$yearRange[1] & MarkerYears$years <= input$yearRange[2], ]
    })
    
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

  
   
    # Render table output for Marker Years in Sidebar
    output$DataTableMY <- renderDataTable({
      filtered_data()
    })
    
    # Render table for RingData in Main Panel
    output$DataTableRD <- renderDataTable({
      filtered_RingData()
    })
    
  #Render 2 sided bargraph so it updates with user input
  output$Barchart <-renderPlot({
    
    filtered_data <- filtered_RingData()
   
    Ring <- filtered_data %>%
      mutate(big_rings = ifelse(rwi >= 0, rwi, NA),
             small_rings = ifelse(rwi < 0, rwi, NA))
    
    # Remove NAs to calculate quantile
    small_rings_no_NA <- na.omit(Ring$small_rings)
    
    # Quantile for small rings
    quantile_value <- quantile(small_rings_no_NA, probs = c(0.10))
    
    get_color <- function(value) {
      ifelse(is.na(value), "darkslategray", 
             ifelse(value < quantile_value, "darkslategray4", "tan4"))
    }
    
    text_colors <- get_color(Ring$small_rings)
    
    Ring$years <- factor(Ring$years)
    
    # p1 - Bar chart for small rings
    p1 <- ggplot(Ring, aes(x = years, y = small_rings)) +
      geom_col(width = .8, fill = text_colors) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_discrete(position = "top") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 85, hjust = -.1, margin = margin(b = -2), color = text_colors),
        axis.title.y = element_text(size=7),
        axis.text.y.left = element_text(size=7),
        plot.margin = margin(3, 3, 3, 3),
        panel.background = element_rect(fill = "white", colour = "white")
      )
    
    # p2 - Bar chart for big rings
    p2 <- ggplot(Ring, aes(x = years, y = big_rings)) +
      geom_col(width = .8, fill = "darkslategray") +
      scale_y_continuous(expand = c(0, 0)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=7),
        axis.text.y = element_text(size=7),
        plot.margin = margin(3, 3, 3, 3),
        panel.background = element_rect(fill = "white", colour = "white")
      )
    
    # Combine the plots
    ggarrange(p2, p1, ncol = 1, nrow = 2)
  
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

