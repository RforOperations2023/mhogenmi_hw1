library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
bway <- read.csv("Final_Bway.csv")
bway_group_month <- read.csv("Monthly_Avg_Metrics.csv")
gb_type <- read.csv("Donut_Chart_Manipulated.csv")


#Define UI for application that plots features of movies -----------
 ui <- fluidPage(

#    Sidebar layout with a input and output definitions --------------
   sidebarLayout(

#      Inputs: Select variables to plot ------------------------------
     sidebarPanel(


# Choose color by option ----------------------------------
      selectInput(inputId = "z",
            label = "Theatre:",
            choices = sort(c(unique(bway$Theatre))),
            selected = "Walter Kerr"),


# Input: Specification of range within an interval ----
sliderInput(inputId = "Year",
            label = "Year:",
            min = 2000, max = 2015,
            step = 1,
            sep = "",
            value = 2008),

# Select variable for y-axis ----------------------------------
selectInput(inputId = "y",
            label = "Y-axis (Graph on the Right)",
            choices = c("Average Capacity Filled (%)" = "Capacity", 
                        "Average Weekly Total Audience Count" = "Attendance",
                        "Average Weekly Gross" = "Gross"),
            selected = "Capacity"),
    

# Show data table ---------------------------------------------
    checkboxInput(inputId = "show_data",
              label = "Show data table",
              value = TRUE),
    
     plotOutput(outputId = "barplot"),
     plotOutput(outputId = "donut")
      

     ),




#    Output: Show scatterplot --------------------------------------
     mainPanel(
       
       plotlyOutput(outputId = "scatterplot"),
       DT::dataTableOutput(outputId = "bwaytable"),
       downloadButton(outputId = "download", label = "Download Data")
     )
   )
 )
#
# # Define server function required to create the scatterplot ---------
 server <- function(input, output) {
   
   
# Create a subset of data filtering by Year Range ------
   bway_subset <- reactive({
     req(input$Year) # ensure availablity of value before proceeding
     bway %>%
       filter(Year == input$Year,
              Theatre == input$z)
   })
   
#Make another subset using the code in the test script for grouped by month averages

   bway_subset_scatter <- reactive({
     req(input$Year) # ensure availablity of value before proceeding
     bway_group_month %>%
       filter(Year == input$Year,
              Theatre == input$z)
   })
   
   
# #Make another subset using the code in the test script for donut chart for show type
#    
   bway_subset_donut <- reactive({
     req(input$Year) # ensure availablity of value before proceeding
     gb_type %>%
       filter(Year == input$Year,
              Theatre == input$z)
   })

#
#   # Create scatterplot object the plotOutput function is expecting --
   output$scatterplot <- renderPlotly({
    
     ggplotly(ggplot(data = bway_subset_scatter(), aes_string(x = "Month", 
                                     y = input$y,
                                    color = c("Meh" = "Show" ))) +
      geom_point() +  theme(axis.text.x = element_text(angle = 45)) + 
        scale_x_discrete(limits=c("January", "February","March","April",'May',"June","July","August",'September',"October","November","December"))
      + ggtitle("Monthly Averages")  + labs(x = "Month", y = input$y) 
      + scale_fill_discrete(name = "Meh")

      
      ) 
     
     
     })
     

       # Create barplot object the plotOutput function is expecting --
     output$barplot <- renderPlot({
      ggplot(data = bway_subset(), aes_string(x ="Show",
                                               fill = "Show" 
                                              )) +
      geom_bar() + labs(x = "Show", y="Number of Weeks") +
      theme(axis.text.x = element_text(angle = 90))
  
   })
     
     # Create donut plot object the plotOutput function is expecting --
     
     output$donut <- renderPlot({
     ggplot(bway_subset_donut(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) +
       geom_rect() +
       coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
       xlim(c(2, 4))  + 
       geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
       theme_void()
     })
     
       
     #Download Data as Broadway data (Year Chosen)
     output$download <- downloadHandler(
         filename = function() { 
           paste("Broadway Data ", input$Year, ".csv", sep="")
         },
         content = function(file) {
           write.csv(bway, file)
         })
  

   
   
   # Print data table if checked -------------------------------------
   output$bwaytable <- DT::renderDataTable(
     if(input$show_data){
       DT::datatable(data = bway_subset()[, 2:9], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)
     }
   )
 }
#
# # Run the application -----------------------------------------------
 shinyApp(ui = ui, server = server)