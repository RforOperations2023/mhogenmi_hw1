library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
bway <- read.csv("Broadway_Prettified_2.csv")



#Define UI for application that plots features of movies -----------
 ui <- fluidPage(

#    Sidebar layout with a input and output definitions --------------
   sidebarLayout(

#      Inputs: Select variables to plot ------------------------------
     sidebarPanel(


       # # Select variable for x-axis ----------------------------------
       # selectInput(inputId = "x",
       #             label = "X-axis:",
       #             choices = c("Month"),
       #             selected = "Month"),

# Choose color by option ----------------------------------
      selectInput(inputId = "z",
            label = "Theatre:",
            choices = c(unique(bway$Show.Theatre)),
            selected = "Marquis Theatre"),


# Input: Specification of range within an interval ----
sliderInput(inputId = "Date.Year",
            label = "Year Range:",
            min = 2000, max = 2020,
            step = 1,
            sep = "",
            value = c(2008,2012)),

# Select variable for y-axis ----------------------------------
selectInput(inputId = "y",
            label = "Y-axis (Graph on the Right)",
            choices = c("Statistics.Attendance", "Total.Weekly.Gross","Avg..Ticket.Price"),
            selected = "Statistics.Attendance"),
    

# Show data table ---------------------------------------------
    checkboxInput(inputId = "show_data",
              label = "Show data table",
              value = TRUE),
    
     plotOutput(outputId = "barplot")

     ),




#    Output: Show scatterplot --------------------------------------
     mainPanel(
       
       plotlyOutput(outputId = "scatterplot", width = "300%", height = "100%"),
       DT::dataTableOutput(outputId = "bwaytable")
       
     )
   )
 )
#
# # Define server function required to create the scatterplot ---------
 server <- function(input, output) {
   
   
# Create a subset of data filtering by Year Range ------
   bway_subset <- reactive({
     req(input$Date.Year) # ensure availablity of value before proceeding
     bway %>%
       filter(Date.Year >= input$Date.Year[1],
              Date.Year <= input$Date.Year[2], 
              Show.Theatre == input$z)
   })
   
   #Make another subset using the code in the test script for grouped by month averages
   # 
   # bway_subset <- reactive({
   #   req(input$Date.Year) # ensure availablity of value before proceeding
   #   bway %>%
   #     filter(Date.Year >= input$Date.Year[1],
   #            Date.Year <= input$Date.Year[2], 
   #            Show.Theatre == input$z)
   # })
   

#
#   # Create scatterplot object the plotOutput function is expecting --
   output$scatterplot <- renderPlotly({
    
     ggplotly(ggplot(data = bway_subset(), aes_string(x = "Date.Full", 
                                     y = input$y,
                                    color = "Show.Name")) +
      geom_point() +  theme(axis.text.x = element_text(angle = 45))
      
      ) 
     
     
     })
     

       # Create barplot object the plotOutput function is expecting --
     output$barplot <- renderPlot({
      ggplot(data = bway_subset(), aes_string(x ="Show.Name",
                                               fill = "Show.Name" 
                                              )) +
      geom_bar() + labs(x = "Show", y="Number of Weeks") +
      theme(axis.text.x = element_text(angle = 90))
  
   })
   
   
   
   # Print data table if checked -------------------------------------
   output$bwaytable <- DT::renderDataTable(
     if(input$show_data){
       DT::datatable(data = bway_subset()[, 1:6], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)
     }
   )
 }
#
# # Run the application -----------------------------------------------
 shinyApp(ui = ui, server = server)