library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
bway <- read.csv("Broadway_Prettified_LFG.csv")

bway_group_month <- bway %>% 
  group_by(Show.Theatre,Date.Year,Show.Name, Date.Month) %>% 
  summarise(mean_capacity=mean(Statistics.Capacity),
            mean_attendance=mean(Statistics.Attendance),
            mean_gross=mean(Statistics.Gross),
            .groups = 'drop')


#TEST TOMORROW --------------------------
gb_type <- bway %>%
  group_by(Date.Year, Show.Theatre) %>%
  count(Show.Type)
gb_type$fraction <- gb_type$n/ sum(gb_type$n)
gb_type$ymax = cumsum(gb_type$fraction)
gb_type$ymin = c(0, head(gb_type$ymax, n=-1))



#Define UI for application that plots features of movies -----------
 ui <- fluidPage(

#    Sidebar layout with a input and output definitions --------------
   sidebarLayout(

#      Inputs: Select variables to plot ------------------------------
     sidebarPanel(


# Choose color by option ----------------------------------
      selectInput(inputId = "z",
            label = "Theatre:",
            choices = c(unique(bway$Show.Theatre)),
            selected = "Marquis Theatre"),


# Input: Specification of range within an interval ----
sliderInput(inputId = "Date.Year",
            label = "Year:",
            min = 2000, max = 2020,
            step = 1,
            sep = "",
            value = 2010),

# Select variable for y-axis ----------------------------------
selectInput(inputId = "y",
            label = "Y-axis (Graph on the Right)",
            choices = c("mean_capacity", "mean_attendance","mean_gross"),
            selected = "mean_capacity"),
    

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
       filter(Date.Year == input$Date.Year,
              Show.Theatre == input$z)
   })
   
#Make another subset using the code in the test script for grouped by month averages

   bway_subset_scatter <- reactive({
     req(input$Date.Year) # ensure availablity of value before proceeding
     bway_group_month %>%
       filter(Date.Year == input$Date.Year,
              Show.Theatre == input$z)
   })
   
   
# #Make another subset using the code in the test script for donut chart for show type
#    
   bway_subset_donut <- reactive({
     req(input$Date.Year) # ensure availablity of value before proceeding
     gb_type %>%
       filter(Date.Year == input$Date.Year,
              Show.Theatre == input$z)
   })

#
#   # Create scatterplot object the plotOutput function is expecting --
   output$scatterplot <- renderPlotly({
    
     ggplotly(ggplot(data = bway_subset_scatter(), aes_string(x = "Date.Month", 
                                     y = input$y,
                                    color = "Show.Name")) +
      geom_point() +  theme(axis.text.x = element_text(angle = 45)) + 
        scale_x_discrete(limits=c("January", "February","March","April",'May',"June","July","August",'September',"October","November","December"))
      
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
     
     # PUT THIS IN A RENDERPLOT TOMORROW FOR THE DONUT CHART
     output$donut <- renderPlot({
     ggplot(bway_subset_donut(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Show.Type)) +
       geom_rect() +
       coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
       xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
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