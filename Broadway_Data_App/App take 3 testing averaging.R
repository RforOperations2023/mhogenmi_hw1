library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
bway <- read.csv("Broadway_Prettified_LFG.csv")


bway <- bway %>% 
  rename( "Show" = "Show.Name",
          "Date" = "Date.Full",
          "Year" = "Date.Year",
          "Month" = "Date.Month",
          "Theatre" = "Show.Theatre",
          "Avg_Attendance" = "Statistics.Attendance",
          "Avg_Capacity" = "Statistics.Capacity",
          "Avg_Gross" = "Statistics.Gross",
          "Type" = "Show.Type"
  )


bway_group_month <- bway %>% 
  group_by(Theatre,Year,Show, Month) %>% 
  summarise(Capacity=mean(Avg_Capacity),
            Attendance=mean(Avg_Attendance),
            Gross=mean(Avg_Gross),
            .groups = 'drop')


#TEST TOMORROW --------------------------
gb_type <- bway %>%
  group_by(Year, Theatre) %>%
  count(Type)
gb_type$fraction <- gb_type$n/ sum(gb_type$n)
gb_type$ymax = cumsum(gb_type$fraction)
gb_type$ymin = c(0, head(gb_type$ymax, n=-1))

# Compute a good label
gb_type$labelPosition <- (gb_type$ymax + gb_type$ymin) / 2
gb_type$label <- paste0(gb_type$Type, "\n (# of weeks): ", gb_type$n)



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
       DT::dataTableOutput(outputId = "bwaytable")
       
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