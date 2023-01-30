library(shiny)
library(ggplot2)
library(dplyr)
bway <- read.csv("Broadway_Prettified.csv")
bway$Month_<- as.integer(factor(bway$Month, levels = month.name))
bway$Avg..Ticket.Price<-as.integer(bway$Avg..Ticket.Price)

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
            choices = c(unique(bway$Theatre)),
            selected = "Marquis Theatre"),


# Input: Specification of range within an interval ----
      sliderInput(inputId = "year_range",
            label = "Year Range:",
            min = 2000, max = 2020,
            step = 1,
            sep = "",
            value = c(2008,2012)),

# Select variable for y-axis ----------------------------------
selectInput(inputId = "y",
            label = "Y-axis (Graph on the Right)",
            choices = c("Seats.Sold", "Total.Weekly.Gross","Avg..Ticket.Price"),
            selected = "Seats.Sold"),
    

# Show data table ---------------------------------------------
    checkboxInput(inputId = "show_data",
              label = "Show data table",
              value = TRUE),
    
     plotOutput(outputId = "barplot")

     ),




#    Output: Show scatterplot --------------------------------------
     mainPanel(
       
       plotOutput(outputId = "scatterplot" , width = 600, height = 400),
       DT::dataTableOutput(outputId = "bwaytable", width = "80%")
       
     )
   )
 )
#
# # Define server function required to create the scatterplot ---------
 server <- function(input, output) {
   
   
# Create a subset of data filtering by Year Range ------
   bway_subset <- reactive({
     req(input$year_range) # ensure availablity of value before proceeding
     bway %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2], Theatre == input$z)
   })
#
#   # Create scatterplot object the plotOutput function is expecting --
   output$scatterplot <- renderPlot({
     ggplot(data = bway_subset(), aes_string(x = "Month_", 
                                    y = input$y,
                                    color = "Show.Title")) +
      geom_line(stat="summary", fun.y="mean")
     
     })
     

       # Create barplot object the plotOutput function is expecting --
     output$barplot <- renderPlot({
      ggplot(data = bway_subset(), aes_string(x ="Show.Title",
                                               fill = "Show.Title" 
                                              )) +
      geom_bar() + labs(x = "Show", y="Number of Weeks") +
      theme(axis.text.x = element_text(angle = 90))
      #geom_segment(aes(x = "Show.Title", xend= "Show.Title", 0, yend= "count"))
       
     
  
   }) 
   
   
   
   # Print data table if checked -------------------------------------
   output$bwaytable <- DT::renderDataTable(
     if(input$show_data){
       DT::datatable(data = bway_subset()[, 2:8], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)
     }
   )
 }
#
# # Run the application -----------------------------------------------
 shinyApp(ui = ui, server = server)