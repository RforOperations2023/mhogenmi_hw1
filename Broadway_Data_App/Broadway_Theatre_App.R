library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
bway <- read.csv("Final_Bway.csv")
bway_group_month <- read.csv("Monthly_Avg_Metrics.csv")
gb_type <- read.csv("Donut_Chart_Manipulated.csv")


#Define UI for application that provides Broadway performance metrics by NYC theatres -------------------------------------

ui <- fluidPage(
  
#   Sidebar --------------------------------------------------------------------------------------------------
# Contains a few user input definitions and a bar plot and donut plot  --------------
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Choose a theatre to filter all plots and data by ----------------------------------
      selectInput(inputId = "z",
                  label = "Theatre:",
                  choices = sort(c(unique(bway$Theatre))), #sort dropdown of theatres
                  selected = "Walter Kerr"), #show a selection that has a great breakdown of plays/musicals and shows 
      
      
      # Choose a year to observe --------------------------------------
      sliderInput(inputId = "Year",
                  label = "Year:",
                  min = 2000, max = 2015, #data drops off in quality after 2015 so only giving 2000-2015 options
                  step = 1,
                  sep = "",
                  value = 2008), #choosing a value in the middle for aesthetic purposes
      
      # Select variable for y-axis of scatterplot ----------------------------------
      selectInput(inputId = "y",
                  label = "Y-axis (Graph on the Right):",
                  choices = c("Average Capacity Filled (%)" = "Capacity", 
                              "Average Weekly Total Audience Count" = "Attendance",
                              "Average Weekly Gross" = "Gross"),
                  selected = "Capacity"),
      
      # Show data table -------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
#OUTPUT--------------------------
      
      # Show barplot with number of weeks each show ran in the chosen year and theatre
      plotOutput(outputId = "barplot"),
      
      #Show donut chart with a breakdown of musicals and plays in the chosen year and theatre 
      plotOutput(outputId = "donut")
      
      
    ),
    
    
#Main Panel ------------------------------------------------------------------------------------------------------------------
# OUTPUT contains scatterplot, data table, and download button
  mainPanel(
      
      plotlyOutput(outputId = "scatterplot"),
      DT::dataTableOutput(outputId = "bwaytable"),
      downloadButton(outputId = "download", label = "Download Data")
    )
  )
)
#


# Define server function required to create the plots,data,and download button --------------------------------------------------------
server <- function(input, output) {
  
  
  # Create a subset of data reacting to the Year and Theatre chosen. This is for the bar plot.-------
    bway_subset <- reactive({
     req(input$Year) # ensure availablity of value before proceeding
      bway %>%
        filter(Year == input$Year,
             Theatre == input$z)
  })
  
  #Make another subset reacting to the Year and Theatre chosen grouped by month for the average metric scatterplot----- 
  
    bway_subset_scatter <- reactive({
     req(input$Year)
     bway_group_month %>%
        filter(Year == input$Year,
             Theatre == input$z)
  })
  
  
  #Make another subset reacting to the Year and Theatre chosen aggregated to create the donut chart for show type.------
  
   bway_subset_donut <- reactive({
     req(input$Year) # ensure availablity of value before proceeding
     gb_type %>%
        filter(Year == input$Year,
             Theatre == input$z)
  })
  
  
  # Create scatterplot object the plotOutput function is expecting --
    output$scatterplot <- renderPlotly({
      ggplotly(ggplot(data = bway_subset_scatter(), aes_string(x = "Month", y = input$y,color = c("Show"))) +
         geom_point()+  
         scale_x_discrete(limits=c("January", "February","March","April",'May',"June","July","August",'September',"October","November","December"))+ #rename tick marks
         ggtitle("Monthly Averages")+
         labs(x = "Month", y = input$y)+ #updating labels
               theme_minimal()+
         theme(axis.text.x = element_text(angle = 45)) +  #rotate axis tick labels
         theme(plot.title = element_text(hjust = 0.5))
             
    ) 
    
  })
  
  
  # Create barplot object the plotOutput function is expecting --
    output$barplot <- renderPlot({
      ggplot(data = bway_subset(), aes_string(x ="Show",
                                            fill = "Show" 
    )) +
        geom_bar()+
        labs(x = "Show", y="Number of Weeks") +
        theme(axis.text.x = element_text(angle = 90))+ #rotating axis tick labels 
        ggtitle("Number of Weeks in Theatre")+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Create donut plot object the plotOutput function is expecting --
    output$donut <- renderPlot({
      ggplot(bway_subset_donut(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) + #fill by show type
        geom_rect() +
        coord_polar(theta="y") + 
        xlim(c(2, 4))  + 
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
        ggtitle("Number of Weeks Plays and Musicals Produced by Theatre")+
        theme_void()+ #create minimal view without the coordinates behind plot
        theme(legend.position = "none")+ #remove legend
        theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  #Download Data as Broadway Data + Year Chosen
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
      DT::datatable(data = bway_subset()[, 2:9], #selecting desired columns 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
}


# # Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)