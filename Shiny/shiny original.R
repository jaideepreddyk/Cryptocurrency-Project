library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyBS)
# library(lubridate)
 # source('C:\\Users\\Jaideep Reddy\\Desktop\\INSOFE\\Crypto Proj\\Shiny\\top_25.R')
 # top_25_table = top_25()
# Define UI
ui <- fluidPage(theme = shinytheme("paper"),
                titlePanel("Cryptocurrency Price Index"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "firstcrypto", label = strong("First Cryptocurrency"),
                                choices = unique(top_25_table$crypto_name),
                                selected = "Bitcoin"),
                    selectInput(inputId = "secondcrypto", label = strong("Second Cryptocurrency"),
                                choices = c('None',unique(top_25_table$crypto_name)),
                                ),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2013-04-28", end = unique(max(top_25_table$Date)),
                                   min = "2013-04-28", max = unique(max(top_25_table$Date))),
                    bsButton("act","log scale",icon("refresh"),style = "primary", 
                             type = "toggle",value = F),
                    
                    radioButtons("rad","Currency:",list("USD","INR")),
                    width = 3
                   
                    ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "700px",dblclick = "plot1_dblclick",width = "1000px",
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE
                               ))
                
                )
)
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    
    req(input$date)
    req(input$firstcrypto)
    req(input$secondcrypto)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
   validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
   # validate(need(input$type1!=input$type,"Error:Select Different Cryptocurrencies to Compare"))
    top_25_table %>%select(crypto_name,Date,Close,Close_INR)%>%
     filter(Date>=as.Date(input$date[1],format="%d-%m-%Y") & Date <= as.Date(input$date[2],format="%d-%m-%Y"),crypto_name==input$firstcrypto|crypto_name==input$secondcrypto)
  })
  #Create scatterplot object the plotOutput function is expecting
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  
  output$lineplot <-  renderPlot({
    if(input$rad=="INR")
    {
      price=as.numeric(selected_trends()$Close_INR)
      label1="Price (INR)"
      label2="Price (INR) Log Scale"
    }
    else{
      price=as.numeric(selected_trends()$Close)
      label1="Price (USD)"
      label2="Price (USD) Log Scale"
    }
    
    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    if(input$act==0){
       ggplot(selected_trends(),aes(Date,price,colour=crypto_name)) +
        geom_line()+coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
        labs (x = "Date", y = label1, title = "Cryptocurrency Price Over Time") 
    }
    
    else{
      ggplot(selected_trends(),aes(Date,price,colour=crypto_name)) +
        geom_line()+ scale_y_log10()+ coord_cartesian(xlim =ranges$x, ylim = ranges$y, expand = FALSE)+
        labs (x = "Date", y = label2, title = "Cryptocurrency Price Over Time")}
    
    
  })
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin,brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
}

  # Pull in description of trend

# Create Shiny object
shinyApp(ui = ui, server = server)