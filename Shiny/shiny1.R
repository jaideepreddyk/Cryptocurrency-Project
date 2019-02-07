library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyBS)
library(shinyWidgets)
# Define UI

ui <- bootstrapPage(theme = shinytheme('lumen'),
                titlePanel("Cryptocurrency Price Index",windowTitle ="Cryptocurrency Price Index" ),
                absolutePanel(top = '17px',left = '400px',
                      dropdownButton(
             # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("First Cryptocurrency"),
                                choices = c('None',unique(top_25_table$crypto_name)),
                                selected = "Bitcoin"),
                    selectInput(inputId = "type1", label = strong("Second Cryptocurrency"),
                                choices = c('None',unique(top_25_table$crypto_name)),
                                ),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2013-04-28", end = "2017-10-30",
                                   min = "2013-04-28", max = "2017-10-30"),
                    
                    bsButton("act","log scale",icon("refresh"),style = "primary", 
                             type = "toggle",value = F),          
                    radioButtons("rad","Currency:",list("USD","INR")),
                    circle = F, status = "primary", icon = icon("cog"), width = "400px",size = 'sm',
                    tooltip = tooltipOptions(title = "Click to see inputs")
                
                    )),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot",height = '700px',width = '1300px' )
                  )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    
    req(input$date)
    req(input$type)
    # req(input$type1)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    # validate(need(input$type1!=input$type,"Error:Select Different Cryptocurrencies to compare"))
    top_25_table %>%select(crypto_name,Date,Close)%>%
     filter(Date>as.Date(input$date[1],format="%d-%m-%Y") & Date < as.Date(input$date[2],format="%d-%m-%Y"),crypto_name==input$type|crypto_name==input$type1)
  })
  #Create scatterplot object the plotOutput function is expecting
  
  output$lineplot <-  renderPlot({

                         if(input$act==0){
                
    ggplot(selected_trends(),aes(Date,as.numeric(Close),colour=crypto_name)) +
     geom_line()+
      labs (x = "Date", y = "Price") 
                         }
    
                       else{
                         ggplot(selected_trends(),aes(Date,as.numeric(Close),colour=crypto_name)) +
                           geom_line()+ scale_y_log10()+
                           labs (x = "Date", y = "Price (log)", title = "Cryptocurrency Price Over Time")}
    
    
})
}
  
  # Pull in description of trend
  

# Create Shiny object
shinyApp(ui = ui, server = server)