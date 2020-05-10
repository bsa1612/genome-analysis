
library(shiny)
library(dplyr)
library(tidyr)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID19 Time Series Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select_country",
                        label = "Country",
                        choices = list("China","Japan","US","Italy","Iran"))
           
        ),
            selectInput("report_type",
                        label ="Report Type",
                        choices = list("confirmed","deaths","recovered"))
    ),
  

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("output_country"),
            textOutput("output_report"),
            plotOutput("country_data"),
           
          
        )
    )


library(ggplot2)
global_data<- covid19_data%>%
    group_by(country,date)%>%
    summarize_at(c("confirmed","deaths","recovered"),sum)%>%
    filter(country%in%c("China","US","Japan","Italy","Iran"))%>%
    select(date,country,recovered,deaths,confirmed)

# Define server logic required to draw a histogram
server <- function(input, output){
    output$output_country <-renderText({
        paste("Country selected",input$select_country)
    })
    output$output_report <-renderText({
        paste("Report Type",input$report_type)
    })

    output$country_data <- renderPlot({
            ggplot(global_data, aes_string(x=date))+
            geom_point(global_data,aes_string(y= input$select_country))+
            geom_line()

    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
