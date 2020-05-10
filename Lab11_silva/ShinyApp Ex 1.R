library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load data from Arctic Data Center

data_time <- time_series_long_joined %>% 
  group_by(Country_Region,Date) %>% 
  summarise_at(c("Confirmed", "Deaths", "Recovered"), sum)%>%
  filter(Country_Region %in% c("US","China","Japan","Italy","Iran"))



data_min<-min(data_time$Date,na.rm = TRUE)
data_max<-max(data_time$Date,na.rm = TRUE)


# Define UI for application that draws two scatter plots
ui <- fluidPage(
  
  # Application title
  titlePanel("Exercise 1"),
  p("Data for this application are from: "),
  tags$ul(
    tags$li("JHU GitHub repository",
            tags$a("doi:10.18739/A25T3FZ8X", href="https://github.com/CSSEGISandData/COVID-19")
    )
  ),
  tags$br(),
  tags$hr(),
  
  verticalLayout(
    # Sidebar with a slider input for depth axis
    sidebarLayout(
      sidebarPanel(
        
        sliderInput("Date",
                    "Date:",
                    min = data_min,
                    max = data_max,
                    value = c(data_min,data_max))
        
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("datePlot")
      )
    ),
    
    tags$hr()
    
    
      
      
     
  )
)

# Define server logic required to draw both plots
server <- function(input, output) {
  
  output$datePlot <- renderPlot({
    ggplot(data_time, mapping = aes(x = Date, y = Confirmed,color=Country_Region)) +
      geom_point(size=3) +
      geom_line()+
      xlim(input$Date) +
      theme_light()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
