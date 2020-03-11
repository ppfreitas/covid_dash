#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

df <- read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

df <- gather(df,"date","confirmed_cases",5:ncol(df))
df['date'] <- sub("X","", df[,'date'])
df['date'] <- as.Date(df[,'date'],"%m.%d.%y")
df <- df %>% group_by(date,Country.Region) %>% 
  summarise(cases = sum(confirmed_cases)) %>%
  arrange(desc(date))
df_china <- df[df[,'Country.Region'] == "Mainland China",]
df_world <- df[df[,'Country.Region'] != "Mainland China",]
df_world <- df_world %>% group_by(date) %>% 
  summarise(Country.Region = "Ex-China",cases = sum(cases)) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Covid19 confirmed cases by date"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("country",
                       "Country:",
                       c("All",
                         unique(as.character(df$Country.Region))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    column(6,
    DT::dataTableOutput("table")
    )
        # shiny::plotOutput("plot1")
  ),
  p("Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df
    if (input$country != "All") {
      data <- data[data$Country.Region == input$country,]
    }
    else {data <- arrange(df_world, desc(date))}
    data
  }))
  
  # output$plot1 <- shiny::renderPlot({plot(x=iris$Sepal.Length, y=iris$Sepal.Width, 
                                   # xlab="Sepal Length", ylab="Sepal Width",  main="Sepal Length-Width")})
  # output$plot1 <- ggplot(df_world, aes(date,cases)) + geom_line()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

