library(plotly)
library(rjson)
library(shinythemes)
data = read.csv('/Users/yuchao/Desktop/visualization/cov.txt')
data$dateRep <- as.Date(ISOdate(data$year,data$month,data$day))
data$dateRep

ui = shinyUI(fluidPage(theme = shinytheme("simplex"),
                       titlePanel(""),
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("dist", 
                                        label = h3("Select"),
                                        choices = list("Cumulative Cases" = 'cases', 
                                                       "Cumulative Deaths" = 'deaths'
                                        ),
                                        
                                        selected = 'cases'),
                           sliderInput("daterange", "Date range:",
                                       min = as.Date("2019-12-31","%Y-%m-%d"),
                                       max = as.Date("2020-05-11","%Y-%m-%d"),
                                       value=as.Date("2020-05-11"),
                                       timeFormat="%Y-%m-%d"),width = 3
                         ),
                         
                         mainPanel(
                           plotlyOutput('distplot'),width = 8),
                         
                       )
)
)

server = shinyServer(function(input, output) {
  
  output$distplot <- renderPlotly({ 
    m = input$dist
    n = input$daterange
    data = subset(data,dateRep <= n )
    countrycode = data$countryterritoryCode
    country = data$countriesAndTerritories
    cases = data[,m]
    data = data.frame(countrycode,country,cases)
    
    data = aggregate(data$cases, by=list(data$countrycode,data$country),sum)
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(data)
    fig <- fig %>% add_trace(
      z = ~x, color = ~x, colors = 'Reds',
      text = ~Group.2, locations = ~Group.1, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Total Cases')
    fig <- fig %>% layout(
      title = '2020 Covid-19 Distribution<br>Source:<a href="https://www.ecdc.europa.eu/en">ECDC</a>',
      geo = g
    )
    fig
  })
  
})

shinyApp(ui = ui, server = server,options = list(width = "100%", height =300))