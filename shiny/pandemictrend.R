library(shiny)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)
library(DT)
library(r2d3)
library(webshot)
library(htmlwidgets)
library(memor)
library(shinyjs)
library(shinythemes)
library(datasets)
library(ggplot2)
library(scales)
library(plotly)
data = read.csv('cov.txt')
data$dateRep <- as.Date(ISOdate(data$year,data$month,data$day))

data = data[order(data$dateRep,decreasing =FALSE),]
m = group_by(data,countriesAndTerritories)
coutry_name = c('China','Spain','Italy','South_Korea','Iran','France','Denmark',
                'United_States_of_America')

ui = shinyUI(fluidPage(theme = shinytheme("simplex"),
                       titlePanel("Epidemic trend"),
                       
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "countries",
                             label = "Countries:",
                             choices = c('World',coutry_name),
                             size = 9,selectize = FALSE,
                             selected = 'World'),
                           
                           radioButtons("dist", 
                                        label = h3(""),
                                        choices = list("Daily" = 'Cases(Daily)', 
                                                       "Cumulative" = 'Cases(cumulative)',
                                                       'Percentage increase' = 'Percentage increase'
                                        ),
                                        
                                        selected = 'Cases(Daily)'),
                           
                           sliderInput("daterange", "Date range:",
                                       min = as.Date("2019-12-31","%Y-%m-%d"),
                                       max = as.Date("2020-05-11","%Y-%m-%d"),
                                       value=as.Date("2020-05-11"),
                                       timeFormat="%Y-%m-%d")
                         ),
                         
                         mainPanel(
                           tabsetPanel(type = "tabs",
                                       tabPanel("Cases", plotlyOutput('distplot')),
                                       tabPanel("Deaths", plotlyOutput('distplott'))
                           )
                         )
                       )  
) 
)




server = shinyServer(function(input, output) {
  
  
  # Return the formula text for printing as a caption
  output$distplot <- renderPlotly({
    daterange = input$daterange
    data = filter(data, dateRep >= "2019-12-31",dateRep <= daterange)
    dayrange = ceiling(as.integer(diff(range(as.Date(data$dateRep))))/10)
    dayrange = str_c(dayrange,' days')
    
    
    if(input$countries == 'World'){
      
      if(input$dist == 'Cases(Daily)'){
        n = aggregate(data$cases, by=list(data$dateRep),sum)
        cases = n$x
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep,cases)}
      
      else if(input$dist == 'Cases(cumulative)'){
        n = aggregate(data$cases, by=list(data$dateRep),sum)
        cases = cumsum(n$x)
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep, cases)}
      
      else if(input$dist == 'Percentage increase'){
        data = data[order(data$dateRep,decreasing =FALSE),]
        n = aggregate(data$cases, by=list(data$dateRep),sum)
        casesum = cumsum(n$x)
        case = n$x
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep, casesum,case)
        
        for (i in 2:nrow(worlddata)){
          worlddata$cases[1] = 0
          worlddata$cases[i] = worlddata$case[i]/worlddata$casesum[i-1]
        }
        
        worlddata$cases[!is.finite(worlddata$cases)] <- 0
      }
      
      v = ggplot(data=worlddata, aes(x=dateRep, y=cases)) +
        geom_line(color="#D16103")+geom_point(color="#D16103",size = 1)+
        scale_x_datetime(labels = date_format("%m-%d"),date_breaks = dayrange)+
        theme_minimal()+xlab("Date") + scale_y_continuous(labels = scales::comma)+
        ylab(input$dist)
      ggplotly(v)
    }
    
    
    
    else{
      if(input$dist == 'Cases(Daily)'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
        
      }
      else if(input$dist == 'Cases(cumulative)'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        countrydata$cases = cumsum(countrydata$cases)
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
      }
      
      else if(input$dist == 'Percentage increase'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        
        countrydata$casesum = cumsum(countrydata$cases)
        
        for (i in 2:nrow(countrydata)){
          countrydata$cases[i] = countrydata$cases[i]/countrydata$casesum[i-1]
        }
        
        countrydata$cases[!is.finite(countrydata$cases)] <- 0
        
        
        
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
        
        dayrange = ceiling(as.integer(diff(range(as.Date(countrydata$dateRep))))/10)
        dayrange = str_c(dayrange,' days')
      }
      v = ggplot(data=countrydata, aes(x=dateRep, y=cases, group=1)) +
        geom_line(color="#D16103")+geom_point(color="#D16103",size = 1)+
        scale_x_datetime(labels = date_format("%m-%d"),date_breaks = dayrange)+
        theme_minimal()+xlab("Date") + scale_y_continuous(labels = scales::comma)+
        ylab(input$dist)
      ggplotly(v)}
  })
  
  
  
  
  
  
  output$distplott <- renderPlotly({
    daterange = input$daterange
    data = filter(data, dateRep >= "2019-12-31",dateRep <= daterange)
    dayrange = ceiling(as.integer(diff(range(as.Date(data$dateRep))))/10)
    dayrange = str_c(dayrange,' days')
    
    
    if(input$countries == 'World'){
      if(input$dist == 'Cases(Daily)'){
        n = aggregate(data$deaths, by=list(data$dateRep),sum)
        deaths = n$x
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep, deaths)}
      
      else if(input$dist == 'Cases(cumulative)'){
        n = aggregate(data$deaths, by=list(data$dateRep),sum)
        deaths = cumsum(n$x)
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep, deaths)}
      
      else if(input$dist == 'Percentage increase'){
        data = data[order(data$dateRep,decreasing =FALSE),]
        n = aggregate(data$deaths, by=list(data$dateRep),sum)
        deathsum = cumsum(n$x)
        deaths = n$x
        dateRep = as.POSIXct(n$Group.1)
        worlddata = data.frame(dateRep, deathsum,deaths)
        
        for (i in 2:nrow(worlddata)){
          worlddata$deaths[i] = worlddata$deaths[i]/worlddata$deathsum[i-1]
        }
        
        worlddata$deaths[!is.finite(worlddata$deaths)] <- 0
      }
      
      v = ggplot(data=worlddata, aes(x=dateRep, y=deaths)) +
        geom_line(color="#999999")+geom_point(color="#999999",size = 1)+
        scale_x_datetime(labels = date_format("%m-%d"),date_breaks = dayrange)+
        theme_minimal()+xlab("Date") + scale_y_continuous(labels = scales::comma)+
        ylab(input$dist)
      ggplotly(v)
    }
    
    
    
    else{
      if(input$dist == 'Cases(Daily)'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
        
        
      }
      else if(input$dist == 'Cases(cumulative)'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        countrydata$deaths = cumsum(countrydata$deaths)
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
      }
      
      else if(input$dist == 'Percentage increase'){
        countrydata = subset(data,countriesAndTerritories == input$countries)
        
        countrydata$deathsum = cumsum(countrydata$deaths)
        
        for (i in 2:nrow(countrydata)){
          countrydata$deaths[i] = countrydata$deaths[i]/countrydata$deathsum[i-1]
        }
        
        countrydata$deaths[!is.finite(countrydata$deaths)] <- 0
        
        
        
        countrydata$dateRep = as.POSIXct(countrydata$dateRep)
      }
      v = ggplot(data=countrydata, aes(x=dateRep, y=deaths, group=1)) +
        geom_line(color="#999999")+geom_point(color="#999999",size = 1)+
        scale_x_datetime(labels = date_format("%m-%d"),date_breaks = dayrange)+
        theme_minimal()+xlab("Date") + scale_y_continuous(labels = scales::comma)+
        ylab(input$dist)
      ggplotly(v)}
    
  })
}) 

shinyApp(ui = ui, server = server)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
