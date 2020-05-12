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


diseases = c('COVID-19','H1N1','SARS','Ebola')
averagedeath = c(1740,743,3.2,5.3)
deaths = c(207094,362000,774,13023)
deathsrate = c(1.4,0.05,9.6,50)
cases = c(3069757,60800000,8096,28646)
countries = c(186,214,26,10)

diseasedata = data.frame(diseases,averagedeath,deaths,deathsrate,cases,countries)
diseasedata[order(deaths),]

ui = shinyUI(fluidPage(theme = shinytheme("simplex"),
          titlePanel("Outbreak Comparsion"),
          sidebarLayout(
            sidebarPanel(
                    radioButtons("dist", 
                                 label = h3("Select Comparsion"),
                                        choices = list("Daily average death" = 'averagedeath', 
                                                    "Deaths" = 'deaths', 
                                                    "Death rate" = 'deathsrate',
                                                    'Total cases' = 'cases',
                                                    'Countries/Regions affected' ='countries'), 
                                        selected = 'deaths'),
                    ),
                         
                     mainPanel(
                       plotlyOutput('distplot')),
                    
                           )
                         )
                       )



server = shinyServer(function(input, output){

  output$distplot <- renderPlotly({ 
    m = input$dist
    disease = diseasedata$diseases
    amount = diseasedata[,m]
    data = data.frame(disease,amount)
    datanew =data[order(amount),]
    p<-ggplot(data=datanew, aes(x=disease, y=amount))+
     geom_bar(stat="identity",width = 0.7,fill = 'steelblue')+scale_y_continuous(labels = comma)+
      xlab("Disease") +
      ylab("Amount") +
     coord_flip()
    
    ggplotly(p)
    
    })
  
})

shinyApp(ui = ui, server = server)
  
  
