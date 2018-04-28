library(shiny)
library(rCharts)
bd<-readRDS("bd.rds")
odchart<-readRDS("od.rds")
gtmed<-readRDS("gtmed.rds")

# Define UI ----
ui <- shinyUI(fluidPage(
  
  headerPanel('Séries de Tempo'),
  sidebarPanel(
      selectInput('var', label= 'Selecione a série desejada',
                choices = c("Despesa"=1, 
                            "Casos de dengue"=2,
                            "Interesse ao longo do tempo"=3),
                selected = 1),
      mainPanel(
        showOutput("myChart", "nvd3"))
      )
  )
  )

# Define server logic ----
server = function(input, output) {
  output$myChart <- renderChart2({
    if(input$var==1){
      q <- nPlot(VLIQ ~ Data, data = odchart, type = 'lineChart')
      q$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
      q$chart(forceY = c(0,4000))
      q$yAxis(axisLabel = 'Valor Liquidado - R$ mil',width=60)
      q$chart(showLegend = FALSE)
    }
    if(input$var==2){
      q <- nPlot(Casos ~ Data, data = bd, type = 'lineChart')
      q$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
      q$chart(forceY = c(0,60000))
      q$yAxis(axisLabel = 'Casos Confirmados de Dengue',width=60)
      q$chart(showLegend = FALSE)
    }
    if(input$var==3){
      q <- nPlot(Media ~ Data, data = gtmed, type = 'lineChart')
      q$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
      q$chart(forceY = c(0,100))
      q$yAxis(axisLabel = 'Google Trends - Interesse Sintomas Dengue',width=60)
      q$chart(showLegend = FALSE)
    }
    q
     
    
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)