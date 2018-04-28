library(shiny)
library(rCharts)
scatterbd<-readRDS("scatterbd.rds")
scatterbo<-readRDS("scatterbo.rds")
scatterod<-readRDS("scatterod.rds")

# Define UI ----
ui <- shinyUI(fluidPage(
  
  headerPanel('Gráficos de Dispersão'),
  sidebarPanel(
      selectInput('var', label= 'Selecione o gráfico de dispersão desejado',
                choices = c("Interesse X Casos"=1, 
                            "Despesa X Casos"=2,
                            "Despesa X Interesse"=3),
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
      p <- nPlot(Media ~ Casos, data = scatterbd, type = 'scatterChart')
      p$xAxis(axisLabel = 'Log (Casos Confirmados)')
      p$yAxis(axisLabel = 'Log (Interesse Google Trends)')
      p$chart(forceY = c(0,5))
      p$chart(forceX = c(0,12))
      p$chart(showControls = FALSE)
      p$chart(showLegend = FALSE)
    }
    if(input$var==2){
      p <- nPlot(VLIQ ~ Casos, data = scatterbo, type = 'scatterChart')
      p$xAxis(axisLabel = 'Log (Casos Confirmados)')
      p$yAxis(axisLabel = 'Log (Valor Liquidado)')
      p$chart(forceY = c(8,18))
      p$chart(forceX = c(0,12))
      p$chart(showControls = FALSE)
      p$chart(showLegend = FALSE)
    }
    if(input$var==3){
      p <- nPlot(VLIQ ~ Media, data = scatterod, type = 'scatterChart')
      p$xAxis(axisLabel = 'Log (Interesse Google Trends)')
      p$yAxis(axisLabel = 'Log (Valor Liquidado)')
      p$chart(forceY = c(8,18))
      p$chart(forceX = c(0,5))
      p$chart(showControls = FALSE)
      p$chart(showLegend = FALSE)
    }
    p
     
    
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)