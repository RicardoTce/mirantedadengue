library(shiny)
library(dynlm)
library(stargazer)
reg<-readRDS("reg.rds")

# Define UI ----
ui <- fluidPage(
  
  headerPanel('Resultados de Regressão'),
  sidebarPanel(
      selectInput('var', label= 'Selecione o modelo desejado',
                choices = c("Despesa (Y), Casos de dengue (X)"=1, 
                            "Casos de dengue (Y), Interesse (X)"=2),
                selected = 1),
      mainPanel(
        br(),
        br(),
        uiOutput("myreg",position="right")
      )
  ))
 


# Define server logic ----
server = function(input, output) {
  output$myreg <- renderUI({
    if(input$var==1){
      score<-dynlm(VLIQ~Casos,data=reg)
    }
    if(input$var==2){
      score<-dynlm(Casos~Media,data=reg)
    }
    return(HTML((stargazer(score,type="html"))))
    
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)