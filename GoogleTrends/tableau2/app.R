library(shiny)
library(shinyjs)

# Define UI ----
ui = fluidPage(
  
  useShinyjs(rmd = TRUE),
    tags$head(tags$script(src="https://public.tableau.com/javascripts/api/tableau-2.min.js")),
  extendShinyjs(text = "shinyjs.initViz = function(){
                var containerDiv = document.getElementById('viz1524066124696');
                url = 'https://public.tableau.com/views/DengueBH/MapadaDengue-CasosConfirmadosemBeloHorizonte?:embed=y&:display_count=yes&publish=yes';
                viz = new tableau.Viz(containerDiv, url);
                }"),
  
  tags$div(id = 'viz1524066124696')
  )
  
 # Define server logic ----
server = function(input, output, session){js$initViz()}
 

# Run the app ----
shinyApp(ui = ui, server = server)