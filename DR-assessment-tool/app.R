library(shiny)
library(ggplot2)
library(igraph)
library(Rtsne)
library(dslabs)
library(dplyr)
library(plotly)

source("../Algorithms/DR assessment tool algs.R")

load("../Data/MNIST data.Rda")

Z_mst = get_mst(Z_pca)
p = ggplot(df_long, aes(x = x, y = y, color = factor(labels))) +
  geom_point(size = 1) +
  labs(color = "Digit")

ui = fluidPage(
  titlePanel("MNIST"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("from", "From", value = 1, min = 1, max = length(X[,1])),
      numericInput("to", "To", value = 2, min = 1, length(X[,1]))
    ),
    
    mainPanel(
      plotlyOutput("tsnePlot")
    )
  )
)

server = function(input, output) {
  
  output$tsnePlot = renderPlotly({
    
    ggplotly(add_path(p, df_long, input$from, input$to, Z_mst))
  })
}

shinyApp(ui = ui, server = server)
