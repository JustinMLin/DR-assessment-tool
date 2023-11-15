library(shiny)
library(ggplot2)
library(igraph)
library(Rtsne)
library(dslabs)
library(dplyr)
library(plotly)

source("../Algorithms/DR assessment tool algs.R")

load("../Data/MNIST data.Rda")

Z_mst = g
p = ggplot(df_long, aes(x = x, y = y, color = factor(labels), label = id)) +
   geom_point(size = 1) +
   labs(title = "t-SNE embedding of MNIST dataset", color = "Digit")
  
ui = fluidPage(
  titlePanel("DR Assessment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("from", "From id", value = 1, min = 1, max = length(X[,1])),
      numericInput("to", "To id", value = 2, min = 1, length(X[,1]))
    ),
    
    mainPanel("Plots",
      fluidRow(column(6, plotlyOutput("tsnePlot"))),
      fluidRow(column(6, plotOutput("pathWeights"))),
      fluidRow(column(6, plotOutput("pathWeightCompare")))
    )
  )
)

server = function(input, output) {
  
  output$tsnePlot = renderPlotly({
    ggplotly(add_path(p, df_long, input$from, input$to, Z_mst),
             tooltip = c("x", "y", "label"))
  })
  
  output$pathWeights = renderPlot({
    path_weights = get_path_weights(get_shortest_path(g, input$from, input$to))
    x = seq(from = 0, to = 10, length.out = length(path_weights))
    
    data.frame(x = x, weight = path_weights) %>%
      ggplot(aes(x = x, y = weight)) + 
        geom_point() + 
        geom_line() +
        labs(title = "MST Path Weights", y = "Weight") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
  })
  
  output$pathWeightCompare = renderPlot({
    path_weights = get_path_weights(get_shortest_path(g, input$from, input$to))
    path_emb_weights = get_emb_path_weights(X, get_shortest_path(g, input$from, input$to))
    
    data.frame(path_weights, path_emb_weights) %>%
      ggplot(aes(x = path_weights, y = path_emb_weights)) + 
      geom_point() + 
      labs(title = "Path Weight Comparison", x = "High-Dimensional Space", y = "Visualization Space")
  })
}

shinyApp(ui = ui, server = server)
