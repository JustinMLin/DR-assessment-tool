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
      numericInput("from", "From id", value = df_long$id[1]),
      numericInput("to", "To id", value = df_long$id[2]),
      uiOutput("slider")
    ),
    
    mainPanel("Plots",
      fluidRow(column(6, plotlyOutput("tsnePlot"))),
      fluidRow(column(6, plotOutput("pathWeights"))),
      fluidRow(column(6, plotOutput("pathWeightCompare")))
    )
  )
)

server = function(input, output) {
  
  shortest_path = reactive({
    get_shortest_path(Z_mst, which(df_long$id == input$from), which(df_long$id == input$to))
  })
  
  output$slider = renderUI({
    length(get_path_weights(shortest_path()))
    
    sliderInput("slider", 
                "Path component", 
                min = 0, 
                max = length(shortest_path()$vpath) - 1, 
                value = 0,
                step = 1)
  })
  
  output$tsnePlot = renderPlotly({
    ggplotly(add_path(p, df_long, shortest_path(), input$slider),
             tooltip = c("x", "y", "label"))
  })
  
  output$pathWeights = renderPlot({
    path_weights = get_path_weights(shortest_path())
    x = seq(from = 0, to = 10, length.out = length(path_weights))
    
    df = data.frame(x = x, weight = path_weights)
    q = ggplot(df, aes(x = x, y = weight)) + 
          geom_point() +
          geom_line() +
          labs(title = "MST Path Weights", y = "Weight") +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
    
    if (input$slider != 0) {q = q + geom_point(data = df[input$slider,], color = "red")}
    
    print(q)
  })
  
  output$pathWeightCompare = renderPlot({
    path_weights = get_path_weights(shortest_path())
    path_emb_weights = get_emb_path_weights(X, shortest_path())
    
    df = data.frame(path_weights, path_emb_weights)
    
    q = ggplot(df, aes(x = path_weights, y = path_emb_weights)) + 
      geom_point() + 
      labs(title = "Path Weight Comparison", x = "High-Dimensional Space", y = "Visualization Space")
    
    if (input$slider != 0) {q = q + geom_point(data = df[input$slider,], color = "red")}
    
    print(q)
  })
}

shinyApp(ui = ui, server = server)
