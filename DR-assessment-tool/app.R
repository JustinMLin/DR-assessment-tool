library(shiny)
library(ggplot2)
library(igraph)
library(Rtsne)
library(dslabs)
library(dplyr)
library(plotly)

source("../Algorithms/DR assessment tool algs.R")

load("../Data/MNIST data.Rda")

#g = Z_mst
#g = Z_nng
g = Z_sl

p = ggplot(df_long, aes(x = x, y = y, color = factor(labels), label = id)) +
   geom_point(size = 1) +
   labs(title = "t-SNE embedding of MNIST dataset", color = "Digit")
  
ui = fluidPage(
  titlePanel("DR Assessment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("from", "From id", value = df_long$id[1]),
      numericInput("to", "To id", value = df_long$id[2]),
      numericInput("digit_id", "Digit id", value = df_long$id[1]),
      uiOutput("slider"),
      radioButtons("med_mst",
                   label = "Show Medoid MST?",
                   choices = c("Hide", "Show"),
                   inline = TRUE)
    ),
    
    mainPanel("Plots",
      fluidRow(column(8, plotlyOutput("tsnePlot")),
               column(4, plotOutput("digitImage"))),
      fluidRow(column(6, plotOutput("pathWeights")),
               column(6, plotOutput("pathWeightCompare")))
    )
  )
)

server = function(input, output) {
  
  shortest_path = reactive({
    get_shortest_path(g, which(df_long$id == input$from), which(df_long$id == input$to))
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
    if (input$med_mst == "Show") {
      ggplotly(plot_medoid_mst(p, df_long, Z_pca, g, df_long$labels),
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan')
    }
    else {
      ggplotly(add_path(p, df_long, shortest_path(), input$slider),
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan')
    }
  })
  
  output$digitImage = renderPlot({
    view_image(data, input$digit_id)
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
