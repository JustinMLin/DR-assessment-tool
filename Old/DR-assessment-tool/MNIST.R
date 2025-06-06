library(shiny)
library(ggplot2)
library(igraph)
library(Rtsne)
library(dslabs)
library(dplyr)
library(plotly)

source("../Algorithms/assessment tool.R")

load("../Data/avg-linkage data.Rda")

Z_dist = dist(Z_pca)

g = Z_mst
# g = Z_sl

p = ggplot(df_long, aes(x = x, y = y, color = factor(labels), label = id)) +
   geom_point(size = 1) +
   labs(title = "t-SNE embedding of MNIST dataset", color = "Digit")

view_image = function(data, id) {
  image(1:28,1:28, matrix(data$images[id,],nrow=28)[,28:1], col=gray(seq(0,1,0.05)), xlab="", ylab="")
}
  
ui = fluidPage(
  titlePanel("DR Assessment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("from", "From id", value = df_long$id[1]),
      numericInput("to", "To id", value = df_long$id[2]),
      numericInput("digit_id", "Digit id", value = df_long$id[1]),
      uiOutput("slider"),
      numericInput("k", "k for kNN-density estimate", min = 1, max = 1000, value = 30),
      radioButtons("med_subtree",
                   label = "Show medoid subtree?",
                   choices = c("Hide", "Show"),
                   inline = TRUE)
    ),
    
    mainPanel("Plots",
      fluidRow(column(8, plotlyOutput("tsnePlot")),
               column(4, plotOutput("digitImage"))),
      fluidRow(column(6, plotOutput("pathWeights")),
               column(6, plotOutput("pathDensities"))),
      fluidRow(column(6, plotOutput("pathWeightCompare")))
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
    if (input$med_subtree == "Show") {
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
    plot_path_weights(shortest_path(), input$slider)
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
  
  output$pathDensities = renderPlot({
    plot_path_densities(Z_dist, shortest_path(), input$k)
  })
}

shinyApp(ui = ui, server = server)
