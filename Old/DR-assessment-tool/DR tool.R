library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

#load("../Data/MNIST data.Rda")
load("../Data/MNIST data manhattan.Rda")

Z = Z_pca
tree = Z_mst
id = subsample

source("../Algorithms/assessment tool.R")

check_inputs(Z, X, tree, labels, id)

max_length = max(E(tree)$weight)

plotting_df = data.frame(x=X[,1], y=X[,2], labels, id)
p = ggplot(plotting_df, aes(x=x, y=y, color=factor(labels), label=id)) +
      geom_point(size=1) +
      labs(title="t-SNE Embedding", color="Class")

ui = fluidPage(
  titlePanel("DR Assessment Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("from", "From id", value = id[1]),
      numericInput("to", "To id", value = id[2]),
      uiOutput("slider"),
      numericInput("k", "k for kNN-density estimate", min = 1, max = dim(Z)[1]/2, value = 1),
      radioButtons("med_subtree",
                   label = "Show medoid subtree?",
                   choices = c("Hide", "Show"),
                   inline = TRUE)
    ),
    
    mainPanel("Plots",
      fluidRow(column(8, plotlyOutput("tsnePlot"))),
      fluidRow(column(6, plotOutput("pathWeights")),
               column(6, plotOutput("pathDensities")))
    )
  )
)

server = function(input, output) {
  
  shortest_path = reactive({
    get_shortest_path(tree, which(id == input$from), which(id == input$to))
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
      ggplotly(plot_medoid_mst(p, plotting_df, Z_dist, tree),
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan')
    }
    else {
      ggplotly(add_path(p, plotting_df, shortest_path(), input$slider),
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan')
    }
  })
  
  output$pathWeights = renderPlot({
    plot_path_weights(shortest_path(), input$slider, max_length)
  })
  
  output$pathDensities = renderPlot({
    #plot_path_densities(Z_dist, shortest_path(), input$k)
    #plot_path_density_cont(Z, shortest_path(), input$k)
    plot_2d_projection(Z, shortest_path(), labels)
  })
}

shinyApp(ui = ui, server = server)
