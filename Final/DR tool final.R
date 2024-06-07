library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinydashboard)

source("DR tool functions final.R")

run_app = function(Z_dist, X, cluster, id=NULL) {
  if (is.null(id)) {id = 1:nrow(X)}
  
  tree = get_mst(Z_dist)
  
  max_length = max(E(tree)$weight)
  
  plotting_df = data.frame(x=X[,1], y=X[,2], cluster, id, row=1:nrow(X))
  p = ggplot(plotting_df, aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
    geom_point(size=0.5) +
    labs(color="Class")
  
  ui = navbarPage("dashboard", theme=shinytheme("cerulean"),
                  tabPanel("Tab1",
                           sidebarLayout(
                             sidebarPanel(
                               width=3,
                               numericInput("from", "From id", value = id[1]),
                               numericInput("to", "To id", value = id[2]),
                               uiOutput("slider"),
                               radioButtons("med_subtree1",
                                            label = "Show medoid subtree?",
                                            choices = c("Hide", "Show"),
                                            inline = TRUE)
                             ),
                             mainPanel(
                               box(title="Low-Dimensional Embedding",
                                   height=6,
                                   width=6,
                                   plotlyOutput("lowDimPlot")),
                               tabBox(height=6,
                                      width=6,
                                      tabPanel("2D Path Projection", plotlyOutput("projPath")),
                                      tabPanel("Path Weights", plotOutput("pathWeights")))
                             )
                           )
                  ),
                  tabPanel("Tab2",
                           sidebarLayout(
                             sidebarPanel(
                               width=3,
                               actionButton("group1", "Submit Group 1"),
                               actionButton("group2", "Submit Group 2"),
                               actionButton("clear_brush", "Clear Groups"),
                               numericInput("from_brush", "From id", value = id[1]),
                               numericInput("to_brush", "To id", value = id[2]),
                               uiOutput("slider_brush"),
                               radioButtons("med_subtree2",
                                            label = "Show medoid subtree?",
                                            choices = c("Hide", "Show"),
                                            inline = TRUE)
                             ),
                             mainPanel(
                               box(title="Low-Dimensional Embedding",
                                   height=6,
                                   width=6,
                                   plotlyOutput("lowDimPlot_brush")),
                               tabBox(height=6,
                                      width=6,
                                      tabPanel("2D Path Projection", plotlyOutput("projPath_brush")),
                                      tabPanel("Path Weights", plotOutput("pathWeights_brush"))
                                      )
                             )
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
    
    output$lowDimPlot = renderPlotly({
      if (input$med_subtree1 == "Show") {
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
    
    output$projPath = renderPlotly({
      ret = plot_2d_projection(Z, shortest_path(), cluster, id, input$slider)
      
      ggplotly(ret$p,
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan') %>%
        add_annotations(text=paste(round(ret$var_explained, 2)),
                        xref='paper', yref='paper',
                        x=1, y=1,
                        showarrow = FALSE) %>%
        layout(showlegend = FALSE)
    })
    
    output$pathWeights = renderPlot({
      plot_path_weights(shortest_path(), input$slider, max_length)
    })
    
    #######################
    
    shortest_path_brush = reactive({
      get_shortest_path(tree, which(id == input$from_brush), which(id == input$to_brush))
    })
    
    rv = reactiveValues(g1 = NULL, g2 = NULL)
    
    observeEvent(input$group1, {
      d = event_data("plotly_selecting")
      rv$g1 = as.numeric(d$key)
      
      updateNumericInput(inputId="from_brush", value=id[get_medoid(Z_dist, rv$g1)])
    })
    
    observeEvent(input$group2, {
      d = event_data("plotly_selecting")
      rv$g2 = as.numeric(d$key)
      
      updateNumericInput(inputId="to_brush", value=id[get_medoid(Z_dist, rv$g2)])
    })
    
    observeEvent(input$clear_brush, {
      rv$g1 = NULL
      rv$g2 = NULL
      
      updateNumericInput(inputId="from_brush", value=id[1])
      updateNumericInput(inputId="to_brush", value=id[2])
    })
    
    output$slider_brush = renderUI({
      length(get_path_weights(shortest_path_brush()))
      
      sliderInput("slider_brush", 
                  "Path component", 
                  min = 0, 
                  max = length(shortest_path_brush()$vpath) - 1, 
                  value = 0,
                  step = 1)
    })
    
    output$lowDimPlot_brush = renderPlotly({
      if (input$med_subtree2 == "Show") {
        ggplotly(plot_medoid_mst(p, plotting_df, Z_dist, tree),
                 tooltip = c("x", "y", "label")) %>%
          layout(dragmode='pan')
      }
      else {
        alpha_id = unique(c(rv$g1, rv$g2))
        if (!is.null(alpha_id)) {
          alpha = rep(0.3, nrow(X))
          alpha[alpha_id] = 1
        }
        else {
          alpha = rep(1, nrow(X))
        }
        
        p_brush = ggplot(plotting_df, aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
          geom_point(size=0.5, alpha=alpha) +
          labs(color="Class")
        
        ggplotly(add_path(p_brush, plotting_df, shortest_path_brush(), input$slider_brush),
                 tooltip = c("x", "y", "label")) %>%
          layout(dragmode='select') %>%
          event_register("plotly_selecting")
      }
    })
    
    output$projPath_brush = renderPlotly({
      if (is.null(rv$g1) | is.null (rv$g2)) {
        return(plotly_empty())
      }
      
      ret = plot_2d_projection_brush(Z, Z_dist, tree, rv$g1, rv$g2, cluster, id, input$slider_brush)
      
      ggplotly(ret$p,
               tooltip = c("x", "y", "label")) %>%
        layout(dragmode='pan') %>%
        add_annotations(text=paste(round(ret$var_explained, 2)),
                        xref='paper', yref='paper',
                        x=1, y=1,
                        showarrow = FALSE) %>%
        layout(showlegend = FALSE)
    })
    
    output$pathWeights_brush = renderPlot({
      plot_path_weights(shortest_path_brush(), input$slider_brush, max_length)
    })
  }
  
  shinyApp(ui = ui, server = server)
}