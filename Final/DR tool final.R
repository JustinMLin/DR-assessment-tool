library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(bslib)
library(stringr)

source("/Users/justinlin/Desktop/Research/DR-assessment-tool/Final/DR tool functions final.R")

run_app = function(Z, X, cluster, id=NULL) {
  Z_dist = unname(dist(Z))
  X = unname(X)

  cluster = as.integer(as.factor(rank(cluster, ties.method="min")))

  if (is.null(id)) {id = 1:nrow(X)}

  tree = get_mst(Z_dist)

  max_length = max(E(tree)$weight)

  plotting_df = data.frame(x=X[,1], y=X[,2], cluster, id, row=1:nrow(X))
  p = ggplot(plotting_df, aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
    geom_point(size=0.3) +
    labs(color="Class")
  medoid_p = plot_medoid_mst(p, plotting_df, Z_dist, tree)

  ui = page_navbar(
    title="Dimension Reduction Tool",
    theme=bs_theme(bootswatch="cosmo"),
    fillable=FALSE,
    nav_panel(
      title="Default Clusters",

      layout_sidebar(
        sidebar=sidebar(
          open="always",
          accordion(
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            accordion_panel(
              "Path Selection",
              style="background-color: #f2f2f2",
              numericInput("from", "From ID", value = 0),
              numericInput("to", "To ID", value = 0)
            ),
            accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              numericInput("dim", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              sliderInput("degree", "CCA Degree", min=2, max=10, value=2, step=1),
              sliderInput("adjust", "Bandwidth Adjustment", min=0, max=5, value = 0, step = .05),
              radioButtons("show_all_edges",
                           label = "Show all MST edges?",
                           choices = c("Hide", "Show"),
                           inline = TRUE)
            )
          ),
          radioButtons("med_subtree1",
                       label = "Show medoid subtree?",
                       choices = c("Hide", "Show"),
                       inline = TRUE),
          uiOutput("slider")
        ),

        card(
          card_header("Low-Dimensional Embedding"),
          plotlyOutput("lowDimPlot")
        ),

        navset_card_underline(
          title="Analytical Plots",
          nav_panel("2D Path Projection", plotlyOutput("projPath")),
          nav_panel("Path Weights", plotOutput("pathWeights"))
        )
      )
    ),

    nav_panel(
      title="Custom Clusters",
      layout_sidebar(
        sidebar=sidebar(
          open="always",
          accordion(
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            accordion_panel(
              "Group Selection",
              style="background-color: #f2f2f2",
              actionButton("group1", "Submit Group 1",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              actionButton("group2", "Submit Group 2",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              actionButton("clear_brush", "Clear Groups",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              numericInput("from_brush", "From ID", value = 0),
              numericInput("to_brush", "To ID", value = 0)
            ),
            accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              numericInput("dim_brush", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              sliderInput("degree_brush", "CCA Degree", min=2, max=10, value=2, step=1),
              sliderInput("adjust_brush", "Bandwidth Adjustment", min=0, max=5, value = 0, step = .05),
              radioButtons("show_all_edges_brush",
                           label = "Show all MST edges?",
                           choices = c("Hide", "Show"),
                           inline = TRUE),
              radioButtons("path_color_brush",
                           label="Path Projection Coloring",
                           choices=c("Original Coloring", "Group Coloring"),
                           selected="Original Coloring")
            )
          ),
          radioButtons("med_subtree2",
                       label = "Show medoid subtree?",
                       choices = c("Hide", "Show"),
                       inline = TRUE),
          uiOutput("slider_brush")
        ),

        card(
          card_header("Low-Dimensional Embedding"),
          plotlyOutput("lowDimPlot_brush")
        ),

        navset_card_underline(
          title="Analytical Plots",
          nav_panel("2D Path Projection", plotlyOutput("projPath_brush")),
          nav_panel("Path Weights", plotOutput("pathWeights_brush"))
        )
      )
    )
  )

  server = function(input, output) {
    shortest_path = reactive({
      sp = tryCatch({
        get_shortest_path(tree, which(id == input$from), which(id == input$to))
      }, error = function(err) {
        return(NULL)
      })

      sp
    })

    projected_pts = reactive({
      if (is.null(shortest_path())) NULL
      else {
        get_projection(Z, shortest_path(), cluster, input$dim, input$degree)
      }
    })

    output$slider = renderUI({
      max = ifelse(is.null(shortest_path()),
                   0,
                   length(shortest_path()$vpath) - 1)

      sliderInput("slider",
                  "Path component",
                  min = 0,
                  max = max,
                  value = 0,
                  step = 1)
    })

    output$lowDimPlot = renderPlotly({
      if (input$med_subtree1 == "Show") {
        ggplotly(medoid_p,
                 tooltip = c("x", "y", "label")) %>%
          layout(dragmode='pan')
      }
      else {
        if (is.null(shortest_path())) {
          ggplotly(p,
                   tooltip = c("x", "y", "label")) %>%
            layout(dragmode='pan')
        }
        else {
          ggplotly(add_path(p, plotting_df, shortest_path(), input$slider),
                   tooltip = c("x", "y", "label")) %>%
            layout(dragmode='pan')
        }
      }
    })

    output$projPath = renderPlotly({
      if (is.null(projected_pts())) {
        return(plotly_empty(type="scatter", mode="markers"))
      }

      ret = plot_2d_projection(tree, cluster, projected_pts()$projected_pts,
                               projected_pts()$ids, projected_pts()$path_ids,
                               projected_pts()$var_explained, input$degree,
                               input$slider, input$adjust, input$show_all_edges)

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
      if (is.null(shortest_path())) {
        return(plotly_empty(type="bar"))
      }

      plot_path_weights(shortest_path(), input$slider, max_length)
    })

    #######################

    shortest_path_brush = reactive({
      sp = tryCatch({
        get_shortest_path(tree,
                          which(id == input$from_brush),
                          which(id == input$to_brush))
      }, error = function(err) {
        return(NULL)
      })

      sp
    })

    rv = reactiveValues(g1 = NULL, g2 = NULL)

    projected_pts_brush = reactive({
      if (is.null(shortest_path_brush())) NULL
      else {
        get_projection_brush(Z, shortest_path_brush(), rv$g1, rv$g2, cluster, input$dim_brush, input$degree_brush)
      }
    })

    observeEvent(input$group1, {
      d = event_data("plotly_selecting")
      rv$g1 = as.numeric(d$key)

      if (length(rv$g1) > 0) {
        updateNumericInput(inputId="from_brush", value=id[get_medoid(Z_dist, rv$g1)])
      } else rv$g1 = NULL
    })

    observeEvent(input$group2, {
      d = event_data("plotly_selecting")
      rv$g2 = as.numeric(d$key)

      if (length(rv$g2) > 0) {
        updateNumericInput(inputId="to_brush", value=id[get_medoid(Z_dist, rv$g2)])
      } else rv$g2 = NULL
    })

    observeEvent(input$clear_brush, {
      rv$g1 = NULL
      rv$g2 = NULL

      updateNumericInput(inputId="from_brush", value=0)
      updateNumericInput(inputId="to_brush", value=0)
    })

    output$slider_brush = renderUI({
      max = ifelse(is.null(shortest_path_brush()),
                   0,
                   length(shortest_path_brush()$vpath) - 1)

      sliderInput("slider_brush",
                  "Path component",
                  min = 0,
                  max = max,
                  value = 0,
                  step = 1)
    })

    output$lowDimPlot_brush = renderPlotly({
      if (input$med_subtree2 == "Show") {
        ggplotly(medoid_p,
                 tooltip = c("x", "y", "label")) %>%
          layout(dragmode='pan')
      }
      else {
        if (is.null(shortest_path_brush())) {
          alpha_id = unique(c(rv$g1, rv$g2))
          if (!is.null(alpha_id)) {
            alpha = rep(0.3, nrow(X))
            alpha[alpha_id] = 1
          }
          else {
            alpha = rep(1, nrow(X))
          }

          p_brush = ggplot(plotting_df, aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
            geom_point(size=0.3, alpha=alpha) +
            labs(color="Class")

          ggplotly(p_brush,
                   tooltip = c("x", "y", "label")) %>%
            layout(dragmode='lasso') %>%
            event_register("plotly_selecting")
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
            layout(dragmode='lasso') %>%
            event_register("plotly_selecting")
        }
      }
    })

    output$projPath_brush = renderPlotly({
      if (is.null(projected_pts_brush())) {
        return(plotly_empty(type="scatter", mode="markers"))
      }

      ret = plot_2d_projection_brush(tree, cluster, rv$g1, rv$g2, projected_pts_brush()$projected_pts,
                                     projected_pts_brush()$ids, projected_pts_brush()$path_ids,
                                     projected_pts_brush()$var_explained, input$degree_brush,
                                     input$slider_brush, input$adjust_brush, input$show_all_edges_brush,
                                     input$path_color_brush)

      q = ggplotly(ret$p, tooltip = c("x", "y", "label"))

      # edit legend after conversion to plotly because ggplotly changes legend
      for (i in 1:length(q$x$data)) {
        if (q$x$data[[i]]$mode == "markers") {
          q$x$data[[i]]$name = str_extract(q$x$data[[i]]$name, "(?<=\\().+(?=(,1\\)))")
        }
        else if (q$x$data[[i]]$mode == "lines") {
          q$x$data[[i]]$showlegend = FALSE
        }
      }

      q %>%
        layout(dragmode='pan') %>%
        add_annotations(text=paste(round(ret$var_explained, 2)),
                        xref='paper', yref='paper',
                        x=1, y=1,
                        showarrow = FALSE) %>%
        layout(legend=list(title=list(text="Group"))) %>%
        {if (input$path_color_brush == "Original Coloring") layout(., showlegend = FALSE) else .}
    })

    output$pathWeights_brush = renderPlot({
      if (is.null(shortest_path_brush())) {
        return(plotly_empty(type="bar"))
      }

      plot_path_weights(shortest_path_brush(), input$slider_brush, max_length)
    })
  }

  shinyApp(ui=ui, server=server)
}
