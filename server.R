
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("serverUtility.R")

shinyServer(function(input, output) {

  #phylogenetic tree
  output$treePlot <- renderPlot({
    tree<-readRDS("./data/ebolaTree.RDS")
    
    if(input$treeLayout=="circ"){
      tree<-readRDS("./data/ebolaTree_circular.RDS")
    }
    
    #metadata is available as a global variable, so we don't need to load it
    tree<<-colorTreeTip(tree,metadata,input$colorBy)
    
    #works, 
    if(!is.null(input$plot_brush)){
      e<-input$plot_brush
      tree<- tree + 
        xlim(e$xmin,e$xmax) +
        ylim(e$ymin,e$ymax)
    }
    #return the tree
    tree
  })
  

  #text out to test out brushing
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
})

