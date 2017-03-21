
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggtree)
library(leaflet)
library(RColorBrewer)

source("serverUtility.R")
source("colourManager.R")

shinyServer(function(input, output) {

  #phylogenetic tree
  output$treePlot <- renderPlot({
    tree<-readRDS("./data/ebolaTree.RDS")
    
    if(input$treeLayout=="circ"){
      tree<-readRDS("./data/ebolaTree_circular.RDS")
    }
    
    #metadata is available as a global variable, so we don't need to load it
    tree<-colorTreeTip(tree,metadata,input$colorBy)
    
    #works!
    if(!is.null(input$plot_brush) & input$treeLayout == "rec"){
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
  
  #a map on the side, which will change layers according to 
  output$caseMap<-renderLeaflet({
    m<-NULL
    
    if(input$colorBy=="Country" | input$colorBy=="Date"){
      #pal<-colorFactor(brewer.pal(name="Set1",3), domain = c("GIN","LIB","SLE"))
      
      aggDat<-metadata %>%
        filter(Country !="?") %>%
        group_by(Country,country_lon,country_lat) %>%
        count()%>% 
        mutate(popup=sprintf("%s = %d cases",Country,n))
      
      m<-leaflet(aggDat) 
      
      m %>%
        addTiles()%>% 
        addCircleMarkers(
          lng=~country_lon,
          lat= ~country_lat,
          radius=~sqrt(n)*2,
          color = ~pal(Country),
          stroke = FALSE, fillOpacity = 0.7,
          label=~as.character(popup),
          labelOptions = labelOptions(noHide = T)
        )
    }else if(input$colorBy=="Region"){
      #this option here lets me have clusters instead
      # m<-leaflet(metadata) 
      # 
      # m %>%
      #   addTiles()%>%
      #   addCircleMarkers(
      #     lng=~region_lon,
      #     lat= ~region_lat,
      #     stroke = FALSE, fillOpacity = 0.5,
      #     clusterOptions= markerClusterOptions(titile="regional clusters")
      #   )
      
      #pal<-colorFactor(brewer.pal(name="Set1",3), domain = c("GIN", "SLE","LIB"))
      
      aggDat<-metadata %>%
        filter(Country !="?") %>%
        group_by(Country,Region,region_lon,region_lat) %>%
        count()%>% 
        mutate(popup=sprintf("%s (%s) = %d cases",Region,Country,n))
      
      m<-leaflet(aggDat)
      
      m %>%
        addTiles()%>% 
        addCircleMarkers(
          lng=~region_lon,
          lat= ~region_lat,
          radius=~sqrt(n)*2,
          color = ~pal(Country),
          stroke = FALSE, fillOpacity = 0.7,
          label=~as.character(popup),
          labelOptions = labelOptions(noHide = F)
        )
    }
    
  })
  
  output$timeline<-renderDygraph({
    #Ginea
    GIN<-metadata %>%
      filter(Country=="GIN") %>%
      group_by(YearMonth) %>%
      count()
    
    GIN$YearMonth<-ymd(sapply(GIN$YearMonth,function(x){paste(x,"01",sep="-")}))
    
    xtsGIN<-xts(GIN$n, GIN$YearMonth)
    
    #Sierra Leone
    SLE<-metadata %>%
      filter(Country=="SLE") %>%
      group_by(YearMonth) %>%
      count()
    
    SLE$YearMonth<-ymd(sapply(SLE$YearMonth,function(x){paste(x,"01",sep="-")}))
    xtsSLE<-xts(SLE$n, SLE$YearMonth)
    
    xtsObj<-cbind(xtsGIN,xtsSLE)
    
    #Liberia
   LIB<-metadata %>%
      filter(Country=="LBR") %>%
      group_by(YearMonth) %>%
      count()
    
    LIB$YearMonth<-ymd(sapply(LIB$YearMonth,function(x){paste(x,"01",sep="-")}))
    xtsLIB<-xts(LIB$n, LIB$YearMonth)
    
    xtsObj<-cbind(xtsGIN,xtsSLE,xtsLIB)
    colnames(xtsObj)<-c("GIN","SLE","LIB")
    
    dygraph(xtsObj) %>% dyRangeSelector()
  })
})

