
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(ape)
library(ggtree)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggmap)
library(RColorBrewer)
library(dygraphs)
library(xts)
library(leaflet)

source("serverUtility.R") #custom bit of code I wrote with helper functions

shinyServer(function(input, output) {
  
  ##### REACTIVE VARIABLES
  # metadata variable that changes reactive according to the
  # timeline date range
  metadataReactive <- reactive({
    startDate<-input$timeline_date_window[[1]]
    endDate<-input$timeline_date_window[[2]]
    
    if(is.null(startDate)){
      metadata #when program initializes, start date will be null, so we send up the full dataset
    }else{
      metadata %>% filter(Date>=startDate & Date <= endDate)
    }
  })
    
  
  
  ##### VISUALIZATIONS
  
  # ********
  # PHYLOGENETIC TREEE
  
  output$treePlot <- renderPlot({
    
    # We're going to load trees that have already been stored.
    # The alternative is to re-compute tree each time, which can be slow
    # Since we're keeping the base structure, it's good to 
    tree<-readRDS("./data/ebolaTree.RDS")  # default is rooted tree
    
    if(input$treeLayout=="circ"){
      tree<-readRDS("./data/ebolaTree_circular.RDS") #alternative is 
    }
    
    #metadata is available as a global variable, so we don't need to load it
    #but we've also created this reactive variable, so we're going to 
    #also colour by
    colTreeMeta<-metadata[,c("ID","Country","Region","Date")]
    colTreeMeta$Country<-factor(colTreeMeta$Country,levels=c(levels(colTreeMeta$Country),"")) #this is a hack to avoid a ggtree error
    
    temp<-metadataReactive()
    colTreeMeta<-colTreeMeta %>%
      mutate(Country = replace(Country,!(ID %in% temp$ID),""))
    
    tree<-colorTreeTip(tree,colTreeMeta,input$colorBy)
  
    #Works - but buggy brushing interaction
    if(!is.null(input$plot_brush) & input$treeLayout == "rec"){
      e<-input$plot_brush
      tree<- tree + 
        xlim(e$xmin,e$xmax) +
        ylim(e$ymin,e$ymax)
    }
    
    #return the tree
    tree
  })
  

  # ********
  # Little bit of testing code that shows what is being clicked on in the phylogenetic tree
  #
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
  
  # ********
  # Map that shows case counts
  #
  output$caseMap<-renderLeaflet({
     m<-NULL
    
    if(input$colorBy=="Country" | input$colorBy=="Date"){
      #pal<-colorFactor(brewer.pal(name="Set1",3), domain = c("GIN","LIB","SLE"))
      
      aggDat<-metadataReactive() %>%
        filter(Country !="?") %>%
        group_by(Country,country_lon,country_lat) %>%
        dplyr::count()%>% 
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
      #this option here lets me have clusters instead,
      #I don't use this, but left the code here for others to play around with
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
      
      aggDat<-metadataReactive() %>%
        filter(Country !="?") %>%
        group_by(Country,Region,region_lon,region_lat) %>%
        dplyr::count()%>% 
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
  
  
  # ********
  # Timeline
  #
  
  output$timeline<-renderDygraph({
    ######
    # To create the dygraph, we need to generate and xts series for *each* of the countries.
    # I've done this manually here, but there are ways to do this automatically
    
    #count cases by date, we're also going to aggregatge by *month* so we're going to 
    #create a new time variable
    timeseriesData<-metadata %>%
      mutate(yearMonth=ymd(sapply(YearMonth,function(x){paste(x,"01",sep="-")}))) %>% 
      group_by(yearMonth)%>% 
      dplyr::count(Country) %>%
      complete(yearMonth,Country) %>% #make sure that all dates are represented
      mutate(n=replace(n,is.na(n),0)) #turn NAs from above command in zeros
    
    #create an xts object
    xtsObj<-c()
    for(i in unique(timeseriesData$Country)){
      temp<-timeseriesData %>%
        filter(Country == i)
      
      xtsObj<-cbind(xtsObj,xts(temp$n, temp$yearMonth))
    }
    
    #name out object, so that it plots the time series correctly
    colnames(xtsObj)<-unique(timeseriesData$Country)

    #now make the the dygraph (yay!)
    dygraph(xtsObj,height=200) %>% 
      dyOptions(stackedGraph = TRUE,colors = countryCol$colVals) %>% 
      dyRangeSelector(fillColor="#c97f91",strokeColor="#c97f91")
  })
})

