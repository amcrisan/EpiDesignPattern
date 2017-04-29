colVals<-c("#fdc086","#7fc97f","#beaed4","#386cb0",NA)

countryCol<-data.frame(colVals=colVals,ord=c("?","GIN","LBR","SLE",""))
pal<-colorFactor(colVals[1:4], domain = c("?","GIN","LBR","SLE")) #leaflet

