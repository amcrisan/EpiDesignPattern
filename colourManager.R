library(viridis)

colVals<-c("#fdc086","#7fc97f","#beaed4","#386cb0",NA)

countryCol<-data.frame(colVals=colVals,ord=c("?","GIN","LBR","SLE",""))
pal<-colorFactor(colVals[1:4], domain = c("?","GIN","LBR","SLE")) #leaflet


# dataAgg<-metadata %>%
#   select(Country,Region)%>%
#   unique()%>%
#   group_by(Country) %>%
#   count()
  
#GIN
#ginCol<-magma(n=unlist(dataAgg[2,"n"]))

#LBR
#lbrCol<-inferno(n=unlist(dataAgg[3,"n"]))

#SLE
#sleCol<-plasma(n=unlist(dataAgg[3,"n"]))

# regions + dates

#colour scale of countries

#countryCol<-data.frame(colVals=c(viridis(4),"white"),ord=c("?","GIN","LBR","SLE",""))
#pal<-colorFactor(viridis(4), domain = c("?","GIN","LBR","SLE")) #leaflet

#colour scale of regions
