library(viridis)

dataAgg<-metadata %>%
  select(Country,Region)%>%
  unique()%>%
  group_by(Country) %>%
  count()
  
#GIN
ginCol<-magma(n=unlist(dataAgg[2,"n"]))

#LBR
lbrCol<-inferno(n=unlist(dataAgg[3,"n"]))

#SLE
sleCol<-plasma(n=unlist(dataAgg[3,"n"]))

# regions + dates

#colour scale of countries

countryCol<-c(viridis(4))
pal<-colorFactor(viridis(4), domain = c("GIN","LIB","SLE","?")) #leaflet

#colour scale of regions
