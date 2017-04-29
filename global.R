# metadata, which has already been stored as an R object
metadata<-readRDS(file="data/ebola_metadata.RDS")
tree<-NULL


#setting up the colours for the different countries
colVals<-c("#fdc086","#7fc97f","#beaed4","#386cb0",NA)
countryCol<-data.frame(colVals=colVals,ord=c("?","GIN","LBR","SLE",""))
pal<-colorFactor(colVals[1:4], domain = c("?","GIN","LBR","SLE")) #leaflet
