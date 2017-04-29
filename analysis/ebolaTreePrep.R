#code from the following online tutorials
#https://www.r-phylo.org/wiki/HowTo/InputtingTrees
#https://bioconductor.org/packages/release/bioc/html/ggtree.html

#data from:https://github.com/nickloman/ebov/blob/master/phylo/beast/Makona_728_cds_ig.MCC.tree
#paper about data: https://www.nature.com/nature/journal/v530/n7589/full/nature16996.html
library(ape)
library(ggtree)
library(lubridate)
library(dplyr)
library(ggmap)
library(RColorBrewer)
library(dygraphs)
library(xts)


#####################
#
# Loading ebola phylogenetic tree
#
#####################
#read in the nexus treelibrary
myTree <- read.nexus("./analysis/Makona.tree") #this will read it into a class called phylo


# theses trees can be saved, and don't have to be recomputed when the app starts (saves time!)
saveRDS(file="./data/ebolaTree.RDS",ggtree(myTree))
saveRDS(file="./data/ebolaTree_circular.RDS",ggtree(myTree,layout="circular"))
saveRDS(file="./data/ebolaTree_unrooted.RDS",ggtree(myTree,layout="unrooted"))

#####################
#
# NOW, let's set up the metadata file for three
#
#####################

pTree<-ggtree(myTree)

# I also want to get out the metadata 

rawMeta<-get.tree(myTree)$tip.label # for these files all the INFO is the tree node label

rawMeta<-sapply(rawMeta,function(x){strsplit(x,"\\|")} %>% unlist()) %>% t()
rawMeta<-cbind(rownames(rawMeta),rawMeta)

colnames(rawMeta)<-c("ID","ORG","LAB","LAB2","LAB3","Country","Region","Region2","Protocol","Date") #now its a data frame

metadata<-data.frame(rawMeta)

#formatting the date a little bit
metadata$Date<-ymd(as.character(metadata$Date))
metadata$YearMonth<-format(metadata$Date,"%Y-%m") #for quick access

#now lets draw some trees!
#very nice, the tree is coloured according to the country
pTree %<+% metadata + geom_tippoint(aes(color=YearMonth),size=5, alpha=0.35) + theme(legend.position="right")

#what about a radial tree? -yes, it works, awesome, ggtree is a great package
pTree<-ggtree(myTree,layout="circular")
pTree %<+% metadata+ geom_tippoint(aes(color=Country),size=5, alpha=0.35) + theme(legend.position="right")


#so to add geographic data, I need to do a bit more processing here to store co-ordinates
#I know the countries and Guinea, Sierra Leone, and Liberia, so I'll look those up
dfCountry<-data.frame(Country=c("GIN","SLE","LBR"),
                      longName=c("Guinea","Sierra Leone","Liberia"),
                      geocode(c("Guinea","Sierra Leone","Liberia"),output="latlon"))
colnames(dfCountry)<- c("Country","countryLongName","country_lon","country_lat")


#let's joint this data with the rest of the metadata, I'm going to need the long names to get the accurate region
#test2<-base::merge(x = metadata, y = dfCountry, by = "Country",all.x=T), problem is it effects ggtree

#for resons unknown to me, merge is doing something bad to the data frame, so I've have to implement this hack
mergedCountry<-sapply(metadata$Country,function(x){
  test<-dfCountry %>% filter(Country==as.character(x)) %>% select(countryLongName,country_lon,country_lat)
  c(as.character(test[1,1]),test[1,2],test[1,3])
}) %>% t() %>% data.frame

colnames(mergedCountry)<-c("countryLongName","country_lon","country_lat")


metadata<-cbind(metadata,mergedCountry)

regionString<-metadata %>% 
  filter(Region !='?') %>% 
  mutate(searchString = paste(Region,countryLongName,sep=", ")) %>% 
  select(Region,searchString) %>%
  unique()

dfRegion<-data.frame(Region = regionString$Region,
                     geocode(unique(regionString$searchString)))
colnames(dfRegion)<-c("Region","region_lon","region_lat")

#for resons unknown to me, merge is doing something bad to the data frame, so I've have to implement this hack
mergedRegion<-sapply(metadata$Region,function(x){
  test<-dfRegion %>% filter(Region==as.character(x)) %>% select(region_lon,region_lat)
  c(as.character(test[1,1]),test[1,2])
}) %>% t() %>% data.frame()
colnames(mergedRegion)<-c("region_lon","region_lat")

metadata<-cbind(metadata,mergedRegion)

#now I have to fix all of the number issues
metadata$country_lon<-as.numeric(as.character(metadata$country_lon))
metadata$country_lat<-as.numeric(as.character(metadata$country_lat))

metadata$region_lon<-as.numeric(as.character(metadata$region_lon))
metadata$region_lat<-as.numeric(as.character(metadata$region_lat))

saveRDS(file="data/ebola_metadata.RDS",metadata) #it all works now, that was annoying


################################
# TESTING OUT SOME VISUALIZAIONS
#


myplot = function(pTree,metadata,var) {
  pTree %<+% metadata + geom_tippoint(aes_string(color=var),size=5, alpha=0.35) + theme(legend.position="right")
}

p2<-myplot(pTree,metadata,"Country")

p2dat<-ggplot_build(p2)$data[[2]]
nodes<-p2dat %>% 
  filter(x>=1 & x <=1.9)%>%
  select(node)

p2+geom_rect(xmin=1,xmax=1.9,ymin=598.9,ymax=763.3,fill=NA,colour="red") +
  xlim(1,1.9) +
  ylim(598.9,764.3)

myplot(pTree,metadata,"Country")+geom_hilight(identify(p),fill="lightgrey")
myplot(pTree,metadata,"Year")


#testing that the stupid tree plays nice
pTree %<+% metadata+ geom_tippoint(aes(color=Country),size=5, alpha=0.35) + theme(legend.position="right")


#Now let's also do this for the different regions
pal<-colorFactor(brewer.pal(name="Set1",3), domain = c("GIN", "SLE","LIB"))

m<-leaflet(metadata) 

m %>%
  addTiles()%>%
  addCircleMarkers(
    lng=~region_lon,
    lat= ~region_lat,
    stroke = FALSE, fillOpacity = 0.5,
    clusterOptions= markerClusterOptions(titile="regional clusters")
  )

#well, that's fun, but not useful, so lets aggregate some data
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

# now lets try some xts stuff

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
dygraph(xtsObj) %>% dyRangeSelector()
