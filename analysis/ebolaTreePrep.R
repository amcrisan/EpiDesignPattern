#code from the following online tutorials
#https://www.r-phylo.org/wiki/HowTo/InputtingTrees
#https://bioconductor.org/packages/release/bioc/html/ggtree.html

#data from:https://github.com/nickloman/ebov/blob/master/phylo/beast/Makona_728_cds_ig.MCC.tree
#read in the nexus treelibrary

library(ape)
library(ggtree)
library(lubridate)
library(dplyr)

myTree <- read.nexus("./analysis/Makona.tree") #this will read it into a class called phylo


# theses trees can be saved, and don't have to be recomputed from base 
# if the sole purpose of the application is display

saveRDS(file="./data/ebolaTree.RDS",ggtree(myTree))
saveRDS(file="./data/ebolaTree_circular.RDS",ggtree(myTree,layout="circular"))
saveRDS(file="./data/ebolaTree_unrooted.RDS",ggtree(myTree,layout="unrooted"))

#Okay, now here I am just playing around, and figuring things out
pTree<-ggtree(myTree)

# I also want to get out the metadata 

rawMeta<-get.tree(myTree)$tip.label

rawMeta<-sapply(rawMeta,function(x){strsplit(x,"\\|")} %>% unlist()) %>% t()
rawMeta<-cbind(rownames(rawMeta),rawMeta)

colnames(rawMeta)<-c("ID","ORG","LAB","LAB2","LAB3","Country","Region","Region2","Protocol","Date") #now its a data frame

metadata<-data.frame(rawMeta)

#formatting the date a little bit
metadata$Date<-ymd(as.character(metadata$Date))
metadata$YearMonth<-format(metadata$Date,"%Y-%m") #for quick access

saveRDS(file="data/ebola_metadata.RDS",metadata)

#now lets draw some trees!
#very nice, the tree is coloured according to the country
pTree %<+% metadata + geom_tippoint(aes(color=YearMonth),size=5, alpha=0.35) + theme(legend.position="right")

#what about a radial tree? -yes, it works, awesome, ggtree is a great package
pTree<-ggtree(myTree,layout="circular")
pTree %<+% metadata + geom_tippoint(aes(color=var),size=5, alpha=0.35) + theme(legend.position="right")



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

#if I want to highlight a certain range..


