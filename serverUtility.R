colorTreeTip = function(tree,metadata,var) {
  t<-tree %<+% metadata + geom_tippoint(aes_string(color=var),size=5, alpha=0.35) + theme(legend.position="right")

  if(var %in% c("Country")){
    #be intelligent about the colour scale, based upon what data there actuall is
    t<-t + scale_color_manual(values=as.character(countryCol$colVals),drop=FALSE)
  }
  
  t
}


