library(ggplot2)
library(lattice)
setwd("C:/Users/a-gerbeaux/Documents/0 PROJET")
load("0 Data Cleaning/POI/ExtendPOIDataBase.RData")
load("2 Data Visualization/Carte/MapParis.RData")
load("0 Data Cleaning/Actuaire/Targets.RData")
load("0 Data Cleaning/Actuaire/VarAdd.RData")
load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")

PlotParis <- function(title){
  theme_xkcd <- theme(
    panel.background = element_rect(fill="white"),
    axis.ticks = element_line(colour=NA),
    panel.grid = element_line(colour="white"),
    axis.text.y = element_text(colour="black"),
    axis.text.x = element_text(colour="black"),
    text = element_text(size=16, family="Humor Sans")
  )
  
  p <- ggplot(data=Paris)
  p <- p + geom_line(  aes(x=V8, y=V9, group = paste(Paris[,"V3"],Paris[,"V5"])),colour="black", fill="white" )
  p<-p+ggtitle(paste("Paris et la petite couronne : \n",title))
  p<-p + xlab("longitude")
  p<-p+ ylab("latitude")
  p<-p+theme_xkcd
  p <- p + geom_line(data = Departement,  aes(x=V8, y=V9, group = paste(Departement[,"V3"],Departement[,"V5"])),colour="black", fill="white" )
  return(p)
}



for(i in ExcPOI){
 
  png(paste('2 Data Visualization/Plot POI/Localisation POI/',i,'.png',sep = ''),width =820, height = 661)
  print(PlotParis(i)+geom_point(data =Extend_POI_data[[i]], aes(x=long,y=lat, colour = "black") , alpha = 1)+theme(legend.position="none"))
  dev.off()
}


PlotMap <- function(bool = TRUE,value,title, color){
  load("0 Data Cleaning/Actuaire/Targets.RData")
  load("0 Data Cleaning/Actuaire/VarAdd.RData")
  load("2 Data Visualization/Carte/MapParis.RData")
  
  PlotParis <- function(){
    theme_xkcd <- theme(
      panel.background = element_rect(fill="white"),
      axis.ticks = element_line(colour=NA),
      panel.grid = element_line(colour="white"),
      axis.text.y = element_text(colour="black"),
      axis.text.x = element_text(colour="black"),
      text = element_text(size=16, family="Humor Sans")
    )
    
    p <- ggplot(data=Paris)
    p <- p + geom_line(  aes(x=V8, y=V9, group = paste(Paris[,"V3"],Paris[,"V5"])),colour="black", fill="white" )
    p<-p+ggtitle(paste("Paris et la petite couronne : \n",title))
    p<-p + xlab("longitude")
    p<-p+ ylab("latitude")
    p<-p+theme_xkcd
    p <- p + geom_line(data = Departement,  aes(x=V8, y=V9, group = paste(Departement[,"V3"],Departement[,"V5"])),colour="black", fill="white" )
    return(p)
  }
  
  p <- PlotParis()
  
  
  if(bool){
    data <- data.frame(Coord_WFS84,value = value)
    p<-p+geom_point(data =data, aes(x=long,y=lat, colour = value) , alpha = 0.18)
    p<-p+scale_colour_gradientn(colours=color)
    
  } else if (!bool){
    data <- data.frame(Coord_WFS84[freqVOL>0],value = value)
    p<-p+geom_point(data =data, aes(x=long,y=lat, colour = value) , alpha = 1)
    p<-p+scale_colour_gradientn(colours=color)
  }
  return(p)
}


for(i in ExcPOI){
  
  png(paste('2 Data Visualization/Plot POI/Densité POI/',i,'.png',sep = ''),width =1920, height = 1080)
  print(PlotMap(TRUE,Extend_POI_Predictors[,i],paste(i,"normalisé par la distance au 1000e voisin"),color = rainbow(7) ))
  dev.off()
}
i <- "veterinary"

w <- 400
h <- 374

png(paste('5 Rapport/Presentation Orale/POI',i,'.png',sep = ''),width =w, height = h)
PlotParis(i)+geom_point(data =Extend_POI_data[[i]], aes(x=long,y=lat, colour = "black") , alpha = 1)+theme(legend.position="none")
dev.off()

png(paste('5 Rapport/Presentation Orale/POI_dist',i,'.png',sep = ''),width =w, height = h)
PlotMap(TRUE,Extend_POI_Predictors[,i],i,color = rainbow(7) )
dev.off()

png(paste('5 Rapport/Presentation Orale/POI_dist_norm',i,'.png',sep = ''),width =w, height = h)
PlotMap(TRUE,Extend_POI_Predictors[,i]/Densite_dist_client[,6],paste(i,"normalisé"),color = rainbow(7) )
dev.off()