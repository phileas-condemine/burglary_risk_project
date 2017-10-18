############################################
#########CLEANING DATA######################
############################################
#MRH Project Alexandre Gerbeaux


library(FNN)
library(proj4)
#ptransformation
library(rgdal) 
library(sp)
library(fossil)
setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/0 Data Cleaning")


##Data Cleaning Première Partie (Data Actuariat)
ChargementData <- function () {
  DataBrut<-read.csv("CSV Contrats geocodes 2010.csv", header = TRUE, sep = ",")
  vect_Idf <- (floor(DataBrut[,"codpos"]/1000)) %in% c(75,92,93,94)
  Coord_WFS84<-read.csv("Coord_MRH_WGS84.csv",header = FALSE)
  DataBrut <- DataBrut[vect_Idf,]
  Coord_WFS84<-Coord_WFS84[vect_Idf,]
 
  
  #Cleaning
  DataBrut[is.na(DataBrut)] <- 0
  Coord_WFS84 <- Coord_WFS84[,-1]
  colnames(Coord_WFS84)<-c("long","lat")
  rm(vect_Idf)
  save(DataBrut,file = "Actuaire/DataParis.RData")
  
  
  ###Variables additionnelles
  
  Exposition <- DataBrut$anpol
  filtre_exposition <- Exposition > 0.5
  Exposition <- Exposition[filtre_exposition]
  Coord_WFS84 <- Coord_WFS84[filtre_exposition,]
  id_client <- DataBrut$NUMCNT
  id_client <- id_client[filtre_exposition]
  Coord_LII <- cbind(DataBrut$x,DataBrut$y)
  Coord_LII <- Coord_LII[filtre_exposition,]
  colnames(Coord_LII) <- c("long","lat")
  filtre_75 <- (floor(DataBrut[filtre_exposition,"codpos"]/1000)) %in% c(75) 
  save(Exposition, filtre_exposition,Coord_WFS84,id_client,Coord_LII,filtre_75, file = "Actuaire/VarAdd.RData")  
  rm(DataBrut,Coord_WFS84)
  
}

Predictors <- function(){
    load("Actuaire/DataParis.RData")
  
    ##Predictors
      NGLM <- c("var_habit","otpion_Franchise25","option_BdG","CDREGION","pieceobj_me","captxov_me")
      DGGLM <- DataBrut[(names(DataBrut)) %in%  
                           c("var_habit","otpion_Franchise25","option_BdG",
                             "CDREGION","pieceobj_me","captxov_me")]
      piecesobjcdregion <- paste(DGGLM[,"CDREGION"],DGGLM[,"pieceobj_me"])
      DGGLM <- cbind(piecesobjcdregion, DGGLM)
      rm(NGLM, piecesobjcdregion)
      
      Exposition <- DataBrut[,"anpol"]
      filtre_Exposition <- Exposition > 0.5
      DGGLM <- DGGLM[filtre_Exposition,]
      rm(filtre_Exposition,Exposition)
      save(DGGLM, file = "Actuaire/DGGLM.RData")
}

Targets <- function(){
  load("Actuaire/DataParis.RData")
  load("Actuaire/DGGLM.RData")
  ##Construction des variables Cibles
  Exposition <- DataBrut[,"anpol"]
  nbVOL <- DataBrut[,"nbsinVOL"]
  ChargemVOL <- DataBrut[,"chargemVOL"]
  ChargemVOL[ChargemVOL < 0] <- 0
  
  ##On enlève les nbVOL en trop
  BernouilliVOL <- ChargemVOL
  BernouilliVOL[BernouilliVOL > 0] <- 1
  nbVOL <- BernouilliVOL * nbVOL
  rm(BernouilliVOL)
  
  
  filtre_Exposition <- Exposition > 0.5
  Exposition <- Exposition[filtre_Exposition]
  ChargemVOL <- ChargemVOL[filtre_Exposition]
  nbVOL <- nbVOL[filtre_Exposition]
  SinistreVOL <- ChargemVOL / Exposition #Les Sinistres sont rescale avec l'exposition
  freqVOL <- nbVOL / Exposition
  CoutMoyenVOL <- ChargemVOL[ChargemVOL > 0]/nbVOL[ChargemVOL >0]
  #Remarque : les nbVOL sont mal référencés (> 0 avec Chargem VOL = 0, mais les Chargem VOL sont bien
  #référencés : si il y a ChargemVOL >0 alor nbVOL >0)
  rm(filtre_Exposition)
  
  save(nbVOL,SinistreVOL,freqVOL,CoutMoyenVOL, ChargemVOL, file = "Actuaire/Targets.RData")
  
  }

ConstructionData <- function(){  
  load("Actuaire/DGGLM.RData")
  load("Actuaire/Targets.RData")
  
  applyfunction <- function(x)##fonction qui renvoit explicitement la matrice binaire pour les factors
  {
    sortie <- matrix(1,nrow(x),1)
    for(i in 1:ncol(x))
    {
      t<-model.matrix(~factor(x[,i]))
      t<-t[,2:ncol(t)]
      sortie <- cbind(sortie, t)
      
    }
    return(sortie)
  }
  
  DGGLMVOLSinistre <- cbind(DGGLM,SinistreVOL)
  DGGLMnbVOL <- cbind(DGGLM,nbVOL)
  DGGLMFrequence <- cbind(DGGLM, freqVOL)
  DGGLMVOLCoutMoyen <- cbind(DGGLM[ChargemVOL >0,], CoutMoyenVOL)
  
  ##Variable factorielle
  X <- applyfunction(DGGLM)
  
  save(DGGLMVOLSinistre,DGGLMnbVOL,DGGLMFrequence,DGGLMVOLCoutMoyen,X, file = "Actuaire/DataModeles.RData")
}


  

##Data Cleaning Deuxième Partie (Open Data)

##Batiment
BatimentDataLoad <- function(){
    Base_IMM <- read.csv("Batiment/Base_IMM_STD_2010.csv", header = TRUE)
    IMM_1 <- read.csv("Batiment/IMM_ADOK1.csv")
    IMM_2 <- read.csv("Batiment/IMM_ADOK2.csv")
    IMM_00 <- read.csv("Batiment/immnat_1ok.csv")
    IM_01 <- read.csv("Batiment/suite_imm_1_gcdok.csv")
    IMM_1 <- as.matrix(IMM_1)
    IMM_2 <- as.matrix(IMM_2)
    bat_geo <- rbind(IMM_1,IMM_2)
    id_IMM <- bat_geo[,1]
    coord_LII <- bat_geo[,c("x", "y")]
    Var_Batiment <- Base_IMM[,c("NUMCNT","nbniveau","DTconst","surftot","surfco")]
    id_match <- match(id_IMM,Var_Batiment[,1])
    Var_Batiment_filtre <- Var_Batiment[id_match,]
    outputbat <- cbind(Var_Batiment_filtre,coord_LII)
    BatimentDataBase <- outputbat[complete.cases(outputbat[,c("x","y")]),]
    rm(Base_IMM,IMM_1,IMM_2,IMM_00,IM_01,bat_geo,coord_LII,
    Var_Batiment,id_match,Var_Batiment_filtre, id_IMM,outputbat)
    save(BatimentDataBase,file = "Batiment/BatimentDataBase.RData")
}

BatimentPredictors <- function(){
  load("Actuaire/VarAdd.RData") 
  #On récupère la variable Coord_LII, c'est celle ci qui nous intéresse
  rm(Coord_WFS84,Exposition, filtre_exposition,id_client)
  load("Batiment/BatimentDataBase.RData")
  
  DateDeConstruction <- function(){
    #Date de Construction
    BatimentDataBasecleanDT <- BatimentDataBase[complete.cases(BatimentDataBase[,"DTconst"]),]
    BatimentDataBase1500 <- BatimentDataBasecleanDT[BatimentDataBasecleanDT$DTconst > 1500, ]
    Coord_Batiment <- as.matrix(BatimentDataBase1500[, c("x","y")])
    #on filtre à > 1500s
    #/!\ Les coordonnées des batiments ont besoin d'être numérisée
    Coord_Batiment <- cbind(as.numeric(as.character(Coord_Batiment[,1])),as.numeric(as.character(Coord_Batiment[,2])))
    N<-10
    #Recherche des N plus proches voisin dans l'algorithme
    neigh_bat <- get.knnx(Coord_Batiment,Coord_LII, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
    
    neigh_DT <- matrix(1,nrow(Coord_LII),1)
    for(i in c(1:N)){
      neigh_DT <-cbind(neigh_DT,BatimentDataBase1500[neigh_bat$nn.index[,i],3])
    }
    neigh_DT <- neigh_DT[,2:ncol(neigh_DT)]
    DTConst <-neigh_DT[,1]
    for(i in c(3,5,10)){
      DTConst <- cbind(DTConst,apply(neigh_DT[,c(1:i)],1,mean))
    }
    colnames(DTConst) <- c("DTConst1","DTConst3","DTConst5","DTConst10")
  
    return(DTConst)
  }
  
DTConst <- DateDeConstruction()
  
  NombredeNiveau <- function(){
  #Nombre de Niveau
  BatimentDataBasecleanNB <- BatimentDataBase[complete.cases(BatimentDataBase[,"nbniveau"]),]
  BatimentDataBase0 <- BatimentDataBasecleanNB[BatimentDataBasecleanNB$nbniveau > 0, ]
  Coord_Batiment <- as.matrix(BatimentDataBase0[, c("x","y")])
  #on filtre à > 1500s
  #/!\ Les coordonnées des batiments ont besoin d'être numérisée
  Coord_Batiment <- cbind(as.numeric(as.character(Coord_Batiment[,1])),as.numeric(as.character(Coord_Batiment[,2])))
  
  N<-10
  #Recherche des N plus proches voisin dans l'algorithme
  neigh_bat <- get.knnx(Coord_Batiment,Coord_LII, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  neigh_NB <- matrix(1,nrow(Coord_LII),1)
  for(i in c(1:N)){
    neigh_NB <-cbind(neigh_NB,BatimentDataBase0[neigh_bat$nn.index[,i],2])
  }
  neigh_NB <- neigh_NB[,2:ncol(neigh_NB)]
  
  NB <-neigh_NB[,1]
  for(i in c(3,5,10)){
    NB<- cbind(NB,apply(neigh_NB[,c(1:i)],1,mean))
  }
  colnames(NB) <- c("NB1","NB3","NB5","NB10")
  
  return(NB)
  
  }
  
NB <- NombredeNiveau()

TauxCommerce <- function(){
  #Nombre de Niveau
  BatimentDataBasecleanCO <- BatimentDataBase[complete.cases(BatimentDataBase[,c("surftot","surfco")]),]
  BatimentDataBaseCO <- BatimentDataBasecleanCO[BatimentDataBasecleanCO$surftot > 0, ]
  Coord_Batiment <- as.matrix(BatimentDataBaseCO[, c("x","y")])
  BatimentDataBaseCO <- BatimentDataBaseCO$surfco/BatimentDataBaseCO$surftot
  #on filtre à > 1500s
  #/!\ Les coordonnées des batiments ont besoin d'être numérisée
  Coord_Batiment <- cbind(as.numeric(as.character(Coord_Batiment[,1])),as.numeric(as.character(Coord_Batiment[,2])))
  
  N<-100
  #Recherche des N plus proches voisin dans l'algorithme
  neigh_bat <- get.knnx(Coord_Batiment,Coord_LII, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  neigh_CO <- matrix(1,nrow(Coord_LII),1)
  for(i in c(1:N)){
    neigh_CO <-cbind(neigh_CO,BatimentDataBaseCO[neigh_bat$nn.index[,i]])
  }
  neigh_CO <- neigh_CO[,2:ncol(neigh_CO)]
  
  CO <-neigh_CO[,1]
  for(i in c(3,5,10,20,30,50,70,100)){
    CO<- cbind(CO,apply(neigh_CO[,c(1:i)],1,mean))
  }
  colnames(CO) <- c("CO1","CO3","CO5","CO10","CO20","CO30","CO50","CO70","CO100")
  
  return(CO)
  
}

CO <- TauxCommerce()



save(DTConst,NB,CO,file ="Batiment/BatimentPredictors.RData" )

}


##POI

POIDataLoad <- function() {
  load("Actuaire/VarAdd.Rdata")
  POI <- read.csv("POI/nodes_with_amenity.csv", header = FALSE)
  POI<- POI[,-5]
  quadrillage_paris <- function(long,lat){
    return((long > 0.9*min(Coord_WFS84$long))&(long < 1.1*max(Coord_WFS84$long)) &(lat > 0.9*min(Coord_WFS84$lat))&(lat < 1.1*max(Coord_WFS84$lat)))
  }
  
  colnames(POI) <- c("key","type","long","lat")
  POI <- POI[quadrillage_paris(POI$long,POI$lat),]
  
  
  cPOI <- c("place_of_worship","school","restaurant","police","taxi","nightclub","prison",
    "embassy","social_centre","university","theatre","social_facility", "bar","pub","bank",
    "doctors","art_centre", "cinema")
 
  POI_data <- list()
  for(i in cPOI){
    POI_data[[i]] <- POI[POI$type == i,c("key","long","lat")]
  }
  
  save(cPOI,POI_data, file = "POI/POIDataBase.RData")
  
  }

ExtendPOIDataLoad <- function() {
  load("Actuaire/VarAdd.Rdata")
  POI <- read.csv("POI/nodes_with_amenity.csv", header = FALSE)
  POI<- POI[,-5]
  quadrillage_paris <- function(long,lat){
    #nul <- #return((long > 0.9*min(Coord_WFS84$long))&(long < 1.1*max(Coord_WFS84$long)) &(lat > 0.9*min(Coord_WFS84$lat))&(lat < 1.1*max(Coord_WFS84$lat)))
      return((long > min(Coord_WFS84$long))&(long < max(Coord_WFS84$long)) &(lat > min(Coord_WFS84$lat))&(lat < max(Coord_WFS84$lat)))
  }
  
  colnames(POI) <- c("key","type","long","lat")
  POI <- POI[quadrillage_paris(POI$long,POI$lat),]#101POI
  
  freq_POI <- table(POI[,"type"])
  ExcPOI <- row.names(as.matrix(data.frame(freq_POI[freq_POI>49])))
  
  Extend_POI_data <- list()
  for(i in ExcPOI){
    Extend_POI_data[[i]] <- POI[POI$type == i,c("key","long","lat")]
  }
  
  save(ExcPOI,Extend_POI_data, file = "POI/ExtendPOIDataBase.RData")
  
}



POIPredictors <- function(){
  load("Actuaire/VarAdd.Rdata")
  load("POI/POIDataBase.RData")
 
  
  geodetic.distance <- function(point1, point2) #Première Distance
  {
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
    d <- acos(d)
    R*d
  }
  
  distance.chord <- function(point1,point2) #Deuxième Distance
  {
    R <- 6371
    
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    
    lat <- p1rad[2]
    lon <- p1rad[1]
    
    u1 <- c(cos(lat)*cos(lon), cos(lat)*sin(lon), sin(lat))
    
    lat <- p2rad[2]
    lon <- p2rad[1]
    
    u2 <- c(cos(lat)*cos(lon),cos(lat)*sin(lon),sin(lat))
    
    R*sqrt(sum((u1-u2)^2))
    
  }
  
  
POIplusProche <- function(POI){
  POI <- as.data.frame(POI)
  local_name <- c("key","long","lat")
  colnames(POI) <- local_name
  #latlong -> WSF83
  ##################
  #Nécessite la charge de Localisatio_Contrat_LII et localisatio_Contrat_latlong
  dfr <- data.frame(x = POI$long, y = POI$lat) 
  dfr 
  spdfr <- dfr 
  ## transformation en SpatialPointDataFrame 
  coordinates(spdfr) <- ~ x + y 
  ## on déclare le système de coordonnées de référence: dégrés décimaux WGS84 
  proj4string(spdfr) <- CRS("+proj=longlat +ellps=WGS84") 
  spdfr 
  ## conversion en Lamber II étendu 
  spdfrLambert <- spTransform(spdfr, CRS("+init=epsg:27572")) 
  spdfrLambert
  quiery_matrix <- data.frame(spdfrLambert@coords)
  
  Near <- get.knnx(quiery_matrix,Coord_LII, 1, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  Index_Near <- Near$nn.index
  
  
  #return(mapply(distVilleStation,Coord_LII[,1],Coord_LII[,2],POI[Index_Near,"long"],POI[Index_Near,"lat"]))
  point1 <- cbind(Coord_WFS84[,1],Coord_WFS84[,2])
  point2 <- cbind(POI[Index_Near,"long"],POI[Index_Near,"lat"])
  output <- matrix(NA,nrow(point1),1)
  for(i in c(1:nrow(point1))){
    output[i] <- geodetic.distance(point1[i,],point2[i,])
  }
  return(output)
}



POI_Predictors <- matrix(NA,nrow(Coord_LII),1)
for(i in cPOI){
  POI_Predictors <- cbind(POI_Predictors,POIplusProche(POI_data[i]))
  print(i)
}
POI_Predictors <- POI_Predictors[,2:ncol(POI_Predictors)]

colnames(POI_Predictors) <- cPOI

save(POI_Predictors, file = "POI/POI_Predictors.RData")
}

ExtendPOIPredictors <- function(){
  load("Actuaire/VarAdd.Rdata")
  load("POI/ExtendPOIDataBase.RData")
  
  
  geodetic.distance <- function(point1, point2) #Première Distance
  {
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
    d <- acos(d)
    R*d
  }
  
  distance.chord <- function(point1,point2) #Deuxième Distance
  {
    R <- 6371
    
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    
    lat <- p1rad[2]
    lon <- p1rad[1]
    
    u1 <- c(cos(lat)*cos(lon), cos(lat)*sin(lon), sin(lat))
    
    lat <- p2rad[2]
    lon <- p2rad[1]
    
    u2 <- c(cos(lat)*cos(lon),cos(lat)*sin(lon),sin(lat))
    
    R*sqrt(sum((u1-u2)^2))
    
  }
  
  
  POIplusProche <- function(POI){
    POI <- as.data.frame(POI)
    local_name <- c("key","long","lat")
    colnames(POI) <- local_name
    #latlong -> WSF83
    ##################
    #Nécessite la charge de Localisatio_Contrat_LII et localisatio_Contrat_latlong
    dfr <- data.frame(x = POI$long, y = POI$lat) 
    dfr 
    spdfr <- dfr 
    ## transformation en SpatialPointDataFrame 
    coordinates(spdfr) <- ~ x + y 
    ## on déclare le système de coordonnées de référence: dégrés décimaux WGS84 
    proj4string(spdfr) <- CRS("+proj=longlat +ellps=WGS84") 
    spdfr 
    ## conversion en Lamber II étendu 
    spdfrLambert <- spTransform(spdfr, CRS("+init=epsg:27572")) 
    spdfrLambert
    quiery_matrix <- data.frame(spdfrLambert@coords)
    
    Near <- get.knnx(quiery_matrix,Coord_LII, 1, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
    Index_Near <- Near$nn.index
    
    
    #return(mapply(distVilleStation,Coord_LII[,1],Coord_LII[,2],POI[Index_Near,"long"],POI[Index_Near,"lat"]))
    point1 <- cbind(Coord_WFS84[,1],Coord_WFS84[,2])
    point2 <- cbind(POI[Index_Near,"long"],POI[Index_Near,"lat"])
    output <- matrix(NA,nrow(point1),1)
    for(i in c(1:nrow(point1))){
      output[i] <- geodetic.distance(point1[i,],point2[i,])
    }
    return(output)
  }
  
  
  
  Extend_POI_Predictors <- matrix(NA,nrow(Coord_LII),1)
  for(i in ExcPOI){
    Extend_POI_Predictors <- cbind(Extend_POI_Predictors,POIplusProche(Extend_POI_data[i]))
    print(i)
  }
  Extend_POI_Predictors <- Extend_POI_Predictors[,2:ncol(Extend_POI_Predictors)]
  
  colnames(Extend_POI_Predictors) <- ExcPOI
  
  save(Extend_POI_Predictors, file = "POI/Extend_POI_Predictors.RData")
}


##Densité
ClusterDensiteData <- function(){
load("Actuaire/VarAdd.Rdata")

geodetic.distance <- function(point1, point2) #Première Distance
{
  R <- 6371
  p1rad <- point1 * pi/180
  p2rad <- point2 * pi/180
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d)
  R*d
}

N <- 100
#Recherche des N voisins d'un client
neigh_client <- get.knn(Coord_LII, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))




neigh_dens <- matrix(NA, nrow(Coord_LII),N)
for(i in c(1:N)){
  point1 <- cbind(Coord_WFS84[,1],Coord_WFS84[,2]) 
  point2 <- cbind(Coord_WFS84[neigh_client$nn.index[,i],"long"],
                  Coord_WFS84[neigh_client$nn.index[,i],"lat"])
  print(i)
  for(j in c(1:nrow(Coord_LII))){
    neigh_dens[j,i] <- geodetic.distance(point1[j,],point2[j,])
  }
  
}
  
#   neigh_dens_1000 <- matrix(NA, nrow(Coord_LII),N)
#   for(i in c(1:N)){
#     point1 <- cbind(Coord_WFS84[,1],Coord_WFS84[,2]) 
#     point2 <- cbind(Coord_WFS84[index_1000[,i],"long"],
#                     Coord_WFS84[index_1000[,i],"lat"])
#     print(i)
#     for(j in c(1:nrow(Coord_LII))){
#       neigh_dens_1000[j,i] <- geodetic.distance(point1[j,],point2[j,])
#     }
#   
# }

densite_data100 <- neigh_dens
index_VOL_data100 <- neigh_client$nn.index
save(densite_data100, index_VOL_data100, file = "Densite/densite_data100.RData")


###########Cluster
load("VarAdd.RData")
N <- 10000
neigh_client10000 <- get.knn(Coord_LII, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
index_VOL_data10000 <- neigh_client10000$nn.index
save(neigh_client10000, index_VOL_data10000, file = "densite_data10000.RData")



}#Calcul la distance entre le client et son neme voisin, la fonction a été sur le cluster pour N = 1000

ClusterDensitePredictors <- function(){
  load("Densite Client/neigh_dens_1000.RData")
  X_km <- c(0.05,0.1,0.2,0.5,1,3,5,7.5)
  X_dist <- c(1,10,50,100,500,1000)
  Dens_km <- function(V){
  DensiteVect <- function(x){
    table(V < x)[2]
  }
  
  return(mapply(DensiteVect, X_km))
}
  Dens_dist <- function(V){
    
  
mapply(function(x)(V[x]),X_dist)
  }


Densite_km_client <- t(apply(neigh_dens_1000,1,Dens_km))
Densite_dist_client <- t(apply(neigh_dens_1000,1,Dens_dist))


colnames(Densite_km_client) <- c("50m","100m","200m","500m","1km","3km","5km","7km500")
colnames(Densite_dist_client) <- c("1e Client","10e Client","50e Client","100e Client","500e Client","1000e Client")

save(Densite_km_client, Densite_dist_client, file = "Pedictors_densite_client.RData")

}#Extraction de prédicteurs, cette fonction sera lancé sur le cluster
  

DensitePredictorsVOL  <- function(){
  load("Actuaire/VarAdd.Rdata")
  load("Actuaire/Targets.Rdata")
  
  POI <- Coord_WFS84
  POI <- as.data.frame(POI)
  local_name <- c("key","long","lat")
  colnames(POI) <- local_name
  #latlong -> WSF83
  ##################
  #Nécessite la charge de Localisatio_Contrat_LII et localisatio_Contrat_latlong
  dfr <- data.frame(x = POI$long, y = POI$lat) 
  dfr 
  spdfr <- dfr 
  ## transformation en SpatialPointDataFrame 
  coordinates(spdfr) <- ~ x + y 
  ## on déclare le système de coordonnées de référence: dégrés décimaux WGS84 
  proj4string(spdfr) <- CRS("+proj=longlat +ellps=GRS80") 
  spdfr 
  ## conversion en Lamber II étendu 
  spdfrLambert <- spTransform(spdfr, CRS("+init=epsg:27572")) 
  Coord_LII_bis <- as.matrix(data.frame(spdfrLambert$coords))
  
  geodetic.distance <- function(point1, point2) #Première Distance
  {
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
    d <- acos(d)
    R*d
  }
  
  Coord_LII_VOL <- Coord_LII_bis[ChargemVOL > 0,]
  Coord_WFS84_VOL <- Coord_WFS84[ChargemVOL > 0,]
  
  N <- 10
  #Recherche des N voisins d'un client
  neigh_client_VOL <- get.knnx(Coord_WFS84_VOL,Coord_WFS84, k=N, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  
  neigh_dens <- matrix(NA, nrow(Coord_LII),N)
  for(i in c(1:N)){
    point1 <- cbind(Coord_WFS84[,1],Coord_WFS84[,2]) 
    point2 <- cbind(Coord_WFS84[neigh_client_VOL$nn.index[,i],"long"],
                    Coord_WFS84[neigh_client_VOL$nn.index[,i],"lat"])
    print(i)
    for(j in c(1:nrow(Coord_LII))){
      neigh_dens[j,i] <- geodetic.distance(point1[j,],point2[j,])
    }
    
  }
  
  densite_VOL_data10 <- neigh_dens
  index_VOL_data10 <- neigh_client_VOL$nn.index
  save(densite_VOL_data10,index_VOL_data10, file = "Densite/densite_VOL_data10.RData")
  
}##Les résultats de cette fonction sont mauvais

DensitePredictorsVOL2  <- function(){
  #load("Actuaire/Targets.RData")
  load("Targets.RData")
  freqVOL <- freqVOL[freqVOL > 0] ##ici on considère que le subset de VOL 
  SinistreVOL <- SinistreVOL[SinistreVOL >0] ##ici on considère que le subset de VOL 
  
  N  <- 2797
  X_km <- c(0.5,1,3,5,7.5)
  X_perc <- floor(c(0.05,0.1,0.3,0.5)*N)
  X_dist <- c(1,10,50,100)
  X_percEtdist <- c(X_perc,X_dist)
  
  Vol_km <- function(V){
    
    DensiteVect <- function(x){
      sum(freqVOL[V < x])
      #table(V < x)[2]
    }
    
    return(mapply(DensiteVect, X_km))
    
  }#calcul la fréquence de VOL autour d'un cercle de x km autour du client
  Vol_CM <- function(V){
    
    DensiteVect_CM <- function(x){
      (sum(SinistreVOL[V < x]))/(sum(freqVOL[V < x]))
      #table(V < x)[2]
    }
    
    return(mapply(DensiteVect_CM, X_km))
    
  }#Calcul le sinistre Moyen autour d'un cercle de x km autour du client
  Vol_percETdist <- function(V){
    
    #V_order <- order(V)
    
    mapply(function(x)(V[order(V)[x]]),X_percEtdist)
    
  }#perc : Calcul le rayon d'un cercle autour du client qui contient x % de Vol total
                                    #dist : La distance du x em Vol autour du client (distance à vol d'oiseau)
  
  
  
  
  #/!\calcul des distances sorti du cluster, il a été coupé en 6 environ 50000 ligne par vect


##Ici on travaille en "Bernouilli" par soucis de simplicité (pour les perc et dist)

###1
  #load("Densite/Cluster/vect1.RData")
  load("vect1.RData")
  vect_local <- t(vect1)
  rm(vect1)
  Densite_km <- t(apply(vect_local,1,Vol_km))
  Densite_CM <- t(apply(vect_local,1,Vol_CM))
  Densite_perc <- t(apply(vect_local,1,Vol_percETdist))
###2
  #load("Densite/Cluster/vect2.RData")
  load("vect2.RData")
  vect_local <- t(vect2)
  rm(vect2)
  Densite_km <- rbind(Densite_km,t(apply(vect_local,1,Vol_km)))
  Densite_CM <- rbind(Densite_CM,t(apply(vect_local,1,Vol_CM)))
  Densite_perc <- rbind(Densite_perc,t(apply(vect_local,1,Vol_percETdist)))
###3
  #load("Densite/Cluster/vect3.RData")
  load("vect3.RData")
  vect_local <- t(vect3)
  rm(vect3)
  Densite_km <- rbind(Densite_km,t(apply(vect_local,1,Vol_km)))
  Densite_CM <- rbind(Densite_CM,t(apply(vect_local,1,Vol_CM)))
  Densite_perc <- rbind(Densite_perc,t(apply(vect_local,1,Vol_percETdist)))
###4
  #load("Densite/Cluster/vect4.RData")
  load("vect4.RData")
  vect_local <- t(vect4)
  rm(vect4)
  Densite_km <- rbind(Densite_km,t(apply(vect_local,1,Vol_km)))
  Densite_CM <- rbind(Densite_CM,t(apply(vect_local,1,Vol_CM)))
  Densite_perc <- rbind(Densite_perc,t(apply(vect_local,1,Vol_percETdist)))
###5
  #load("Densite/Cluster/vect5.RData")
  load("vect5.RData")
  vect_local <- t(vect5)
  rm(vect5)
  Densite_km <- rbind(Densite_km,t(apply(vect_local,1,Vol_km)))
  Densite_CM <- rbind(Densite_CM,t(apply(vect_local,1,Vol_CM)))
  Densite_perc <- rbind(Densite_perc,t(apply(vect_local,1,Vol_percETdist)))
###6
  #load("Densite/Cluster/vect6.RData")
  load("vect6.RData")
  vect_local <- t(vect6)
  rm(vect6)
  Densite_km <- rbind(Densite_km,t(apply(vect_local,1,Vol_km)))
  Densite_CM <- rbind(Densite_CM,t(apply(vect_local,1,Vol_CM)))
  Densite_perc <- rbind(Densite_perc,t(apply(vect_local,1,Vol_percETdist)))



colnames(Densite_km) <- X_km
colnames(Densite_CM) <-  c("0.5 CM","1 CM","3 CM","5 CM","7.5 CM")
colnames(Densite_perc) <- c("5% Vol","10% Vol","30% Vol","50% Vol",
                            "1e Vol","10e Vol","50e Vol","100e Vol")

save(Densite_km,Densite_CM,Densite_perc,"DensiteVOL_Predictors.RData")

# a voir après Densite_CM[is.nan(Densite_CM)]<-0



# 
# test <- vect2[15000,]
# X <- c(1:(floor(max(test))*100))/100
# 
# dens <- as.matrix(mapply(DensiteVol, X))
# dens[is.na(dens)]<-0
# rownames(dens) <- X
# 
# plot(X,dens, type = "l", col = "red", main = "Evolution du nombre de VOLs en fonction du nombre de kilomètres", xlab  = "nombre de kilomètres",
#      ylab = "nombre de Vols")


}

ClusterDensiteVOL <- function(){
  #load("Actuaire/Targets.RData")
  load("Targets.RData")
  load("distance_client_vol.RData")
  filtreVOL <- freqVOL > 0  
  fV <- freqVOL
  SV <- SinistreVOL
  freqVOL <- freqVOL[freqVOL > 0] ##ici on considère que le subset de VOL 
  
  SinistreVOL <- SinistreVOL[SinistreVOL >0] ##ici on considère que le subset de VOL 
  
  distance_client_vol[is.na(distance_client_vol)]<-0
  
  N  <- 2797
  X_km <- c(0.5,1,3,5,7.5)
  X_perc <- floor(c(0.05,0.1,0.3,0.5)*N)
  X_dist <- c(1,10,50,100)
  X_percEtdist <- c(X_perc,X_dist)
  
  Vol_km <- function(V){
    
    DensiteVect <- function(x){
      sum(freqVOL[V < x])
      #table(V < x)[2]
    }
    
    return(mapply(DensiteVect, X_km))
    
  }#calcul la fréquence de VOL autour d'un cercle de x km autour du client
  Vol_CM <- function(V){
    
    DensiteVect_CM <- function(x){
      (sum(SinistreVOL[V < x]))
      #table(V < x)[2]
    }
    
    return(mapply(DensiteVect_CM, X_km))
    
  }#Calcul le sinistre Moyen autour d'un cercle de x km autour du client
  Vol_percETdist <- function(V){
    
    #V_order <- order(V)
    
    mapply(function(x)(V[order(V)[x]]),X_percEtdist)
    
  }#perc : Calcul le rayon d'un cercle autour du client qui contient x % de Vol total
  #dist : La distance du x em Vol autour du client (distance à vol d'oiseau)
  
  #load("Densite/Cluster/vect1.RData")
  #load("distance_client_vol.RData")
  Densite_km <- t(apply(distance_client_vol,1,Vol_km))- fV #On retire le client (lui même) que l'on a compté en trop
  Densite_CM <- t(apply(distance_client_vol,1,Vol_CM))- SV #On retire le client(lui même) que l'on a compté en trop
  Densite_CM <- (Densite_CM/Densite_km)
  Densite_perc <- t(apply(distance_client_vol,1,Vol_percETdist))
  
  ##AJUSTEMENT PERCEDDIST
  N_adj <- 2796
  X_perc_adj <- floor(c(0.05,0.1,0.3,0.5)*N_adj)
  X_dist_ajd <- c(1,10,50,100) + 1
  X_percEtdist_adj <- c(X_perc_adj,X_dist_ajd)
  Vol_percETdist_adj <- function(V){
    
    #V_order <- order(V)
    
    mapply(function(x)(V[order(V)[x]]),X_percEtdist_adj)
    
  }
  
  Densite_perc_adj <- t(apply(distance_client_vol[filtreVOL,],1,Vol_percETdist_adj))
  Densite_perc[freqVOL,] <- Densite_perc_adj
  
  
  ################################################Cluster
  
  load("Targets.RData")
  load("distance_client_vol.RData")
  filtreVOL <- freqVOL > 0  
  fV <- freqVOL
  SV <- SinistreVOL
  freqVOL <- freqVOL[freqVOL > 0] ##ici on considère que le subset de VOL 
  
  SinistreVOL <- SinistreVOL[SinistreVOL >0] ##ici on considère que le subset de VOL 
  
  distance_client_vol[is.na(distance_client_vol)]<-0
  
  N  <- 2797
  X_km <- c(0.5,1,3,5,7.5)
  X_perc <- floor(c(0.05,0.1,0.3,0.5)*N)
  X_dist <- c(1,10,50,100)
  X_percEtdist <- c(X_perc,X_dist)
  
  Vol_percETdist <- function(V){
    
    #V_order <- order(V)
    
    mapply(function(x)(V[order(V)[x]]),X_percEtdist)
    
  }
  
  Densite_perc <- t(apply(distance_client_vol,1,Vol_percETdist))
  
  N_adj <- 2796
  X_perc_adj <- floor(c(0.05,0.1,0.3,0.5)*N_adj)
  X_dist_ajd <- c(1,10,50,100) + 1
  X_percEtdist_adj <- c(X_perc_adj,X_dist_ajd)
  Vol_percETdist_adj <- function(V){
    
    #V_order <- order(V)
    
    mapply(function(x)(V[order(V)[x]]),X_percEtdist_adj)
    
  }
  
  Densite_perc_adj <- t(apply(distance_client_vol[filtreVOL,],1,Vol_percETdist_adj))
  Densite_perc[filtreVOL,] <- Densite_perc_adj
  
  save(Densite_perc,file = "Densite_perc_adj.RData")
  
  ##################################Fin du Cluster
  
  
  #Correction pour les Volés (ils ne doivent pas être pris en compte dans le calcul des features !)
          #   subset_distance_client_vol <- distance_client_vol[filtreVOL,]
          #   rownames(subset_distance_client_vol) <- c(1:2797)
          #   subset_distance_client_vol <- as.matrix(subset_distance_client_vol)
  #load("Densite Vol/subset_distance_client_vol.RData")
  
#   Vol_km_corr <- function(V){
#     T <- length(V)-1
#     
#     DensiteVect <- function(x){
#       sum(freqVOL[V[1:T] < x]) - freqVOL[V[T+1]]
#       #table(V < x)[2]
#     }
#     
#     return(mapply(DensiteVect, X_km))
#     
#   }#calcul la fréquence de VOL autour d'un cercle de x km autour du client
#   Vol_CM_corr <- function(V){
#     
#     DensiteVect_CM <- function(x){
#       (sum(SinistreVOL[V < x])- SinistreVOL[as.numeric(row.names(V))])/(sum(freqVOL[V < x]) - freqVOL[as.numeric(row.names(V))])
#       #table(V < x)[2]
#     }
#     
#     return(mapply(DensiteVect_CM, X_km))
#     
#   }#Calcul le sinistre Moyen autour d'un cercle de x km autour du client
#   
#   Densite_km_corr <- t(apply(cbind(subset_distance_client_vol,c(1:N)),1,Vol_km_corr))
#   Densite_CM_corr <-  t(apply(subset_distance_client_vol,1,Vol_CM_corr))
  
  colnames(Densite_km) <- X_km
  colnames(Densite_CM) <-  c("0.5 CM","1 CM","3 CM","5 CM","7.5 CM")
  colnames(Densite_perc) <- c("5% Vol","10% Vol","30% Vol","50% Vol",
                              "1e Vol","10e Vol","50e Vol","100e Vol")
  
  

  
  save(Densite_km,Densite_CM,Densite_perc,file = "DensiteVOL_Predictors2.RData")
  
  
}

ClusterDensiteParal <- function (N) {
  
  load("VarAdd.RData")
  load("Targets.RData")
  Coord_WFS84 <- as.matrix(Coord_WFS84)
  Coord_WFS84_VOL <- Coord_WFS84[ChargemVOL > 0,]
  
  
  
  #On divise le calcul par 10
  n<-nrow(Coord_WFS84) %/% 10
  X <- list()
  for(i in c(0:8)){
  X[[i+1]] <- c((i*n+1):((i+1)*n))
  }
  X[[10]] <- c((9*n):nrow(Coord_WFS84))
  geodetic.distance <- function(point1, point2) #Première Distance
  {
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
    d <- acos(d)
    R*d
  }
  
  
  
  calc.distance <- function(point1){
    
    return(apply(Coord_WFS84_VOL,1,(function(x)(geodetic.distance(x,point1)))))
    
  }
  
  
  vect1 <- apply(Coord_WFS84[X[[N]],],1,calc.distance)

  
  save(vect1, file = "vect1.RData")
  # save(vect2, file = "vect2.RData")
  # save(vect3, file = "vect3.RData")
  # save(vect4, file = "vect4.RData")
  # save(vect5, file = "vect5.RData")
  # save(vect6, file = "vect6.RData")
}#Fonction de parallélisation

##Fait Divers

FaitDiversLoad <- function() {
  FaitDivers <- read.csv("Fait Divers/FaitDivers.csv", sep = ";", header = FALSE)
  FD_VOL <- c("Va","VA","VB","VC","VD","VE","VM","VO","VP","VPP","VU","VV")

#   
#   setwd("C:/Users/a-gerbeaux/Documents/Documents/crime paris")
#   crime <- read.csv("crime.txt", header = FALSE, sep =";")
#   crime2 <- read.csv("crimetest2.csv")
#   
#   loc_crime <- data.frame(cbind(as.numeric(as.character(crime2[,2])),as.numeric(as.character(crime2[,1])),crime2[,8]))
#   ##Découpage Paris
#   loc_crime_Paris <- loc_crime[(loc_crime[,1])>2.145652&(loc_crime[,1]<2.615731)&(loc_crime[,2])>48.68764&(loc_crime[,2]<49.01233),]
}

DensiteFaitDivers <- function(){}


#load("MRH_2010.RData")

#####check

#load("0 Data Cleaning/Densite Vol/Densite_km_corr.RData")



VOL_densite_guillaume <- function(){
  
  ########################
  ##COUT MOYEN############
  ########################
  ########################
  ##Moyenne des Cout Moyens sur les 100 Voisins volés les plus proches
  #########################
  
  
  load("Targets.RData")
  load("distance_client_vol.RData")
  
  filtre_vol <- freqVOL > 0
  
  Cout_Moyen_100_VOL <- function(vect){
    local <- rank(vect, ties.method = c("first"))
    filtre <- local < 101
    return(mean(CoutMoyenVOL[filtre]))
    
  }
  
  CM_100e_VOL <- apply(distance_client_vol,1,Cout_Moyen_100_VOL)
  
  Cout_Moyen_100_VOL_adj <- function(vect){
    local <- rank(vect, ties.method = c("first"))
    filtre <- (local < 102)&(local > 1)
    return(mean(CoutMoyenVOL[filtre]))
    
  }
  
  CM_100e_VOL_adj <- apply(distance_client_vol[filtre_vol,],1,Cout_Moyen_100_VOL_adj)
  CM_100e_VOL[filtre_vol] <- CM_100e_VOL_adj
  
  save(CM_100e_VOL, file = "CM_100e_VOL.RData")
  
  
  
  load("Targets.RData")
  load("distance_client_vol.RData")
  
  filtre_vol <- freqVOL > 0
  
  Cout_Moyen_10_VOL <- function(vect){
    local <- rank(vect, ties.method = c("first"))
    filtre <- local < 11
    return(mean(CoutMoyenVOL[filtre]))
    
  }
  
  CM_10e_VOL <- apply(distance_client_vol,1,Cout_Moyen_10_VOL)
  
  Cout_Moyen_10_VOL_adj <- function(vect){
    local <- rank(vect, ties.method = c("first"))
    filtre <- (local < 12)&(local > 1)
    return(mean(CoutMoyenVOL[filtre]))
    
  }
  
  CM_10e_VOL_adj <- apply(distance_client_vol[filtre_vol,],1,Cout_Moyen_10_VOL_adj)
  CM_10e_VOL[filtre_vol] <- CM_10e_VOL_adj
  
  save(CM_10e_VOL, file = "CM_10e_VOL.RData")
  
  
  
  
  
  load("Targets.RData")
  load("distance_client_vol.RData")
  filtre_vol <- freqVOL > 0
  
  library(parallel)
  
  X_gui_CM <- c(1,5,10,20,50,70,100,150,200,500,1000,2000)
  
  CM_Xe_VOL <- function(x){
    
    
    CM_Xe_VOL_vect <- function(vect){
      local <- rank(vect, ties.method = c("first"))
      filtre <- local < (x+1)
      return(mean(CoutMoyenVOL[filtre]))
      
    }
        
    return(apply(distance_client_vol,1,CM_Xe_VOL_vect))
    
    
  }
    
  CM_X <- mclapply(X_gui_CM, CM_Xe_VOL_vect,mc.cores = 12)
  
  
  
  save(CM_X, file = "CM_X.RData")
  
  
  
  load("Targets.RData")
  load("distance_client_vol.RData")
  filtre_vol <- freqVOL > 0
  
  library(parallel)
  
  X_gui_CM <- c(1,5,10,20,50,70,100,150,200,500,1000,2000)
  
  CM_Xe_VOL_adj <- function(x){
    
    
    CM_Xe_VOL_vect_adj <- function(vect){
      local <- rank(vect, ties.method = c("first"))
      filtre <- (local < (x+2))&(local > 1)
      return(mean(CoutMoyenVOL[filtre]))
      
    }
    
    return(apply(distance_client_vol[filtre_vol,],1,CM_Xe_VOL_vect_adj))
    
    
  }
  
  CM_X_adj <- mclapply(X_gui_CM, CM_Xe_VOL_adj,mc.cores = 12)
  
  
  
  save(CM_X_adj, file = "CM_X_adj.RData")
  
  
  
  
  
  
  
  
  ########################
  ##Frequence############
  ########################
  ########################
  ##Moyenne des Fréquences sur les 1000 Voisins les plus proches
  #########################
  load("Targets.RData")
  load("index_1000.RData")
  
  Frequence_1000 <- function(vect){
    #return(sum(freqVOL[vect])/1000)
    return(mean(freqVOL[vect]))
  }
  
  freq_1000 <- apply(index_1000,1,Frequence_1000)
  
save(freq_1000, file = "freq_1000.RData")
  
}

Cleaning_CM_X <- function(){
  load("Densite Vol/CM_X.RData")
  load("Densite Vol/CM_X_adj.RData")
  load("Actuaire/Targets.RData")
  filtre_vol <- freqVOL > 0
  
  L <- length(CM_X[[1]])
  
  col <- paste(rep("CM_",12), as.character(c(1,5,10,20,50,70,100,150,200,500,1000,2000)), rep("e_VOL",12),sep = "")
  output <- matrix(NA,L,12)
  
  
  for(i in c(1:12)){
    output[,i] <- CM_X[[i]]
    output[filtre_vol,i] <- CM_X_adj[[i]]
  }
  
  colnames(output)<- col
  
  CM_Xe_VOL <- output
  save(CM_Xe_VOL, file = "Densite Vol/CM_Xe_VOL.RData")
  
}

Guillaume_freq_cluster <- function(){
#   
 
  
  
    load("Actuaire/VarAdd.RData")
    load("Actuaire/Targets.RData")
    
    load("VarAdd.RData")
    load("Targets.RData")
    
    pivot <- c(Coord_WFS84$long[1],Coord_WFS84$lat[1])
    
  geodetic.distance <- function(point1, point2) #Première Distance
  {
    R <- 6371
    p1rad <- point1 * pi/180
    p2rad <- point2 * pi/180
    d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
    d <- acos(d)
    R*d
  }
  
  fV1<- c(10,100,500,1000,2000,5000,7000,10000,15000,20000,50000,100000,200000)
  output_local <- matrix(NA,nrow(Coord_WFS84),1)
  
  for(i in c(1:nrow(Coord_WFS84))){
    output_local[i] <- geodetic.distance(c(Coord_WFS84$long[i],Coord_WFS84$lat[i]),pivot)
  }

  
  geodetic.distance <- function(p)
    return(sum((p-pivot)^2))
  system.time(mapply(function(x,y) geodetic.distance(c(x,y)), Coord_LII[, 1], Coord_LII[, 2]))
  
  geodetic.distance <- function(x,y)(return((x-pivot[1])^2 + (y-pivot[2])^2))
  system.time(mapply(geodetic.distance, Coord_LII[, 1], Coord_LII[, 2]))
  
  geodetic.distance <- function(p)
    return((p[1]-pivot[1])^2 + (p[2]-pivot[2])^2)
  system.time(mapply(geodetic.distance, Coord_LII[, 1], Coord_LII[, 2]))
  
  
  
  output_local2 <- rank(output_local,ties.method = c("first"))
  
  
  out_freq_local <- matrix(NA,1, length(fV))  
  for(j in c(1:length(fV))){
  filtre <- (output_local2 < (fV[j]+2))&(output_local2 > 1)
  out_freq_local[j] <- mean(freqVOL[filtre])
  }
  
  return(out_freq_local)
  
  library(parallel)
  setwd("/data/agerbeaux")
  load("VarAdd.RData")
  load("Targets.RData")
  
  
  table <- as.data.frame(cbind(Coord_LII,freqVOL))
  table_lapply <- as.data.frame(rbind(t(as.data.frame(Coord_LII)), id = 1:(nrow(Coord_LII))))
  
  fV1<- c(1,5,10,100,200,500,700,1000,
          2000,3000,5000,6000,7000,8000,9000,10000,
          12000,15000,17000,20000,25000,30000,40000,50000,70000,100000,
          150000,200000)
  
  
  VarFreqMoy <- function(p){
    xp <- p[1]
    yp <- p[2]
    id <- p[3]
    table_local <- table[-id,]
      dist <- function(x,y)(return((x-xp)^2 + (y-yp)^2))
  l <- table_local$freqVOL[order(mapply(dist, table_local$long, table_local$lat))]
      var_freq <- function(x) return(mean(l[1:x]))
  return(mapply(var_freq,fV1))
  }
  
 freq_X <- (mcmapply(VarFreqMoy,table_lapply,mc.cores = 22))
 save(freq_X, file = "freq_X_2.RData")
#   
  #output <- mapply(VarFreqMoy,table_lapply[,1:10])
  
 # system.time(mcmapply(VarFreqMoy,table_lapply[,1:100],mc.cores = 22))

  
}



