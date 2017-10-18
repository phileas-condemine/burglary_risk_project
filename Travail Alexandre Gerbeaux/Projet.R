
#MRH Project Alexandre Gerbeaux


############################################################################################################################
######################################################DATA CLEANING###########################################################
############################################################################################################################


library(FNN)
library(proj4)
#ptransformation
library(rgdal) 
library(sp)
library(fossil)
setwd("C:/Users/a-gerbeaux/Documents/0 PROJET")


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
#input : Base de Doan (DataBrut)
#output : - Base de Doan cleané et seulement sur Paris&Petite Couronne avec les coordonnées en lambert II remplacé par les coordonnées GPS (DataParis)
########: - Variables additionnelles : l'Exposition client, le filtre d'exposition (TRUE quand les clients sont restés + de 6 mois), l'id client
#######################################les coordonnées en Lambert II et le filtre qui ne conserve que les clients Paris&Petite Couronne.

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
#intput : DataParis
#output : variables actuariels du contrats DGGLM

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
#intput : DataParis
#output : nombre de Vol, fréquence de Vol (nombre de Vol net d'exposition), Cout Moyen, Chargement Vol et Sinistre Vol (Chargement Vol net d'exposition)
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
#intput DGGLM et targets
#output les différents data base avec predictors + cible (en Sinistre, en nbVOL, en Frequence et en cout moyen)
#également une version du DGGLM en catégorielle binaire



Comparaison <- function(){#####################################################
                          ##############COMPARAISON DES MODELES################
                          #####################################################
                          ##Alexandre Gerbeaux
                          
                          ###INPUT
                          n <- 50 #nombre d'itérations
                          
                          #setwd("/data/agerbeaux/Comparaison Modele 2")
                          writeLines(paste("Lancement du Code...","\n"))
                          
                          #Initialisation et chargement des donn?es
                          library(rpart)
                          library(cplm)
                          library(randomForest)
                          library(stringr)
                          # 
                          # ###chargement des données
                          # writeLines(paste("Chargement des données...","\n"))
                          # setwd("C:/Users/a-gerbeaux/Documents/Documents/Les Modèles")
                          #   load(file = "FeaturesTarget.RData")
                          #   load(file = "featuresfactors.RData")
                          #   load(file = "Expo.RData")
                          #   load(file = "DGGLMnbVOL.RData")
                          # setwd("C:/Users/a-gerbeaux/Documents/Documents/Variables du Modèle/cpart")
                          #   load(file = "dataAR.RData")
                          #   load(file = "observationsVOL.RData")
                          # writeLines(paste("...Ok","\n"))
                          # 
                          # 
                          # 
                          # 
                          # 
                          # ###Modification des données (filtre sur les expositions inférieurs à 6 mois)
                          # writeLines(paste("Modification des données...","\n"))
                          # filtre_expo0.5 <- Expo2 > 0.5
                          # data_sinistre <- DGGLMVolTweedie[filtre_expo0.5,] 
                          # filtre_expo0.5_cm <- filtre_expo0.5[DGGLMVolTweedie[,8]>0]
                          # data_nbVol <- DGGLMnbVOL[filtre_expo0.5,] #nb VOL sans division par l'exposition
                          # data_cm <- DGGLMVOLCoutMoyen[filtre_expo0.5_cm,]
                          # predict <- X2[filtre_expo0.5,]
                          # offset_expo <- Expo2[filtre_expo0.5]
                          # data_cm_RF <- data_CM[filtre_expo0.5_cm,]
                          # data_freq_RF <- data_freq[filtre_expo0.5,]
                          # data_sinistre_avec_exp <- data_sinistre #le sinistre avec division par l'exposition
                          # data_sinistre[,8] <- data_sinistre_avec_exp[,8] * offset_expo #le sinistre sans division par l'exposition 
                          # data_freq <- DGGLMVolFrequence[filtre_expo0.5,]
                          # rm(DGGLMVOLCoutMoyen,DGGLMVolFrequence,DGGLMVolTweedie,DGGLMnbVOL,X2,Expo2, filtre_expo0.5, filtre_expo0.5_cm,data_CM)
                          # 
                          # cm <- (data_sinistre[data_sinistre[,8]>0,8])/(data_nbVol[data_nbVol[,8]>0,8])
                          #  data_cm2 <- cbind(data_sinistre[data_sinistre[,8]>0,-8],cm)
                          # writeLines(paste("...Ok","\n"))
                          
                          # 
                          # writeLines(paste("Chargement des données...","\n"))
                          # setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/0 Data Cleaning")
                          # load("Actuaire/DataModeles.RData")
                          # load("Actuaire/VarAdd.RData")
                          # rm(Coord_LII,Coord_WFS84,filtre_75,filtre_exposition,id_client)
                          # writeLines(paste("...Ok","\n"))
                          # 
                          # writeLines(paste("Interprétation des données par l'algorithme...","\n"))
                          # data_freq <- DGGLMnbVOL #la fréquence doit etre sans exposition pour le glm
                          # data_freq_RF <- DGGLMFrequence[,c(2,3,4,5,6,8)] #la fréquence avec exposition pour le Random Forest
                          # rm(DGGLMFrequence)
                          # data_cm <- DGGLMVOLCoutMoyen
                          # data_cm_RF <-DGGLMVOLCoutMoyen[,c(2,3,4,5,6,8)]
                          # rm(DGGLMVOLCoutMoyen)
                          # data_sinistre <- DGGLMVOLSinistre
                          # data_sinistre[,8] <- data_sinistre[,8]*Exposition #à cause du Gamma et du Tweedie, le sinistre ne doit pas avoir d'exposition
                          # offset_expo <- Exposition
                          # rm(Exposition)
                          # predict <- X
                          # rm(X, DGGLMVOLSinistre,DGGLMnbVOL)
                          # 
                          # 
                          # writeLines(paste("...Ok","\n"))
                          # 
                          #setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles")
                          # save(data_cm,data_cm_RF,data_freq,data_freq_RF,data_sinistre,predict,offset_expo, file = "DataComparaison.RData")
                          
                          load("DataComparaison.RData")
                          
                          ###Création des output
                          writeLines(paste("Création des outputs...","\n"))
                          L <- nrow(data_sinistre) #taille des observations totales
                          L_cm<- nrow(data_cm) #taille des observations sur les personnes volées
                          key <- str_replace_all(as.character(Sys.time()), "[[:punct:]]", " ")  ##clé unique qui va générer les fichiers créés
                          filtre <- matrix(NA,L,n)
                          for(i in c(1:n)){
                            filtre[,i] <- rbinom(L,1,0.7) == TRUE #le split est de 70% pour le train set, et 30% pour le test set
                          }
                          save(filtre, file = paste("filtre",key,".RData"))
                          
                          writeLines(paste("Création des outputs valeurs fitted...","\n"))
                          ##On sauvegarde les valeurs fitted
                          output_fitted_test <- matrix(NA,L,(6*n),
                                                       dimnames = list(c(1:L),
                                                                       rep(c("Obs","Bench","Gamma","Tweedie","FC/PG","FC/RF"),n)))
                          output_fitted_train <- matrix(NA,L,(6*n),
                                                        dimnames = list(c(1:L),
                                                                        rep(c("Obs","Bench","Gamma","Tweedie","FC/PG","FC/RF"),n)))
                          output_fitted_test_freq <- matrix(NA,L,(3*n),
                                                            dimnames = list(c(1:L),
                                                                            rep(c("Obs","FC/PG","FC/RF"),n)))
                          output_fitted_train_freq <- matrix(NA,L,(3*n),
                                                             dimnames = list(c(1:L),
                                                                             rep(c("Obs","FC/PG","FC/RF"),n)))
                          output_fitted_test_cm <- matrix(NA,L_cm,(3*n),
                                                          dimnames = list(c(1:L_cm),
                                                                          rep(c("Obs","FC/PG","FC/RF"),n)))
                          output_fitted_train_cm <- matrix(NA,L_cm,(3*n),
                                                           dimnames = list(c(1:L_cm),
                                                                           rep(c("Obs","FC/PG","FC/RF"),n)))
                          
                          writeLines(paste("Création des outputs métriques...","\n"))
                          ##On sauvegarde les métriques
                          output_metrique <- matrix(NA,n,30,
                                                    dimnames  = list(c(1:n),
                                                                     c("Benchm MSEt","Benchm MSEe","Benchm MAEt","Benchm MAEe","Benchm spearmant","Benchm spearmane",
                                                                       "Gamma MSEt","Gamma MSEe","Gamma MAEt","Gamma MAEe","Gamma spearmant","Gamma spearmane",
                                                                       "Tweedie MSEt","Tweedie MSEe","Tweedie MAEt","Tweedie MAEe","Tweedie spearmant","Tweedie spearmane",
                                                                       "FC/PG MSEt","FC/PG MSEe","FC/PG MAEt","FC/PG MAEe","FC/PG spearmant","FC/PG spearmane",
                                                                       "FC/RF MSEt","FC/RF MSEe","FC/RF MAEt","FC/RF MAEe","FC/RF spearmant","FC/RF spearmane")))
                          
                          output_metrique_test_freq <- matrix(NA,(2*n),3, dimnames = list(c(1:(2*n)),c("MSE","MAE","Spear"))) #D'abord GLM ensuite RF
                          output_metrique_train_freq <- matrix(NA,(2*n),3, dimnames = list(c(1:(2*n)),c("MSE","MAE","Spear")))
                          output_metrique_test_cm <-matrix(NA,(2*n),3, dimnames = list(c(1:(2*n)),c("MSE","MAE","Spear")))
                          output_metrique_train_cm <-matrix(NA,(2*n),3, dimnames = list(c(1:(2*n)),c("MSE","MAE","Spear")))
                          writeLines(paste("...Ok","\n"))
                          
                          
                          
                          
                          
                          
                          #définition fonctions
                          writeLines(paste("Définition des fonctions...","\n"))
                          fMSE <- function(vect){
                            return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
                          }
                          fMAE <- function(vect){
                            return(sum(sapply(vect,abs))/(length(vect)))
                          }
                          fspearman <- function(fit,obs){
                            cor(rank(fit),rank(obs))
                          }
                          reblock <- function(vect, N){
                            vect2 <- as.matrix(vect)
                            return(rbind(vect2,matrix(NA,(N-length(vect)),1)))  
                          }
                          
                          writeLines(paste("...Ok","\n"))
                          
                          
                          
                          # ##Initialisation des écritures
                          # writeLines(paste("Initialisation des écritures...","\n"))
                          # setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/output")
                          # write.csv(filtre, file = paste("filtre",key,".csv"),row.names = FALSE) #On sauvegarde les splits
                          # write.csv(output_fitted_test, file = paste("output_fitted_test",key,".csv"),row.names = FALSE) #On sauvegarde les fitted values
                          # write.csv(output_fitted_train, file = paste("output_fitted_train",key,".csv"),row.names = FALSE) #
                          # write.csv(output_fitted_test_freq, file = paste("output_fitted_test_freq",key,".csv"),row.names = FALSE) #
                          # write.csv(output_fitted_train_freq, file = paste("output_fitted_train_freq",key,".csv"),row.names = FALSE) #
                          # write.csv(output_fitted_test_cm, file = paste("output_fitted_test_cm",key,".csv"),row.names = FALSE) #
                          # write.csv(output_fitted_train_cm, file = paste("output_fitted_train_cm",key,".csv"),row.names = FALSE) #
                          # write.csv(output_metrique, file = paste("output_metrique",key,".csv"),row.names = FALSE) #On sauvegarde les metriques
                          # write.csv(output_metrique_test_freq, file = paste("output_metrique_test_freq",key,".csv"),row.names = FALSE) #
                          # write.csv(output_metrique_train_freq, file = paste("output_metrique_train_freq",key,".csv"),row.names = FALSE) #
                          # write.csv(output_metrique_test_cm, file = paste("output_metrique_test_cm",key,".csv"),row.names = FALSE) #
                          # write.csv(output_metrique_train_cm, file = paste("output_metrique_train_cm",key,".csv"),row.names = FALSE) #
                          # writeLines(paste("...Ok","\n"))
                          
                          writeLines(paste("\n"))
                          writeLines(paste("Chargement Terminé !","\n"))
                          
                          #setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/output")
                          
                          writeLines(paste("\n"))
                          writeLines(paste("-Démarrage de la boucle-","\n"))
                          for(i in c(1:n)){
                            #génération du split
                            filtre_entrainement = filtre[,i]
                            filtre_CM <- filtre_entrainement[data_freq[,"nbVOL"]>0]
                            
                            #génération des observations
                            #observation en charge
                            y.ob.e <- data_sinistre[filtre_entrainement,8]
                            y.ob.t <- data_sinistre[!filtre_entrainement,8]
                            output_fitted_train[,(i*6-5)] <- reblock(y.ob.e,L)
                            output_fitted_test[,(i*6-5)] <- reblock(y.ob.t,L)
                            #observation en nb de VOL
                            y.ob.e.freq <- data_freq[filtre_entrainement,8]
                            y.ob.t.freq <- data_freq[!filtre_entrainement,8]
                            output_fitted_train_freq[,i*3-2] <- reblock(y.ob.e.freq,L)
                            output_fitted_test_freq[,i*3-2] <- reblock(y.ob.t.freq,L)
                            #observation en cout moyen
                            y.ob.e.cm <- data_cm[filtre_CM,8]
                            y.ob.t.cm <- data_cm[!filtre_CM,8]
                            output_fitted_train_cm[,i*3-2] <- reblock(y.ob.e.cm,L_cm)
                            output_fitted_test_cm[,i*3-2] <- reblock(y.ob.t.cm,L_cm)
                            
                            
                            ###############
                            ###Benchmark###
                            ###############
                            y.fi.e.b <- mean(y.ob.e) * matrix(1,length(y.ob.e),1)
                            y.fi.t.b <- mean(y.ob.t) * matrix(1,length(y.ob.t),1)
                            output_fitted_train[,i*6-4] <- reblock(y.fi.e.b,L)
                            output_fitted_test[,i*6-4] <- reblock(y.fi.t.b,L)
                            res.t.b <- y.ob.t - y.fi.t.b
                            res.e.b <- y.ob.e - y.fi.e.b
                            #m?triques
                            output_metrique[i,1] <- fMSE(res.t.b)
                            output_metrique[i,2] <- fMSE(res.e.b)
                            output_metrique[i,3] <- fMAE(res.t.b)
                            output_metrique[i,4] <- fMAE(res.e.b)
                            #output_metrique[i,5] <- fspearman(y.fi.t.b,y.ob.t) ##spearman n'a pas de sens pour le benchmark
                            #output_metrique[i,6] <- fspearman(y.fi.e.b,y.ob.e) ##spearman n'a pas de sens pour le benchmark
                            
                            writeLines(paste("Benchmark :",i,"réussi","\n"))
                            
                            ###########
                            ###Gamma###
                            ###########
                            #entrainement
                            local_g <- data_sinistre[filtre_entrainement,]
                            m <- mean(local_g[,8])
                            local_g[,8]<- local_g[,8] + m 
                            glm_g <-glm(formula = SinistreVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +   factor(pieceobj_me)+factor(captxov_me)  + factor(CDREGION) ,
                                        local_g,
                                        family = Gamma(link = "log"), 
                                        offset = offset_expo[filtre_entrainement])
                            coeff <- coefficients(glm_g)
                            coeff[is.na(coeff)]<-0
                            y.fi.t.g <- exp(as.matrix(predict[!filtre_entrainement,]) %*% as.matrix(coeff)) - m
                            y.fi.e.g <- exp(as.matrix(predict[filtre_entrainement,]) %*% as.matrix(coeff)) - m
                            output_fitted_train[,i*6-3] <- reblock(y.fi.e.g,L)
                            output_fitted_test[,i*6-3] <- reblock(y.fi.t.g,L)
                            res.t.g <- y.ob.t - y.fi.t.g
                            res.e.g <- y.ob.e - y.fi.e.g
                            #m?triques
                            output_metrique[i,7] <- fMSE(res.t.g)
                            output_metrique[i,8] <- fMSE(res.e.g)
                            output_metrique[i,9] <- fMAE(res.t.g)
                            output_metrique[i,10] <- fMAE(res.e.g)
                            output_metrique[i,11] <- fspearman(y.fi.t.g,y.ob.t)
                            output_metrique[i,12] <- fspearman(y.fi.e.g,y.ob.e)
                            
                            writeLines(paste("Gamma :",i,"réussi","\n"))
                            
                            ############
                            ###Tweedie##
                            ############
                            #entrainement
                            glm_t <- cpglm(formula = SinistreVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +  factor(captxov_me) + factor(pieceobj_me) + factor(CDREGION),
                                           data =data_sinistre[filtre_entrainement,],
                                           offset = offset_expo[filtre_entrainement],
                                           control = list(bound.p = c(1.41, 1.59)))  
                            coeff <- coefficients(glm_t)
                            coeff[is.na(coeff)]<-0
                            y.fi.t.t <- exp(as.matrix(predict[!filtre_entrainement,]) %*% as.matrix(coeff))
                            y.fi.e.t <- exp(as.matrix(predict[filtre_entrainement,]) %*% as.matrix(coeff))
                            output_fitted_train[,i*6-2] <- reblock(y.fi.e.t,L)
                            output_fitted_test[,i*6-2] <- reblock(y.fi.t.t,L)
                            res.t.t <- y.ob.t - y.fi.t.t
                            res.e.t <- y.ob.e - y.fi.e.t
                            #m?triques
                            output_metrique[i,13] <- fMSE(res.t.t)
                            output_metrique[i,14] <- fMSE(res.e.t)
                            output_metrique[i,15] <- fMAE(res.t.t)
                            output_metrique[i,16] <- fMAE(res.e.t)
                            output_metrique[i,17] <- fspearman(y.fi.t.t,y.ob.t)
                            output_metrique[i,18] <- fspearman(y.fi.e.t,y.ob.e)
                            
                            writeLines(paste("Tweedie :",i,"réussi","\n"))
                            
                            #########
                            ###FC####
                            #########
                            #entrainement
                            glm_cm <- glm(formula = CoutMoyenVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +   factor(pieceobj_me)+factor(captxov_me)  + factor(CDREGION) ,
                                          data_cm[filtre_CM,], 
                                          family = Gamma(link = "log"))
                            glm_f <- glm(formula = nbVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +  factor(captxov_me) + factor(pieceobj_me) + factor(CDREGION),
                                         data =data_freq[filtre_entrainement,],
                                         offset = offset_expo[filtre_entrainement],
                                         family = poisson(link = "log"))
                            
                            #coeff freq
                            coeff <- coefficients(glm_f)
                            coeff[is.na(coeff)]<-0
                            #coeffcm
                            coeff.cm <- coefficients(glm_cm)
                            coeff.cm[is.na(coeff.cm)]<-0
                            names_coeff <- row.names(as.data.frame(coeff))
                            names_coeff_lost <- row.names(as.data.frame(coeff.cm))
                            n_coeff_cout_moyen_CV <- matrix(0,103,1, dimnames = list(names_coeff, "x"))
                            rownames(n_coeff_cout_moyen_CV) <- names_coeff
                            index <- match(names_coeff_lost,names_coeff)
                            coeff_brut<-as.data.frame(coeff.cm)
                            coeff_brut<-as.matrix(coeff_brut)
                            n_coeff_cout_moyen_CV[index]<-coeff_brut
                            n_coeff_cout_moyen_CV[is.na(n_coeff_cout_moyen_CV)]<-0
                            ###
                            y.fi.t.fc <- exp(as.matrix(predict[!filtre_entrainement,])%*%as.matrix(coeff))*exp(as.matrix(predict[!filtre_entrainement,]) %*% as.matrix(n_coeff_cout_moyen_CV))
                            y.fi.e.fc <- exp(as.matrix(predict[filtre_entrainement,])%*%as.matrix(coeff))*exp(as.matrix(predict[filtre_entrainement,]) %*% as.matrix(n_coeff_cout_moyen_CV))
                            
                            #  y.fi.t.fc <- predict( glm_f, newdata =data_freq[!filtre_entrainement,1:7]) *  predict(glm_cm, newdata = data_freq[!filtre_entrainement,1:7])
                            #  y.fi.e.fc <- predict( glm_f, newdata = data_freq[filtre_entrainement,1:7]) *  predict(glm_cm, newdata = data_freq[filtre_entrainement,1:7])
                            
                            
                            output_fitted_train[,i*6-1] <- reblock(y.fi.e.fc,L)
                            output_fitted_test[,i*6-1] <- reblock(y.fi.t.fc,L)
                            res.t.fc <- y.ob.t - y.fi.t.fc
                            res.e.fc <- y.ob.e - y.fi.e.fc
                            
                            #m?triques
                            output_metrique[i,19] <- fMSE(res.t.fc)
                            output_metrique[i,20] <- fMSE(res.e.fc)
                            output_metrique[i,21] <- fMAE(res.t.fc)
                            output_metrique[i,22] <- fMAE(res.e.fc)
                            output_metrique[i,23] <- fspearman(y.fi.t.fc,y.ob.t)
                            output_metrique[i,24] <- fspearman(y.fi.e.fc,y.ob.e)
                            
                            ###freq/CM
                            filtre_pred_CM_e <-  (data_freq[,"nbVOL"]>0)&(filtre_entrainement)
                            filtre_pred_CM_t <- (data_freq[,"nbVOL"]>0)&(!filtre_entrainement)
                            
                            
                            y.fi.e.fc.freq <-exp(as.matrix(predict[filtre_entrainement,])%*%as.matrix(coeff))
                            y.fi.t.fc.freq<-exp(as.matrix(predict[!filtre_entrainement,])%*%as.matrix(coeff))
                            y.fi.e.fc.cm<-exp(as.matrix(predict[filtre_pred_CM_e,])%*%as.matrix(n_coeff_cout_moyen_CV))
                            y.fi.t.fc.cm<-exp(as.matrix(predict[filtre_pred_CM_t,])%*%as.matrix(n_coeff_cout_moyen_CV))
                            
                            
                            output_fitted_train_freq[,i*3-1] <- reblock(y.fi.e.fc.freq,L)
                            output_fitted_test_freq[,i*3-1] <- reblock(y.fi.t.fc.freq,L)
                            output_fitted_train_cm[,i*3-1] <- reblock(y.fi.e.fc.cm,L_cm)
                            output_fitted_test_cm[,i*3-1] <- reblock(y.fi.t.fc.cm,L_cm)
                            
                            res.f.e.glm <- -(y.fi.e.fc.freq - data_freq_RF[filtre_entrainement,6])
                            res.f.t.glm <- -(y.fi.t.fc.freq - data_freq_RF[!filtre_entrainement,6])
                            res.cm.e.glm <- -(y.fi.e.fc.cm - data_cm[filtre_CM,8])
                            res.cm.t.glm <- -(y.fi.t.fc.cm -data_cm[!filtre_CM,8])
                            
                            output_metrique_test_freq[i,1] <- fMSE(res.f.t.glm)
                            output_metrique_test_freq[i,2] <- fMAE(res.f.t.glm)
                            output_metrique_test_freq[i,3] <- fspearman(y.fi.t.fc.freq,data_freq_RF[!filtre_entrainement,6])
                            
                            output_metrique_test_cm[i,1] <- fMSE(res.cm.t.glm)
                            output_metrique_test_cm[i,2] <- fMAE(res.cm.t.glm)
                            output_metrique_test_cm[i,3] <- fspearman(y.fi.t.fc.cm,data_cm_RF[!filtre_CM,6])
                            
                            output_metrique_train_freq[i,1] <- fMSE(res.f.e.glm)
                            output_metrique_train_freq[i,2] <- fMAE(res.f.e.glm)
                            output_metrique_train_freq[i,3] <- fspearman(y.fi.e.fc.freq,data_freq_RF[filtre_entrainement,6])
                            
                            output_metrique_train_cm[i,1] <- fMSE(res.cm.e.glm)
                            output_metrique_train_cm[i,2] <- fMAE(res.cm.e.glm)
                            output_metrique_train_cm[i,3] <- fspearman(y.fi.e.fc.cm,data_cm_RF[filtre_CM,6])
                            
                            
                            writeLines(paste("Poisson Gamma :","réussi",i,"\n"))
                            
                            ##########
                            ####RF####
                            ##########
                            #entrainement  
                            RF_f <- randomForest(formula = freqVOL~.,
                                                 data = data_freq_RF[filtre_entrainement,],
                                                 ntree = 50,
                                                 na.action = na.omit,
                                                 mtry = 3,
                                                 maxnodes = 100)
                            #xtest = data_freq_RF[!filtre_entrainement,30:34],
                            #ytest = data_freq_RF[!filtre_entrainement,35])
                            RF_cm <- randomForest(formula = CoutMoyenVOL~.,
                                                  data = data_cm_RF[filtre_CM,],
                                                  ntree = 50,
                                                  mtry = 3, 
                                                  maxnodes = 30)
                            #xtest = data_cm_RF[!filtre_CM,30:34],
                            #ytest = data_cm_RF[!filtre_CM,35])
                            
                            y.fi.t.rf <- predict(RF_f, newdata = data_freq_RF[!filtre_entrainement,]) *  predict(RF_cm, newdata = data_freq_RF[!filtre_entrainement,])
                            y.fi.e.rf <- predict(RF_f, newdata = data_freq_RF[filtre_entrainement,]) *  predict(RF_cm, newdata = data_freq_RF[filtre_entrainement,])
                            output_fitted_train[,i*6] <- reblock(y.fi.e.rf,L)
                            output_fitted_test[,i*6] <- reblock(y.fi.t.rf,L)
                            res.e.rf <- y.ob.e - y.fi.e.rf
                            res.t.rf <- y.ob.t - y.fi.t.rf
                            
                            #métriques
                            output_metrique[i,25] <- fMSE(res.t.rf)
                            output_metrique[i,26] <- fMSE(res.e.rf)
                            output_metrique[i,27] <- fMAE(res.t.rf)
                            output_metrique[i,28] <- fMAE(res.e.rf)
                            output_metrique[i,29] <- fspearman(y.fi.t.rf,y.ob.t)
                            output_metrique[i,30] <- fspearman(y.fi.e.rf,y.ob.e)
                            
                            
                            
                            ###Severity / Frequency
                            y.fi.e.rf.freq<-predict(RF_f, newdata = data_freq_RF[filtre_entrainement,])
                            y.fi.t.rf.freq<-predict(RF_f, newdata = data_freq_RF[!filtre_entrainement,])
                            y.fi.e.rf.cm<-predict(RF_cm, newdata = data_cm_RF[filtre_CM,])
                            y.fi.t.rf.cm <-predict(RF_cm, newdata = data_cm_RF[!filtre_CM,])
                            
                            output_fitted_train_freq[,i*3] <- reblock(y.fi.e.rf.freq,L)
                            output_fitted_test_freq[,i*3] <- reblock(y.fi.t.rf.freq,L)
                            output_fitted_train_cm[,i*3] <- reblock(y.fi.e.rf.cm,L_cm)
                            output_fitted_test_cm[,i*3] <- reblock(y.fi.t.rf.cm,L_cm)
                            
                            res.f.e.rf <- -(y.fi.e.rf.freq - data_freq_RF[filtre_entrainement,6])
                            res.f.t.rf <- -(y.fi.t.rf.freq - data_freq_RF[!filtre_entrainement,6])
                            res.cm.e.rf <- -(y.fi.e.rf.cm - data_cm_RF[filtre_CM,6])
                            res.cm.t.rf <- -(y.fi.t.rf.cm -data_cm_RF[!filtre_CM,6])
                            
                            output_metrique_test_freq[i+n,1] <- fMSE(res.f.t.rf)
                            output_metrique_test_freq[i+n,2] <- fMAE(res.f.t.rf)
                            output_metrique_test_freq[i+n,3] <- fspearman(y.fi.t.rf.freq,data_freq_RF[!filtre_entrainement,6])
                            
                            output_metrique_test_cm[i+n,1] <- fMSE(res.cm.t.rf)
                            output_metrique_test_cm[i+n,2] <- fMAE(res.cm.t.rf)
                            output_metrique_test_cm[i+n,3] <- fspearman(y.fi.t.rf.cm,data_cm_RF[!filtre_CM,6])
                            
                            output_metrique_train_freq[i+n,1] <- fMSE(res.f.t.rf)
                            output_metrique_train_freq[i+n,2] <- fMAE(res.f.t.rf)
                            output_metrique_train_freq[i+n,3] <- fspearman(y.fi.e.rf.freq,data_freq_RF[filtre_entrainement,6])
                            
                            output_metrique_train_cm[i+n,1] <- fMSE(res.cm.e.rf)
                            output_metrique_train_cm[i+n,2] <- fMAE(res.cm.e.rf)
                            output_metrique_train_cm[i+n,3] <- fspearman(y.fi.e.rf.cm,data_cm_RF[filtre_CM,6])
                            
                            
                            save(filtre,
                                 output_fitted_test,
                                 output_fitted_train,
                                 output_fitted_test_freq,
                                 output_fitted_train_freq,
                                 output_fitted_test_cm,
                                 output_fitted_train_cm,
                                 output_metrique,
                                 output_metrique_test_freq,
                                 output_metrique_train_freq,
                                 output_metrique_test_cm,
                                 output_metrique_train_cm,
                                 file = paste("OutputComparaison",key,"RData"))
                          }
                          # writeLines(paste("Random Forest :","réussi",i,"\n"))
                          # writeLines(paste("\n"))
                          # #Réécriture
                          # writeLines(paste("Réécriture en cours...","\n"))
                          #   setwd("C:/Users/a-gerbeaux/Documents/Documents/Mega comparaison/Output")
                          write.csv(filtre, file = paste("filtre",key,".csv"),row.names = FALSE) #On sauvegarde les splits
                          write.csv(output_fitted_test, file = paste("output_fitted_test",key,".csv"),row.names = FALSE) #On sauvegarde les fitted values
                          write.csv(output_fitted_train, file = paste("output_fitted_train",key,".csv"),row.names = FALSE) #
                          write.csv(output_fitted_test_freq, file = paste("output_fitted_test_freq",key,".csv"),row.names = FALSE) #
                          write.csv(output_fitted_train_freq, file = paste("output_fitted_train_freq",key,".csv"),row.names = FALSE) #
                          write.csv(output_fitted_test_cm, file = paste("output_fitted_test_cm",key,".csv"),row.names = FALSE) #
                          write.csv(output_fitted_train_cm, file = paste("output_fitted_train_cm",key,".csv"),row.names = FALSE) #
                          write.csv(output_metrique, file = paste("output_metrique",key,".csv"),row.names = FALSE) #On sauvegarde les metriques
                          write.csv(output_metrique_test_freq, file = paste("output_metrique_test_freq",key,".csv"),row.names = FALSE) #
                          write.csv(output_metrique_train_freq, file = paste("output_metrique_train_freq",key,".csv"),row.names = FALSE) #
                          write.csv(output_metrique_test_cm, file = paste("output_metrique_test_cm",key,".csv"),row.names = FALSE) #
                          write.csv(output_metrique_train_cm, file = paste("output_metrique_train_cm",key,".csv"),row.names = FALSE) #
                          # writeLines(paste("...Ok","\n"))
                          # 
                          # writeLines(paste("\n"))
                          # writeLines(paste(i,"ème itération terminée","\n"))
                          
                          
                          
                          #}
                          
                          
                          ###A supprimé
                          #decLocations <- quantile(data_cm$CoutMoyenVOL, probs = seq(0.1,0.9,by=0.1))
                          #dec <- findInterval(data_cm$CoutMoyenVOL,c(-Inf,decLocations, Inf))
                          # data <- data_cm[filtre_CM,]
                          # data <- data[complete.cases(data),]
                          # glm_cm2 <- glm(formula = CoutMoyenVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +   factor(pieceobj_me)+factor(captxov_me)  + factor(CDREGION),
                          #               data = data,
                          #               family = Gamma(link = "log"))
                          # 
                          # y.fi.2 <- predict(glm_cm2,newdata = data)
                          # 
                          # 
                          # #####data_cm naif
                          # 
                          # cm <- (data_sinistre[data_sinistre[,8]>0,8]*offset_expo[data_sinistre[,8]>0])/(data_nbVol[data_nbVol[,8]>0,8])
                          
                          
                          #        
                          #        
                          # VOL <- read.csv(file.choose())
                          # Exposition <- read.csv(file.choose())
                          
                          
                          #test des bons splits
                          # L <- nrow(data_freq)
                          # for(i in c(1:100)){
                          # set.seed(1)
                          # filtre_entrainement <- rbinom(L,1,0.8) == TRUE
                          # filtre_CM <- filtre_entrainement[data_freq[,"nbVOL"]>0]
                          # glm_cm <- glm(formula = CoutMoyenVOL ~ factor(piecesobjcdregion)+  factor(otpion_Franchise25) + factor(option_BdG) + factor(var_habit) +   factor(pieceobj_me)+factor(captxov_me)  + factor(CDREGION) ,
                          #               data_cm[filtre_CM,], 
                          #               family = Gamma(link = "log"))
                          # 
                          # try(predict(glm_cm, newdata = data_cm[filtre_CM,1:7]))
                          # print(length(glm_cm$coefficients))
                          # }
}
##Fonction qui va comparer tous les modèles




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
#Matchage des bouts de bases de données que m'a donné Anne Laure et un autre .csv contenant les coordonnées en lambert II étendu.
#On vire les NA sur les cooordonnées LII (inexploitable), et on ne garde que les caractéristiques suivantes : 
#Numéro de contrat, nombre de niveaux, data de construction, surface total, surface commercial


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
#cette fonction utilise les coordonnées LII de la base de batiment de BatimentDataLoad et de celle de la Base Clients
#on utilise un KNN pour chercher les 10 voisins plus proche pour chaque client (100 voisins pour le commerce)
#on définit les variables pour chaque caractéristiques (Nombre de niveaux, Date de construction et taux de commerce)
#Les fonctions des 3 caract sont définis de manière indépendante et sauvé aussi de manière indépendante.



##POI

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
#On récupère les POI de toute la france
#On quadrille sur Paris
#On sauvegarde que celle supérieur à 50 (strictement supérieur à 49)
#On stock le tout dans une liste de 44 éléments

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
#On convertit les coordonnées GPS en coordonnées LII, on utilise un knn croisé entre les LII clients et les LII du POI.
#On récupère les index en sorti du knn croisé, on repère ainsi le POI le plus proche du client et on calcul sa distance à partir des coordonnées GPS
#(j'ai plus confiance aux valeurs GPS d'open street map qu'à la conversion sous R pour la précision)
#Pour calculer la distance en coordonnée GPS, j'utilise la fonction geodetic distance à l'intérieur de la fonction


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
  
  
  
}
#Une partie de la fonction a été faite pour N = 100 en local
#la fonction a été fait sur le cluster pour N = 1000 (N= 10000 trop grand)
#le fichier s'appelle neigh_dens_1000.RData



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
  
}
#Extraction de prédicteurs, cette fonction sera lancé sur le cluster
#Nous avons seulement retourné dans notre algorithme les distances clients



##KNN

ClusterCM <- function(){
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
  
}
#Sur le Cluster, j'ai lancé une fonction qui calcul de manière brutale, la distance en coordonnées gps entre la base de données client et la base de donnnées
#clients volés.
#le fichier s'appelle distance_client_vol, il est trop gros pour etre chargé en local (7,5 Go)
#ClusterCM utilise ce fichier pour calculer les variables de la famille CM, dans un premier temps j'ordonne pour chaque client, sont client volé le plus proche
#et je moyenne le CM de chaque client volé jusqu'au xeme voisin le plus proche
#dans un second temps je refais le calcul sur les 3000 voisins volés car j'ai compté ces personnes en trop, et là je remplace par la moyenne du CM
#de chaque client volé entres son 2eme voisin volé et son x+1 eme voisin volé le plus proche (le 1er est lui même)
#parallélisé en lapply, donc ça sort une liste et j'ai remplacé à la main ensuite les CM
#Oui c'est un peu bourrin je reconnais :(


Clusterfreq <- function(){
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
}
#Fonction qui calcul ligne par ligne :
#toutes les distances entre le client et ses 300000voisins
#ordonne suivant le xeme voisin
#moyenne la fréquence
#lancé sur le cluster en mcmapply pour 22 coeurs.






############################################################################################################################
######################################################DATA MINING###########################################################
############################################################################################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
##fonction très pratique qui créé des multiplot de ggplot :)


Chargement_Data <- function() {
  load("0 Data Cleaning/Actuaire/DGGLM.RData")
  load("0 Data Cleaning/Actuaire/Targets.RData")
  load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
  load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictorsbonapriori.RData")
  load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
  load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
  load("0 Data Cleaning/Densite Vol/CM_100e_VOL.RData")
  load("0 Data Cleaning/Densite Vol/CM_10e_VOL.RData")
  load("0 Data Cleaning/Densite Vol/freq_1000.RData")
  load("0 Data Cleaning/Densite Vol/Densite_perc_adj.RData")
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  load("0 Data Cleaning/Densite Vol/freq_Xe_VOL.RData")
  
  
  colnames(Densite_km) <-  paste("freq",paste(colnames(Densite_km), "km", sep = ""), sep = " ")
  colnames(Densite_CM) <-  paste(rep("CM",5), paste(c("0.5","1","3","5","7.5"), "km", sep= ""))
  colnames(Densite_dist_client) <- paste(c("1e", "10e", "50e", "100e", "500e", "1000e"), rep("Voisin",6))
  colnames(Densite_km_client) <- paste(rep("nb Voisins",8), colnames(Densite_km_client))
  colnames(Densite_perc) <- c("5% Vol","10% Vol","30% Vol","50% Vol","1e Vol","10e Vol","50e Vol","100e Vol")
  DGGLMOD <-cbind((Extend_POI_Predictors/Densite_dist_client[,"1000e Voisin"]),DTConst,NB,CO,Densite_km_client,Densite_dist_client,Densite_km,Densite_perc,Densite_CM,freq_1000,freq_X,CM_Xe_VOL)
  rm(Extend_POI_Predictors,Densite_km_client,Densite_dist_client,NB,DTConst,Densite_km,Densite_perc,Densite_CM,DGGLM, ChargemVOL, nbVOL,CM_Xe_VOL,freq_1000)
  
  save(DGGLMOD, file = "1 Data Mining/DGGLMODD.RData")
}
##fonction qui charge toutes les features précédemment créé, qui éventuellement renomme celle qui ont un nom bizarre et sauvegarde le tout
##DGGLMODD

Clean_Data <- function(){
  load("1 Data Mining/DGGLMODD.RData")
  
  NaDetec2 <- function(vect){
    return(table(is.na(vect))[2])
  }
  
  
  local <- apply(DGGLMOD,2,NaDetec2)
  local[is.na(local)] <- 0
  local <- as.matrix(local)
  col <- names(local[local < 20000,])
  
  DGGLMOD <- DGGLMOD[,col]
  
  rm(local,col)
  save(DGGLMOD, file = "1 Data Mining/DGGLMODD_clean.RData")
  
}
##fonction qui vire les features qui possèdent + de 20 000 NA (8% de données mauvaises)
##DGGLMODD_Clean



Summary_Data<- function(){
  load("0 Data Cleaning/Actuaire/Targets.RData")
  
  load("1 Data Mining/DGGLMODD_clean.RData")
  
  CorTarget<- function(vect){
    filtre <- !(is.na(vect))
    filtre_CM <- !(is.na(vect[freqVOL >0]))
    output <- matrix(NA,6,1)
    output[1] <- cor(SinistreVOL[filtre], vect[filtre])
    output[2] <- cor(SinistreVOL[filtre], vect[filtre], method = "spearman")
    
    output[3] <- cor(freqVOL[filtre], vect[filtre])
    output[4] <- cor(freqVOL[filtre], vect[filtre],method = "spearman")
    
    output[5] <- cor(CoutMoyenVOL[filtre_CM],vect[(freqVOL > 0)&filtre])
    output[6] <- cor(CoutMoyenVOL[filtre_CM],vect[(freqVOL > 0)&filtre],method = "spearman")
    
    return(output)
  }
  
  summary_features <- apply(DGGLMOD,2,CorTarget)
  rownames(summary_features) <- c("Correlation Sinistre", "Correlation Sinistre Spearman",
                                  "Correlation Frequence Vol", "Correlation Frequence Vol Spearman",
                                  "Correlation Cout Moyen", "Correlation Cout Moyen Spearman")
  
  save(summary_features, file = "1 Data Mining/summary_features.RData")
  write.csv(summary_features, file =  "1 Data Mining/summary_features.csv", row.names = FALSE)
}
##fonction qui sort une matrice 6x(le nombre de features cleané) et qui calcul la corrélation de spearman, la corrélation classique entre la feature et le Sinistre/fréquence/Cout Moyen




rel_var <- function(main_title,y_title, vect,NVar, bool= TRUE){
  
  library(ggplot2)
  
  order_20 <- function(vect){
    local <-abs(vect)
    local2 <- local[order(local, decreasing = TRUE)]
    col <- names(local2[1:NVar])
    sortie <- vect[col]
    return(sortie)
  }
  
  col <- names(vect)
  vect0 <- (order_20(vect))
  vect2 <- abs(vect0)
  
  class_vect <- function(v){
    sortie <- sign(v)
    sortie <- as.character(sortie)
    sortie[sortie == "-1"] <- "anti-correlation"
    sortie[sortie == "1"] <- "correlation"
    return(factor(sortie))
  }
  
  x = names(vect2)
  y2 <- paste(floor(1000*vect2)/10, rep("%",length(vect2)))
  df <- data.frame(x = x, y = vect2, behavior = class_vect(vect0), y2 = y2)
  p <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
    coord_flip() +
    ggtitle(main_title) +
    ylab(y_title) +
    xlab("20 best variables")+
    geom_bar(colour="black", stat="identity")
  #guides(fill=FALSE) + 
  
  if(bool == TRUE){
    
    p <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
      coord_flip() +
      ggtitle(main_title) +
      ylab(y_title) +
      xlab("20 best variables")+
      geom_bar(colour="black", stat="identity") +
      scale_x_discrete(limits=rev(x))
    
  }
  if(bool == FALSE){
    p <- ggplot(data=df, aes(x=x, y=y)) +
      coord_flip() +
      ggtitle(main_title) +
      ylab(y_title) +
      xlab("20 best variables")+
      geom_bar(colour="black", stat="identity") +
      guides(fill=FALSE) + 
      scale_x_discrete(limits=rev(col))
    #scale_y_continuous(expand = c(0,0.01))+
    #theme(plot.margin = unit(c(2, 2, 4, 2), "cm"))+
    #geom_text(aes(label=y2), vjust=0, hjust = -0.1, colour = "black")
    
  }
  #scale_colour_gradientn(colours=c("red","blue"))
  
  return(p)
}
##fonction un peu générique qui doit sortir un ggplot
##Tu choisis un subset de summary_Data (en famille, en métrique et en target) avec le input "vect".
##La fonction te classe les 20 premières variables
##Et te trace un histogramme comme dans le rapport

Plot_Variables <- function(){
  load("1 Data Mining/summary_features.RData")
  load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
  load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  load("0 Data Cleaning/Densite Vol/freq_Xe_VOL.RData")
  
  nbPOI <- ncol(Extend_POI_Predictors)
  nbBat <- ncol(NB) + ncol(DTConst)
  nbCM <- ncol(CM_Xe_VOL)
  
  nbTotal <- ncol(summary_features)
  
  nbfreq <- length(nfreq_X)
  
  colPOI <- c(1:nbPOI)
  
  colKNN <- c((nbPOI+nbBat+1):nbTotal)
  
  colCM_X <- c((nbTotal - nbCM +1):nbTotal)
  
  colfreq_X <- c((nbTotal-nbCM-nbfreq+1):(nbTotal-nbCM))
  
  
  fig1 <- rel_var("Total Claim", "Correlation",summary_features[1,],20)
  fig2 <- rel_var("Total Claim", "Spearman Correlation",summary_features[2,],20)
  fig3 <- rel_var("Frequency", "Correlation",summary_features[3,],20)
  fig4 <- rel_var("Frequency", "Spearman Correlation",summary_features[4,],20)
  fig5 <- rel_var("Severity", "Correlation",summary_features[5,],20)
  fig6 <- rel_var("Severity", "Spearman Correlation",summary_features[6,],20)
  
  fig7 <- rel_var("KNN Total Claim", "Correlation",summary_features[1,colKNN],20)
  fig8 <- rel_var("KNN Total Claim", "Spearman Correlation",summary_features[2,colKNN],20)
  fig9 <- rel_var("KNN Frequency", "Correlation",summary_features[3,colKNN],20)
  fig10 <- rel_var("KNN Frequency", "Spearman Correlation",summary_features[4,colKNN],20)
  fig11 <- rel_var("KNN Severity", "Correlation",summary_features[5,colKNN],20)
  fig12 <- rel_var("KNN Severity", "Spearman Correlation",summary_features[6,colKNN],20)
  
  
  fig13 <- rel_var("POI Total Claim", "Correlation",summary_features[1,colPOI],20)
  fig14 <- rel_var("POI Total Claim", "Spearman Correlation",summary_features[2,colPOI],20)
  fig15 <- rel_var("POI Frequency", "Correlation",summary_features[3,colPOI],20)
  fig16 <- rel_var("POI Frequency", "Spearman Correlation",summary_features[4,colPOI],20)
  fig17 <- rel_var("POI Severity", "Correlation",summary_features[5,colPOI],20)
  fig18 <- rel_var("POI Severity", "Spearman Correlation",summary_features[6,colPOI],20)
  
  
  fig19 <- rel_var("CM X Total Claim", "Correlation",summary_features[1,colCM_X],12, bool = FALSE)
  fig20 <- rel_var("CM X Total Claim", "Spearman Correlation",summary_features[2,colCM_X],12,bool = FALSE)
  fig21 <- rel_var("CM X Frequency", "Correlation",summary_features[3,colCM_X],12,bool = FALSE)
  fig22 <- rel_var("CM X Frequency", "Spearman Correlation",summary_features[4,colCM_X],12,bool = FALSE)
  fig23 <- rel_var("CM X Severity", "Correlation",summary_features[5,colCM_X],12,bool = FALSE)
  fig24 <- rel_var("CM X Severity", "Spearman Correlation",summary_features[6,colCM_X],12,bool = FALSE)
  
  
  
  
  fig25 <- rel_var("Freq X Total Claim", "Correlation",summary_features[1,colfreq_X],28, bool = FALSE)
  fig26 <- rel_var("Freq X Total Claim", "Spearman Correlation",summary_features[2,colfreq_X],28,bool = FALSE)
  fig27 <- rel_var("Freq X Frequency", "Correlation",summary_features[3,colfreq_X],28,bool = FALSE)
  fig28 <- rel_var("Freq X Frequency", "Spearman Correlation",summary_features[4,colfreq_X],28,bool = FALSE)
  fig29 <- rel_var("Freq X Severity", "Correlation",summary_features[5,colfreq_X],28,bool = FALSE)
  fig30 <- rel_var("Freq X Severity", "Spearman Correlation",summary_features[6,colfreq_X],28,bool = FALSE)
  
  
  
  
  png('2 Data Visualization/Plot/Variables Total Claim.png',width = 1033, height = 568)
  multiplot(fig1,fig2,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables Frequency.png',width = 1033, height = 568)
  multiplot(fig3,fig4,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables Severity.png',width = 1033, height = 568)
  multiplot(fig5,fig6,cols = 2)
  dev.off()
  
  
  png('2 Data Visualization/Plot/Variables KNN Total Claim.png',width = 1033, height = 568)
  multiplot(fig7,fig8,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables KNN Frequency.png',width = 1033, height = 568)
  multiplot(fig9,fig10,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables KNN Severity.png',width = 1033, height = 568)
  multiplot(fig11,fig12,cols = 2)
  dev.off()
  
  
  png('2 Data Visualization/Plot/Variables POI Total Claim.png',width = 1033, height = 568)
  multiplot(fig13,fig14,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables POI Frequency.png',width = 1033, height = 568)
  multiplot(fig15,fig16,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables POI Severity.png',width = 1033, height = 568)
  multiplot(fig17,fig18,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables CM X Total Claim.png',width = 1033, height = 568)
  multiplot(fig19,fig20,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables CM X Frequency.png',width = 1033, height = 568)
  multiplot(fig21,fig22,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables CM X Severity.png',width = 1033, height = 568)
  multiplot(fig23,fig24,cols = 2)
  dev.off()
  
  
  png('2 Data Visualization/Plot/Variables freq X Total Claim.png',width = 1033, height = 568)
  multiplot(fig25,fig26,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables freq X Frequency.png',width = 1033, height = 568)
  multiplot(fig27,fig28,cols = 2)
  dev.off()
  
  png('2 Data Visualization/Plot/Variables freq X Severity.png',width = 1033, height = 568)
  multiplot(fig29,fig30,cols = 2)
  dev.off()
  
  #   png('2 Data Visualization/Plot/Variables CM X Severity.png',width = 1033, height = 568)
  #   fig23
  #   dev.off()
  
  
}
##Fonction assez moche qui utilise rel_var sous toutes les sous familles possibles, sous toutes les métriques possible, et qui save automatiquement
##les images en utilisant des multiplots



Chargement_Carte <- function(){
  
  
  ######Arrondissements
  France <- read.csv("2 Data Visualization/Carte/relations_boundary=administrative-admin_level=9.csv", header = FALSE)
  France <- France[,-6]
  Arrondissements <- c("1er Arrondissement", "2e Arrondissement",  "3e Arrondissement", "4e Arrondissement","5e Arrondissement","6e Arrondissement","7e Arrondissement","8e Arrondissement","9e Arrondissement","10e Arrondissement","11e Arrondissement","12e Arrondissement","13e Arrondissement","14e Arrondissement","15e Arrondissement","16e Arrondissement","17e Arrondissement","18e Arrondissement","19e Arrondissement","20e Arrondissement")
  vectParis <- France[,"V5"] %in% Arrondissements
  Paris <- France[vectParis,]
  
  
  #####Département
  Departement <- read.csv("2 Data Visualization/Carte/relations_boundary=administrative-admin_level=6-ref92939475.csv", header = FALSE)
  Departement <- Departement[,-6]
  
  save(Paris,Departement,file = "2 Data Visualization/Carte/MapParis.RData")
  
}
##Charge toutes les données Open Street Map de contours de map que nous a gentilment donné Amaury
##La fonction save en MapParis.RData qui ne contient que les contours des arondissements Parisien ainsi que la petite Couronne


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
    data <- data.frame(Coord_WFS84[freqVOL>0,],value = value)
    p<-p+geom_point(data =data, aes(x=long,y=lat, colour = value,size = 3) , alpha = 0.5)
    p<-p+scale_colour_gradientn(colours=color)
  }
  return(p)
}
##Fonction un peu générique qui plot une variable sur un fond de carte parisien
##Je suppose ici que la variable correspond à une valeur client donc j'ai intégré un boolean,
##TRUE suppose que c'est une variable sur tous les clients (vecteur de taille 300 000)
##FALSE suppose que c'est une variable sur seulement les clients volés (vecteur de taille 3000)
##value est le vecteur en question
##title s'ajoutera en dessous du ineffacable "Paris et la petite Couronne"
##on peut controler la couleur


map_CM<- function(){
  library(ggplot2)
  library(lattice)
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  load("2 Data Visualization/Carte/MapParis.RData")
  
  
  
  nCM_X <- colnames(CM_Xe_VOL)
  
  number <- c(1,5,10,20,50,70,100,150,200,500,1000,2000)
  names(number) <- nCM_X 
  
  
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
  
  
  
  for(i in nCM_X){
    
    png(paste('2 Data Visualization/Plot/',i,'.png',sep = ''),width =1920, height = 1080)
    print(PlotMap(TRUE,CM_Xe_VOL[,i],paste("Moyenne des Couts Moyens des ",number[i] ,"voisins volés",sep = ""),rainbow(7)))
    dev.off()
  }
}
map_freq<- function(){
  library(ggplot2)
  library(lattice)
  load("0 Data Cleaning/Densite Vol/freq_X_2.RData")
  load("2 Data Visualization/Carte/MapParis.RData")
  
  freq_X <- t(freq_X)
  
  number<- c(1,5,10,100,200,500,700,1000,
             2000,3000,5000,6000,7000,8000,9000,10000,
             12000,15000,17000,20000,25000,30000,40000,50000,70000,100000,
             150000,200000)
  
  nfreq_X <- paste(rep("freq_",28), number,rep("e_VOL",28), sep = "")
  
  colnames(freq_X) <- nfreq_X
  
  
  
  names(number) <- nfreq_X 
  
  
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
  
  
  
  for(i in nfreq_X){
    
    png(paste('2 Data Visualization/Plot/',i,'.png',sep = ''),width =1920, height = 1080)
    print(PlotMap(TRUE,freq_X[,i],paste("Moyenne des Fréquences des ",number[i] ,"voisins volés",sep = ""),rainbow(7)))
    dev.off()
  }
}
##Fonctions qui utilisent plot Map et sortent toutes les map associées aux CM et freq



SauvegardeDataModele <- function(){
  
  load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
  load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
  load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  load("0 Data Cleaning/Densite Vol/freq_Xe_VOL.RData")
  
  for(i in 1:ncol(Densite_dist_client)){
    Densite_dist_client[is.nan(Densite_dist_client[,i]),i] <- 0
    
    
  }
  
  OpenData <- cbind(Extend_POI_Predictors/Densite_dist_client[,6],
                    DTConst,
                    NB,
                    CO,
                    Densite_dist_client,
                    freq_X,
                    CM_Xe_VOL)
  
  save(OpenData, file = "3 Entrainement/OpenData.RData")  
}
##Fonction qui va servir de base aux modèles Open Data, quelques features (principalement les + NA) ont été retirés, les POI ont été normalisé
##Nom : OpenData


############################################################################################################################
######################################################ENTRAINEMENT###########################################################
############################################################################################################################


ClusterOptimisationParameterCM <-function(){
setwd("/data/agerbeaux/Entrainement")
load("OpenData.RData")
load("Targets.RData")


library(randomForest)
library(parallel)
library(cvTools)
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
Nn <- 10
Nm <- 10
n_tree <- 200
vect_n <- c(1:Nn) * (150 %/% Nn)
vect_m <- c(1:Nm) * ((ncol(OpenData)) %/% Nm) 
combi <- expand.grid(vect_n,vect_m)




RF_o <- function(n,m,CV){
  rF_cout_moyen <- randomForest(x = data_cout_moyen[CV,-108],
                                y  = data_cout_moyen[CV,108],
                                xtest = data_cout_moyen[!CV,-108],
                                ytest  = data_cout_moyen[!CV,108],
                                ntree = n_tree,
                                maxnodes = n,
                                mtry = m)
  return(rF_cout_moyen$test$mse)
}
iRF_o <- function(n,m,matrix_CV){
  
  output <- matrix(NA,n_tree,smooth)
  for(i in 1:smooth){
    CV <- matrix_CV[,i]  
    output[,i] <- RF_o(n,m,CV)
  }
  return(apply(output,1,mean))
  
}

colnames_combi <- function(v){
  n <- v[1]
  m <- v[2]
  return(paste("(",n,",",m,")", sep = ""))
  
}
#return(mapply(iRF_o, combi[,1],combi[,2]))

cvFold <- cvFolds(nrow(data_cout_moyen), type = "random", R=2)

matrix_CV_1 <- matrix(NA,nrow(data_cout_moyen),5)
matrix_CV_2 <- matrix(NA,nrow(data_cout_moyen),5)
for(i in 1:5){
  matrix_CV_1[cvFold$subsets[,1],i]<- !(cvFold$which == i)
  matrix_CV_2[cvFold$subsets[,2],i]<- !(cvFold$which == i)
  
  
}

output1 <- mcmapply(iRF_o, combi[,1],combi[,2],matrix_CV_1,mc.cores = 22)
colnames(output1) <- apply(combi,1,colnames_combi)
save(output1, file = "ParametreCM1.RData")

output2 <- mcmapply(iRF_o, combi[,1],combi[,2],matrix_CV_2,mc.cores = 22)
colnames(output2) <- apply(combi,1,colnames_combi)
save(output2, file = "ParametreCM2.RData")

}
##Fonction qui va :
##Créer deux matrices 5 fold
##On calcul le MSE moyenné sur un 5 fold pour chaque combinaison de maxnodes et mtry dans un random forest
##L'idée et de chercher un paramètre optimal et de comparer la stabilité du paramètre en le comparant à un autre 5 fold
##cette fonction est parallélisé
##/!\ On a dit qu'il était plutôt préférable de controler le nodesize plutôt que le maxnode, pour éviter que l'algorithme focus sur les outlier
##la valeur a été rezoomé

SplitEquilibre <- functon(){
  ####Initilisation des Split
  set.seed(25)
  cvFold <- cvFolds(nrow(data_freq), type = "random", R=10)
  matrix_CV <- matrix(NA,nrow(data_freq),1)
  matrix_CV_local <- matrix(NA,nrow(data_freq),5)
  for(j in 1:10){
    for(i in 1:5){
      matrix_CV_local[cvFold$subsets[,j],i]<- !(cvFold$which == i)
    }
    matrix_CV <- cbind(matrix_CV,matrix_CV_local)
  }
  matrix_CV <- matrix_CV[,2:51]
  
  
  filtre_VOL <- freqVOL > 0
  matrix_CV_VOL <- matrix(NA,nrow(data_cout_moyen),50)
  for (i in 1:50){
    matrix_CV_VOL[,i] <-  matrix_CV[,i][filtre_VOL]
  }
  ##########################
  
  list_matrix <- list()
  for (i in 1:50){
    list_matrix[[i]] <- list(CV1 = matrix_CV_VOL[,i], CV2 = matrix_CV[,i])
    
  }
  save(list_matrix, file = "filtre_metriques2.RData")
}
##Fonction qui créé des splits équilibrés pour le calcul des métriques finales


ClusterOptimisationParameterFrequence <- function(){
  library(randomForest)
  library(parallel)
  setwd("/data/agerbeaux/Entrainement")
  load("Targets.RData")
  load("OpenData.RData")
  load("filtre_metriques2.RData")
  
  var_supp <- -c(68,69,70,71,72,96)
  OpenData <- OpenData[,var_supp]
  data_freq <- data.frame(cbind(OpenData,freqVOL))
  CV <- (list_matrix[[10]]$CV2)
  vect_n <- c(500,c(1:10)*1000)
  vect_m <- c(1:7)*14
  combi <- expand.grid(vect_n,vect_m)
  
  fMSE <- function(v1,v2){
    vect <- v1-v2
    vect <- as.matrix(vect)
    return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
  }
  
  RF_o <- function(n,m){
    rF_freq <- randomForest(x = data_freq[CV,-102],
                            y  = data_freq[CV,102],
                            ntree = 100,
                            nodesize = n,
                            mtry = m)
    return(fMSE(predict(rF_freq, newdata = data_freq[!CV,-102]),data_freq[!CV,102]))
  }
  
  colnames_combi <- function(v){
    n <- v[1]
    m <- v[2]
    return(paste("(",n,",",m,")", sep = ""))
    
  }
  #return(mapply(iRF_o, combi[,1],combi[,2]))
  output1 <- mcmapply(RF_o, combi[,1],combi[,2],mc.cores = 22)
  
  names(output1) <- apply(combi,1,colnames_combi)
  
  
  save(output1, file = "Parametrefreq.RData")
  
  fit = function(n){
    randomForest(x = data_freq[CV,-102],
                 y  = data_freq[CV,102],
                 ntree = n,
                 nodesize = 10000,
                 mtry = 100)
  }
  res = mclapply(rep(1,22),fit, mc.cores = 22)
  rF_freq <- Reduce(combine,res)
  
}
##Autre méthodologie, pour cette fonction, plus empirique, nous avons regardé la médiane d'un random forest overfitté pour la fréquence sur les
##split équilibré dans la fonction précédente, et nous balayons pour une plage de valeur
##(nécessité à cause du peu de temps restant pour mon stage et du temps de calcul)
##Nous avons controlé le nodesize ici

OptimisationParameterViz <- function(){
  
  min_output <-  apply(output,2,min)
  min_output[min_output== min(min_output)]

  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  output <- data.frame(output)
  x <- rep(1:n_tree,ncol(output))
  
  
  output <- output1
  
  df <- data.frame(melt(output),x)
  p <- ggplot(data = df, aes(x = x, y = value)) +
    geom_line(alpha = 0.6, aes(colour = Var2,group = Var2)) +
    xlab("number of trees") + 
    ylab("Mean Square Error") +
    ggtitle("MSE on a test set varying with parameters")+
    theme(legend.position="none")
  
  output_200 <- output[200,]
  
  load("3 Entrainement/Parameter/ParametreCM1.RData")
  load("3 Entrainement/Parameter/combi.RData")
  
  MSE <- (output_200)
  df <- data.frame(cbind(combi,output1))
  
  p1<-ggplot(data = df, aes(x= Var1, y = Var2)) + geom_point(aes(color = output1), size = 8)+
    xlab("max nodes") + ylab("features pick") + ggtitle("Parameter Optimization")+
    scale_colour_gradientn(colours=rainbow(7))
  
  
  load("3 Entrainement/Parameter/ParametreCM2.RData")
  
  output <- output2
  
  output_200 <- output[200,]
  MSE <- (output_200)
  df <- data.frame(cbind(combi,output2))
  p2<-ggplot(data = df, aes(x= Var1, y = Var2)) + geom_point(aes(color = output2), size = 8)+
    xlab("max nodes") + ylab("features pick") + ggtitle("Parameter Optimization")+
    scale_colour_gradientn(colours=rainbow(7))
  
  
  load("3 Entrainement/Parameter/ParametreCM1.RData")
  load("3 Entrainement/Parameter/ParametreCM2.RData")
  
  MSE_ratio <- (output1-output2)/output1
  df <- data.frame(cbind(combi,MSE_ratio))
  p3<-ggplot(data = df, aes(x= Var1, y = Var2)) + geom_point(aes(color = MSE_ratio), size = 8)+
    xlab("max nodes") + ylab("features pick") + ggtitle("Parameter Optimization Difference Ratio")+
    scale_colour_gradientn(colours=rainbow(7))
  
  
  
  png('3 Entrainement/Parameter/OptimizationParameter1.png',width = 2000, height = 1000)
  multiplot(p1,p2,cols = 2)
  dev.off()
  
  png('3 Entrainement/Parameter/OptimizationParameterRatio.png',width = 1000, height = 1000)
  p3
  dev.off()
  
  # 
  # output1_200<-output1[200,]
  # output1_200[min(output1_200)==output1_200]
  # 
  # output2_200<-output2[200,]
  # output2_200[min(output2_200)==output2_200]
  
  output1[output1==min(output1)]
  output2[output2==min(output2)]
  
}
##Fonction qui plot les résultats de ClusterOptimisationParamterCM sur une grille
##/!\Il faut charger les variables avant, je le fais car les output sont stockés sur le cluster, donc la viz je l'ai faite plutôt à la main à la fin
##mais l'idée est dans la fonction
##Une seule partie de la fonction est utilie si on veut regarder en freq, puisqu'on ne compare pas suivant deux 5 fold

ClusterProjetOutput <- function(){
  ###Chargement des Data
  
  setwd("/data/agerbeaux/Entrainement")
  library(randomForest)
  library(parallel)
  load("Targets.RData")
  load("filtre_metriques2.RData")
  load("OpenData.RData")
  var_supp <- -c(68,69,70,71,72,96)
  OpenData <- OpenData[,var_supp]
  
  data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
  data_freq <- data.frame(cbind(OpenData,freqVOL))
  data_prime_pure <- data.frame(cbind(OpenData,SinistreVOL))
  Targ <- 102
  
  
  output_RF <- function(X){
    CV1<- X$CV1
    CV2<- X$CV2
    rF_cout_moyen <- randomForest(x = data_cout_moyen[CV1,-Targ],
                                   y  = data_cout_moyen[CV1,Targ],
                                   ntree = 200,
                                   nodesize = 84,
                                   mtry = 90
    )
    
    rF_freq <- randomForest(x = data_freq[CV2,-Targ],
                            y  = data_freq[CV2,Targ],
                            ntree = 100,
                            nodesize = 500,
                            mtry = 84
    )
    
    
    output <- matrix(NA,18,1)
    
    
    fMSE <- function(v1,v2){
      vect <- v1-v2
      vect <- as.matrix(vect)
      return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
    }
    
    output_metrique <- function(RF,data,CV){
      output <- matrix(NA,6,1)
      y.fit.t <- predict(RF, newdata = data[!CV,-Targ])
      y.fit.e <- predict(RF, newdata = data[CV,-Targ])
      y.true.t <- data[!CV,Targ]
      y.true.e <- data[CV,Targ]
      output[1] <- cor(y.fit.t,y.true.t)
      output[2] <- cor(y.fit.e,y.true.e)
      output[3] <- cor(y.fit.t,y.true.t, method = "spearman")
      output[4] <- cor(y.fit.e,y.true.e, method = "spearman")
      output[5] <- fMSE(y.fit.t,y.true.t)
      output[6] <- fMSE(y.fit.e,y.true.e)
      
      rownames(output)<- c("cor test", "cor train", 
                           "cor spear test","cor spear train",
                           "MSE test", "MSE train")
      
      return(output)
    }
    
    output_metrique_agg <- function(RF1,RF2,data,CV){
      output <- matrix(NA,6,1)
      y.fit.t <- predict(RF1, newdata = data[!CV,-Targ]) * predict(RF2, newdata = data[!CV,-Targ]) 
      y.fit.e <- predict(RF1, newdata = data[CV,-Targ]) * predict(RF2, newdata = data[CV,-Targ]) 
      y.true.t <- data[!CV,Targ]
      y.true.e <- data[CV,Targ]
      output[1] <- cor(y.fit.t,y.true.t)
      output[2] <- cor(y.fit.e,y.true.e)
      output[3] <- cor(y.fit.t,y.true.t, method = "spearman")
      output[4] <- cor(y.fit.e,y.true.e, method = "spearman")
      output[5] <- fMSE(y.fit.t,y.true.t)
      output[6] <- fMSE(y.fit.e,y.true.e)
      
      rownames(output)<- c("cor test", "cor train", 
                           "cor spear test","cor spear train",
                           "MSE test", "MSE train")
      
      return(output)
      
      
      
    }
    
    
    
    
    output <- rbind(output_metrique(rF_cout_moyen,data_cout_moyen, CV1),
                    output_metrique(rF_freq,data_freq, CV2),
                    output_metrique_agg(rF_cout_moyen,rF_freq,data_prime_pure,CV2))
    
    
    return(output)
  }
  
  
  output_OD<- mcmapply(output_RF,list_matrix, mc.cores= 22)
  save(output_OD, file = "output_metrique_102freq.RData")
}
##Fonction qui run deux random forest en CM et fréquence sur les Open Data (nous avons retirés au préalable quelques features pathologiques)
##le output nous renvoit le calcul des trois métriques (MSE, cor, cor spearman) sur les 50 splits.


Visualisation<- function(){
  
  load("3 Entrainement/VisualisationMetriques/output_metrique_102bis.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_ref.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_act.RData")
  
  output <- output_OD
  
  box_viz <- function(title,metrique,data){
    fig<- ggplot(data, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
      geom_boxplot() +
      ggtitle(title) +
      #scale_x_discrete(limits=c("Test","Train")) +
      #scale_fill_manual(values = color_axa, guide=FALSE)+ 
      xlab("Set") + 
      ylab(metrique)
    
    return(fig)
  }
  
  
  figure_list_act <- list()
  Models <- rep(c("OpenData","Actuary"), each = 50)
  
  title <- paste(rep(c("Severity","Frequency","Prime Pure"), each = 6),rep(c("Correlation Test","Correlation Train","Correlation Spearman Test","Correlation Spearman Train","MSE Test","MSE Train"),times = 3))
  metrique <- rep(c("Correlation","Correlation Spearman","MSE"),times = 3,each = 2)
  for(i in 1:18){
    data <- melt((cbind(output[i,],output_act[i,])))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_act[[i]] <- box_viz(title[i],metrique[i],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
  }
  figure_list_norm_act <- list()
  title <- c("Severity MSE test normalisé","Severity MSE train normalisé",
             "Frequency MSE test normalisé","Frequency MSE train normalisé",
             "Prime Pure MSE test normalisé", "Prime Pure MSE train normalisé")
  metrique <- c("MSE","MSE","MSE","MSE","MSE","MSE")
  Models <- rep(c("OpenData","Actuary"), each = 50)
  
  ci <- c(5,6,11,12,17,18)
  j<-1
  for(i in ci){
    data <- melt((cbind(output[i,]/(output_ref[j,]),output_act[i,]/(output_ref[j,]))))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_norm_act[[j]] <- box_viz(title[j],metrique[j],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
    j<-j+1
  }
  
  
  
}
##Visualisation des Output en box plot



