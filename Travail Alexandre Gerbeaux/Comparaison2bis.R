#####################################################
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
                  
#setwd("/data/agerbeaux/Comparaison Modele 2")
setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles")
load("DataComparaison.RData")
load("OutputComparaison 2014 08 14 18 20 07 RData")
###Création des output
writeLines(paste("Création des outputs...","\n"))
L <- nrow(data_sinistre) #taille des observations totales
L_cm<- nrow(data_cm) #taille des observations sur les personnes volées
key <- str_replace_all(as.character(Sys.time()), "[[:punct:]]", " ")  ##clé unique qui va générer les fichiers créés




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




writeLines(paste("\n"))
writeLines(paste("Chargement Terminé !","\n"))

#setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/output")

writeLines(paste("\n"))
writeLines(paste("-Démarrage de la boucle-","\n"))
for(i in c(1:n)){
  #génération du split
  filtre_entrainement <- filtre[,i]
  filtre_CM <- filtre_entrainement[data_freq[,"nbVOL"]>0]
  
  #génération des observations
  #observation en charge
  y.ob.e <- data_sinistre[filtre_entrainement,8]
  y.ob.t <- data_sinistre[!filtre_entrainement,8]
  #output_fitted_train[,(i*6-5)] <- reblock(y.ob.e,L)
  #output_fitted_test[,(i*6-5)] <- reblock(y.ob.t,L)
  #observation en nb de VOL
  y.ob.e.freq <- data_freq[filtre_entrainement,8]
  y.ob.t.freq <- data_freq[!filtre_entrainement,8]
  #output_fitted_train_freq[,i*3-2] <- reblock(y.ob.e.freq,L)
  #output_fitted_test_freq[,i*3-2] <- reblock(y.ob.t.freq,L)
  #observation en cout moyen
  y.ob.e.cm <- data_cm[filtre_CM,8]
  y.ob.t.cm <- data_cm[!filtre_CM,8]
  #output_fitted_train_cm[,i*3-2] <- reblock(y.ob.e.cm,L_cm)
  #output_fitted_test_cm[,i*3-2] <- reblock(y.ob.t.cm,L_cm)
  
  
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
                        ntree = 100,
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



}

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
     file = paste("OutputComparaison2",key,"RData"))