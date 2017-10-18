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


###Création des output
writeLines(paste("Création des outputs...","\n"))
L <- nrow(data_sinistre) #taille des observations totales
L_cm<- nrow(data_cm) #taille des observations sur les personnes volées
key <- str_replace_all(as.character(Sys.time()), "[[:punct:]]", " ")  ##clé unique qui va générer les fichiers créés
filtre <- matrix(NA,L,n)
for(i in c(1:n)){
  filtre[,i] <- rbinom(L,1,0.8) == TRUE #le split est de 70% pour le train set, et 30% pour le test set
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
                        maxnodes = 200)
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

