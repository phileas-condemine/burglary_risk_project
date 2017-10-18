setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/")
library(ggplot2)
library(reshape2)
library(corrplot)

Ancien <- function(){
# 
# QQuant <- function(Vect,N){
#   findInterval(Vect,
#                quantile(Vect, probs = seq(0,1,(1/(N))), na.rm = FALSE),
#                rightmost.closed = FALSE,
#                all.inside = TRUE)
#   
# }
# QQInt <- function(Vect,N) {
#   
#   Delta <- (max(Vect)-min(Vect))/N
#   
#   findInterval(Vect,
#                seq(min(Vect),max(Vect),Delta),
#                rightmost.closed = FALSE,
#                all.inside = TRUE)
#   
# }
# 
# 
# NaDetec <- function(vect){
#   return(table(is.na(vect))[2]>0)
# }
# 
# 
# 
# NanDetec <- function(vect){
#   return(table(is.nan(vect))[2] > 0)
# }
# 
# 
# 
# CorTarget<- function(vect){
#   filtre <- !(is.na(vect))
#   filtre_CM <- !(is.na(vect[freqVOL >0]))
#   output <- matrix(NA,3,1)
#   output[1] <- cor(SinistreVOL[filtre], vect[filtre])  
#   output[2] <- cor(freqVOL[filtre], vect[filtre])
#   output[3] <- cor(CoutMoyenVOL[filtre_CM],vect[(freqVOL > 0)&filtre])
#   return(output)
# }
# 
# ChargementData <- function(){
#   #Chargement Data
#   load("0 Data Cleaning/Actuaire/DGGLM.RData")
#   load("0 Data Cleaning/Actuaire/Targets.RData")
#   load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
#   load("0 Data Cleaning/POI/POI_Predictors.RData")
#   #load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictors.RData")
#   load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictorsbonapriori.RData")
#   load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
#   load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
#   colnames(Densite_CM) <-  c("0.5 CM","1 CM","3 CM","5 CM","7.5 CM")
# 
#   
#   DGGLMOD <-cbind(Extend_POI_Predictors,Densite_km_client,NB,DTConst,Densite_dist_client,Densite_km,Densite_perc,Densite_CM,DGGLM)
#   
#   apply(DGGLMOD,2,NaDetec)#Pb : 50m,100m,200m,500m,1km,3km,5km,7km500,1eClient,50eClient,0.5,1,3,5,7.5, et 0.5CM,1CM,3CM,5CM,7.5CM 
#   #Cleaning Data
#   #POI_Predictors OK
#   #NB OK
#   #Densite_perc OK
#   #Densite_km_client pas OK
# 
#   CorOD <- apply(DGGLMOD[,1:(ncol(DGGLMOD)-7)],2,CorTarget)
#   
#   ###Les meilleurs POI :
#   rescalePOI <- Extend_POI_Predictors/Densite_dist_client[,6]
#   corOD2 <- apply(Extend_POI_Predictors/Densite_dist_client[,6],2,CorTarget)
#   filtre <- abs(corOD2[3,]) > 0.1
#   bisBest_POI <- colnames((t(corOD2[3,filtre])))
#   rm(filtre)
#   
#   for(i in bisBest_POI){
#  boxplot(CoutMoyenVOL ~ findInterval(rescalePOI[freqVOL >0,i],
#                                                  quantile(rescalePOI[freqVOL >0,i], probs = seq(0,1,0.25), na.rm = FALSE),
#                                       rightmost.closed = TRUE, all.inside = FALSE), main = i)
#   
#   #plot(rescalePOI[freqVOL>0,i],CoutMoyenVOL, main = i)
#                                                  
#   }
# }
# load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictors2.RData")
# Densite_km <- rbind(Densite_km[1:250000,], Densite_km[250002:nrow(Densite_km),])
# test <- Densite_km[,1] - freqVOL
# plot(Densite_km[freqVOL >0,1],CoutMoyenVOL)
# 
# boxplot(CoutMoyenVOL ~ findInterval(Densite_CM[freqVOL >0,3],
#                                                              quantile(Densite_CM[freqVOL >0,3], probs = seq(0,1,0.1), na.rm = TRUE),
#                                     rightmost.closed = TRUE))
# 
# load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictorsbonapriori.RData")
# 
# # 
# # boxplot(SinistreVOL[freqVOL >0] ~ findInterval(Densite_perc[freqVOL >0,2],
# #              quantile(Densite_perc[freqVOL >0,2], probs = seq(0,1,0.1), na.rm = FALSE))
# # 
# # plot(Densite_perc[freqVOL > 0,5], CoutMoyenVOL)
# # 
# # x <- Densite_perc[,1:4]
# # 
# # PCA <-  princomp(x, cor = TRUE)
# # 
# # 
# # 
# # plot(PCA$scores[freqVOL>0,1],CoutMoyenVOL)
# 
# 
# 
# 
# 
# 
# 
# TransformationData <- function(){
#   #Chargement Data
#   load("0 Data Cleaning/Actuaire/DGGLM.RData")
#   load("0 Data Cleaning/Actuaire/Targets.RData")
#   load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
#   load("0 Data Cleaning/Densite Vol/DensiteVOL_Predictorsbonapriori.RData")
#   load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
#   load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
#   colnames(Densite_CM) <-  c("0.5 CM","1 CM","3 CM","5 CM","7.5 CM")
#   
#   
#   ##Selection des meilleurs POI
#   
#   
#   ###Les meilleurs POI :
#   rescalePOI <- Extend_POI_Predictors/Densite_dist_client[,6]
#   corOD2 <- apply(Extend_POI_Predictors/Densite_dist_client[,6],2,CorTarget)
#   filtre <- abs(corOD2[3,]) > 0.1
#   bisBest_POI <- colnames((t(corOD2[3,filtre])))
#   rm(filtre)
#   
#   BestPOI <- rescalePOI[,bisBest_POI]
#   DGGLM <- DGGLM[,c(2,3,4,5,6)]
#   DGGLMOD <-cbind(BestPOI,Densite_km_client,NB,DTConst,Densite_dist_client,Densite_km,Densite_perc,Densite_CM,DGGLM)
#   
#   save(DGGLMOD, file = "1 Data Mining/DGGLMODBrut.RData")
#   
# }
# 
# CleanData <- function(){
#   load("1 Data Mining/DGGLMOD.RData")
#   
#   
# }
# 
# 
# 
# 
# 
# library(rpart)
# library(rpart.plot)
# library(randomForest)
# data_rpart <- as.data.frame(cbind(DGGLMOD[freqVOL > 0, ], CoutMoyenVOL))
# 
# DT <- rpart(CoutMoyenVOL~., data_rpart[filtre_CM,], control = rpart.control(cp = 0.0001))
# DT <- rpart(CoutMoyenVOL~., data_rpart[filtre_CM,])
# 
# 
# 
# fMSE(predict(DT), CoutMoyenVOL[filtre_CM])
# fspearman(predict(DT), CoutMoyenVOL[filtre_CM])
# fMSE(predict(DT, newdata = data_rpart[!filtre_CM,]), CoutMoyenVOL[!filtre_CM])
# fspearman(predict(DT, newdata = data_rpart[!filtre_CM,]), CoutMoyenVOL[!filtre_CM])
# 
# load("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/filtre 2014 08 14 18 20 07 .RData")
# i <- 1
# filtre_entrainement <- filtre[,i]
# filtre_CM <- filtre_entrainement[freqVOL>0]
# 
# 
# ##quick random forest
# data_rpart <- data_rpart[,-c(22,23,24,25,26)]
# #View(apply(data_rpart,2,NaDetec2))
# data_rpart <- data_rpart[complete.cases(data_rpart),]
# # RF <- randomForest(formula = CoutMoyenVOL~.,
# #                       data = data_rpart[filtre_CM,],
# #                       ntree = 20,
# #                       mtry = 8, 
# #                       maxnodes = 100,
# #                       xtest = data_rpart[!filtre_CM,1:63],
# #                       ytest = data_rpart[!filtre_CM,64])
# 
# #col <- paste( rep("AXA",58), colnames(data_rpart[,1:58]), sep = "_")
# col <- rep("AXA",58)
# col <- c(col, "CoutMoyenVOL")
# colnames(data_rpart) <- col
# 
# data_rpart
# 
# RF <- randomForest(x = data_rpart[filtre_CM,-59],
#                    y = data_rpart[filtre_CM,59],
#                    ntree = 20,
#                    mtry = 8, 
#                    maxnodes = 100,
#                    xtest = data_rpart[!filtre_CM,1:58],
#                    ytest = data_rpart[!filtre_CM,59])
# 
# 
# fMSE <- function(fit, obs){
#   error <- obs - fit
#   return(sum(sapply(error,function(x)(x^2)))/(length(error)))
# }
# 
# fspearman <- function(fit,obs){
#   cor(rank(fit),rank(obs))
# }

}

###############################################################
###Importance des Variables####################################
###############################################################



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



###############################
####Visualisation sur Carte####
###############################

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

Plot_Var_Paris <- function(){
  
load("0 Data Cleaning/Densite Vol/CM_100e_VOL.RData")
load("0 Data Cleaning/Densite Vol/freq_1000.RData")
load("0 Data Cleaning/Densite Vol/CM_10e_VOL.RData")
load("0 Data Cleaning/Actuaire/Targets.RData")
load("0 Data Cleaning/Actuaire/VarAdd.RData")
load("2 Data Visualization/Carte/MapParis.RData")

map1 <- PlotMap(TRUE,CM_100e_VOL,"Moyenne des Couts Moyens des 100 voisins volés",rainbow(7))

map2 <- PlotMap(TRUE,freq_1000,"Moyenne des fréquences de vol des 1000 voisins",rainbow(7))

map3 <- PlotMap(TRUE,CM_10e_VOL,"Moyenne des Couts Moyens des 10 voisins volés",rainbow(7))


png('2 Data Visualization/Plot/CM100.png',width = 1920, height = 1080)
map1
dev.off()
                             
png('2 Data Visualization/Plot/freq1000.png',width = 1920, height = 1080)
map2
dev.off()

png('2 Data Visualization/Plot/CM10.png',width = 1920, height = 1080)
map3
dev.off()

}



main <- function(){
  Chargement_Data()
  Clean_Data()
  Summary_Data()
  Plot_Variables()
  Plot_Var_Paris()
}



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


plot_Rapport <- function(){
  
  
  
  #/5 Rapport/Open Data Variables/Plot
  library(ggplot2)
  library(lattice)
  setwd("C:/Users/a-gerbeaux/Documents/0 PROJET")
  load("0 Data Cleaning/POI/ExtendPOIDataBase.RData")
  load("2 Data Visualization/Carte/MapParis.RData")
  load("0 Data Cleaning/Actuaire/Targets.RData")
  load("0 Data Cleaning/Actuaire/VarAdd.RData")
  load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
  load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
  
  load("1 Data Mining/summary_features.RData")
  load("0 Data Cleaning/POI/Extend_POI_Predictors.RData")
  load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  
  load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
  load("2 Data Visualization/Carte/MapParis.RData")
  load("0 Data Cleaning/Densite Vol/freq_Xe_VOL.RData")
  load("0 Data Cleaning/Densite Client/Predictors_densite_client.RData")
  load("0 Data Cleaning/Batiment/BatimentPredictors.RData")
  
  
  nCM_X <- colnames(CM_Xe_VOL)
  nfreq_X <- colnames(freq_X)
  colfreq_X <- c((nbTotal-nbCM-nbfreq+1):(nbTotal-nbCM))
  
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
  
  
  nbPOI <- ncol(Extend_POI_Predictors)
  nbBat <- ncol(NB) + ncol(DTConst)
  nbCM <- ncol(CM_Xe_VOL)
  nbTotal <- ncol(summary_features)
  colPOI <- c(1:nbPOI)
  colKNN <- c((nbPOI+nbBat+1):nbTotal)
  colCM_X <- c((nbTotal - nbCM +1):nbTotal)
  
  nNB <- colnames(NB)
  nCO <- colnames(CO)
  nDTConst <- colnames(DTConst)
  
  rel_var_Rapport <- function(y_title, vect,NVar, bool= TRUE, couleur = "black"){
    
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
      ylab(y_title) +
      xlab("20 best variables")+
      geom_bar(colour="black", stat="identity") +
      geom_text(aes(label=y2), vjust=0, hjust = 0, colour = "black")
    #guides(fill=FALSE) + 
    
    if(bool == TRUE){
      
      p <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
        coord_flip() +
        ylab(y_title) +
        xlab("")+
        geom_bar(colour="black", stat="identity") +
        scale_x_discrete(limits=rev(x)) +
        geom_text(aes(label=y2), vjust=+0.4, hjust = +1.1, colour = "black")
      
    }
    if(bool == FALSE){
      p <- ggplot(data=df, aes(x=x, y=y, fill = "black")) +
        coord_flip() +
        ylab(y_title) +
         xlab("")+
        geom_bar(colour="black", stat="identity") +
        guides(fill=FALSE) + 
        scale_x_discrete(limits=rev(col))+
        geom_text(aes(label=y2), vjust=+0.4, hjust = +1.1, colour = "black")+
        scale_fill_manual(values= couleur)
      
      #scale_y_continuous(expand = c(0,0.01))+
      #theme(plot.margin = unit(c(2, 2, 4, 2), "cm"))+
      #geom_text(aes(label=y2), vjust=0, hjust = -0.1, colour = "black")
      
    }
    #scale_colour_gradientn(colours=c("red","blue"))
    
    return(p)
  }
  
  ################################
  ##############POI###############
  ################################
  
  
  POIRAPPORT <- c("veterinary","townhall","bar","bicycle_rental","waste_basket","doctors","arts_centre","fast_food")
    
  for( i in POIRAPPORT){
    png(paste('5 Rapport/Open Data Variables/Plot/POI/Point_',i,'.png',sep = ''),width =400, height = 313)
    print(PlotParis(i)+geom_point(data =Extend_POI_data[[i]], aes(x=long,y=lat, colour = "black") , alpha = 1)+theme(legend.position="none"))
    dev.off()

      local_vecteur <- Extend_POI_Predictors[,i]/Densite_dist_client[,"1000e Client"]
    
     png(paste('5 Rapport/Open Data Variables/Plot/POI/Densite_',i,'.png',sep = ''),width =1000, height = 783)
    print(PlotMap(TRUE,local_vecteur,paste(i,"normalisé par la distance au 1000e voisin"),color = rainbow(7) ))
    dev.off()
    
    local_fig_1 <-PlotParis(i)+geom_point(data =Extend_POI_data[[i]], aes(x=long,y=lat, colour = "black") , alpha = 1)+theme(legend.position="none")
    local_fig_2 <- PlotMap(TRUE,local_vecteur,paste(i,"normalisé"),color = rainbow(7) )+theme(legend.position="none")
    png(paste('5 Rapport/Open Data Variables/Plot/POI/Both_',i,'.png',sep = ''),width =850, height = 313)
    print(multiplot(local_fig_1,local_fig_2, cols = 2))
    dev.off()
   
    
  }
  

  
  
  
  fig13 <- rel_var_Rapport( "Correlation",summary_features[1,colPOI],10)  + ggtitle("POI Prime Pure \n") + 
 theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #+  theme(legend.position="none")
  fig14 <- rel_var_Rapport( "Spearman Correlation",summary_features[2,colPOI],10)  +  ggtitle("") + 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig1 <- rel_var_Rapport( "Correlation",summary_features[3,colPOI],10) + ggtitle("POI Frequency \n") + 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig2 <- rel_var_Rapport( "Spearman Correlation",summary_features[4,colPOI],10)  + ggtitle("") + 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig3 <- rel_var_Rapport( "Correlation",summary_features[5,colPOI],10)  + ggtitle("POI Severity \n") + 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig4 <- rel_var_Rapport( "Spearman Correlation",summary_features[6,colPOI],10)  + ggtitle("") + 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  #500 - 391
  
  png('5 Rapport/Open Data Variables/Plot/POI/POI_Total_Claim.png',width = 500, height = 500)
  multiplot(fig13,fig14,cols = 1)
  dev.off()
  
  png('5 Rapport/Open Data Variables/Plot/POI/POI_Frequency.png',width = 500, height = 500)
  multiplot(fig1,fig2,cols = 1)
  dev.off()
  
  png('5 Rapport/Open Data Variables/Plot/POI/POI_Severity.png',width = 500, height = 500)
  multiplot(fig3,fig4,cols = 1)
  dev.off()
  
  
  ######################
  ########KNN###########
  ######################
 
 col_freq <- "olivedrab2"
 col_CM <- "goldenrod2"
  #########
  ###CM###
  #########
  
  fig23 <- rel_var_Rapport("Correlation",summary_features[5,colCM_X],12,bool = FALSE, couleur =  col_CM) + ggtitle("CM X Severity \n")+ 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig24 <- rel_var_Rapport("Spearman Correlation",summary_features[6,colCM_X],12,bool = FALSE, couleur =  col_CM)+ ggtitle("")+ 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 
  png('5 Rapport/Open Data Variables/Plot/KNN/CMSeverity.png',width = 500, height = 650)
  multiplot(fig23,fig24,cols = 1)
  dev.off()
  
  
  i <- "CM_50e_VOL"
  png(paste('5 Rapport/Open Data Variables/Plot/KNN/',i,'.png',sep = ''),width =1325, height = 1080)
  PlotMap(TRUE,CM_Xe_VOL[,i],paste("Moyenne des Couts Moyens des ",number[i] ,"voisins volés",sep = ""),(rainbow(7)))
  dev.off()
  
  #1000 - 783
  #########
  ###freq###
  ###########
 
 #nfreq_X <->
 subset_freq_X <-c("freq_100e_VOL","freq_500e_VOL","freq_1000e_VOL","freq_2000e_VOL","freq_5000e_VOL",
                   "freq_7000e_VOL","freq_10000e_VOL","freq_15000e_VOL","freq_20000e_VOL","freq_50000e_VOL",
                   "freq_1e+05e_VOL","freq_2e+05e_VOL")
 
  fig25 <- rel_var_Rapport("Correlation",summary_features[3,subset_freq_X],12,bool = FALSE, couleur = col_freq) + ggtitle("freq X Frequency \n")+ 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig26 <- rel_var_Rapport("Spearman Correlation",summary_features[4,subset_freq_X],12,bool = FALSE, couleur = col_freq)+ ggtitle("")+ 
   theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  png('5 Rapport/Open Data Variables/Plot/KNN/freqFrequency.png',width = 500, height = 650)
  multiplot(fig25,fig26,cols = 1)
  dev.off()
  
  
  i <- "freq_20000e_VOL"
  png(paste('5 Rapport/Open Data Variables/Plot/KNN/',i,'.png',sep = ''),width =1325, height = 1080)
  PlotMap(TRUE,freq_X[,i],paste("Moyenne des Fréquences des ",number_freq[i] ,"voisins volés",sep = ""),(rainbow(7)))
  dev.off()
  
  ############
  ###Aggrégé###
  #############

 
 col_freq <- "olivedrab2"
 col_CM <- "goldenrod2"
  rel_var_KNN_Agr <- function(y_title, vect,NVar, couleur){
    
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
      sortie <- names(v)
      sortie <- as.character(sortie)
      sortie <- substr(sortie,1,1)
      sortie[sortie == "f"] <- "Famille Fréquence"
      sortie[sortie == "C"] <- "Famille Coût-Moyen"
      return(factor(sortie))
    }
    
    x = names(vect2)
    y2 <- paste(floor(1000*vect2)/10, rep("%",length(vect2)))
    df <- data.frame(x = x, y = vect2, Type = class_vect(vect0), y2 = y2)
    p <- ggplot(data=df, aes(x=x, y=y, fill = Type)) +
      coord_flip() +
      ylab(y_title) +
      xlab(" ")+
      geom_bar(colour="black", stat="identity")+
      scale_x_discrete(limits=rev(x))+
      scale_fill_manual(values= couleur)+
      geom_text(aes(label=y2), vjust=+0.4, hjust = +1.1, colour = "black")
    #guides(fill=FALSE) + 
#     
#     if(bool == TRUE){
#       
#       p <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
#         coord_flip() +
#         ylab(y_title) +
#         xlab("")+
#         geom_bar(colour="black", stat="identity") +
#         scale_x_discrete(limits=rev(x))
#       
#     }
#     if(bool == FALSE){
#       p <- ggplot(data=df, aes(x=x, y=y)) +
#         coord_flip() +
#         ylab(y_title) +
#         xlab("")+
#         geom_bar(colour="black", stat="identity") +
#         guides(fill=FALSE) + 
#         scale_x_discrete(limits=rev(col))
#       #scale_y_continuous(expand = c(0,0.01))+
#       #theme(plot.margin = unit(c(2, 2, 4, 2), "cm"))+
#       #geom_text(aes(label=y2), vjust=0, hjust = -0.1, colour = "black")
#       
#     }
#     
    
    #scale_colour_gradientn(colours=c("red","blue"))
    
    return(p)
  }
  
  
  fig27 <- rel_var_KNN_Agr("Correlation",summary_features[1,c(nfreq_X,nCM_X)],15,c(col_CM,col_freq)) + ggtitle("KNN Prime pure \n")+ 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig28 <- rel_var_KNN_Agr("Spearman Correlation",summary_features[2,c(nfreq_X,nCM_X)],15,c(col_freq,col_CM))+ ggtitle("")+ 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  png('5 Rapport/Open Data Variables/Plot/KNN/KNNPrimePure.png',width = 500, height = 800)
  multiplot(fig27,fig28,cols = 1)
  dev.off()
  
  ####Densité###

figDensite <- PlotMap(bool = TRUE, log(Densite_dist_client[,6]),"logarithme de la distance avec le 1000e voisins",rainbow(7))
png('5 Rapport/Open Data Variables/Plot/Densite/Distance1000.png',width = 956, height = 694)
figDensite
dev.off()


  ###Bâtiment###

figDateDeConstruction <- PlotMap(bool = TRUE, (DTConst[,4]),"Moyenne de la Date de construction pour les 10 voisins",rainbow(7))
png('5 Rapport/Open Data Variables/Plot/Batiment/DateDeConstruction.png',width = 956, height = 694)
figDateDeConstruction
dev.off()

figNombreDeNiveaux <- PlotMap(bool = TRUE, (NB[,4]),"Moyenne du Nombre de niveaux pour les 10 voisins",rainbow(7))
png('5 Rapport/Open Data Variables/Plot/Batiment/NombreDeNiveaux.png',width = 956, height = 694)
figNombreDeNiveaux
dev.off()

figTauxCommerce <- PlotMap(bool = TRUE, (CO[,9]),"Moyenne du taux de commerce pour les 100 voisins",rainbow(7))
png('5 Rapport/Open Data Variables/Plot/Batiment/TauxCommerce.png',width = 956, height = 694)
figTauxCommerce
dev.off()


#histogram batiment
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

order_20 <- function(vect,NVar){
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
  ylab(y_title) +
  xlab("20 best variables")+
  geom_bar(colour="black", stat="identity") +
  geom_text(aes(label=y2), vjust=0, hjust = 0, colour = "black")
#guides(fill=FALSE) + 

if(bool == TRUE){
  
  p <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
    coord_flip() +
    ylab(y_title) +
    xlab("")+
    geom_bar(colour="black", stat="identity") +
    scale_x_discrete(limits=rev(x)) 
}

#hist 1
nNB <- c("NB1","NB3","NB5","NB10")
local <- order_20(summary_features[1,nNB],4)
oNB <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b1<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Prime Pure")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.015)

local <- order_20(summary_features[3,nNB],4)
behavior <- class_vect(local)
df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b2 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Frequency")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.0125)

local <- order_20(summary_features[5,nNB],4)
behavior <- class_vect(local)
df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b3<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Severity")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.125)

nDT <- c("DTConst1","DTConst3","DTConst5","DTConst10")
local <- order_20(summary_features[1,nDT],4)
oDT <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b4<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+
  ylim(0,0.015)
  #scale_fill_manual(values= "blue")

local <- order_20(summary_features[3,nDT],4)
behavior <- class_vect(local)
df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b5 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.0125)
  #scale_fill_manual(values= "blue")


local <- order_20(summary_features[5,nDT],4)
behavior <- class_vect(local)
df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b6<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.125)

nCO <- c("CO1","CO3","CO5","CO10","CO20","CO30","CO50","CO70","CO100")
local <- order_20(summary_features[1,nCO],9)
oCO <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b7<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+
  ylim(0,0.015)
#scale_fill_manual(values= "blue")

local <- order_20(summary_features[3,nCO],9)
behavior <- class_vect(local)
df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b8 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.0125)
#scale_fill_manual(values= "blue")


local <- order_20(summary_features[5,nCO],9)
behavior <- class_vect(local)
df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b9<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
#   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
#scale_fill_manual(values= "blue")+
  ylim(0,0.125)

png('5 Rapport/Open Data Variables/Plot/Batiment/Correlation.png',width = 839, height = 721)
multiplot(b1,b4,b7,b2,b5,b8,b3,b6,b9, cols = 3)
dev.off()


#hist 2
nNB <- c("NB1","NB3","NB5","NB10")
local <- order_20(summary_features[2,nNB],4)
oNB <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b1<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Prime Pure")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.0125)

local <- order_20(summary_features[4,nNB],4)
behavior <- class_vect(local)
df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b2 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Frequency")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.0125)

local <- order_20(summary_features[6,nNB],4)
behavior <- class_vect(local)
df <- data.frame(x = oNB, y = abs(local), behavior = behavior )
b3<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  ggtitle("Severity")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oNB))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  scale_fill_manual(values= "#00BFC4")+
  ylim(0,0.08)

nDT <- c("DTConst1","DTConst3","DTConst5","DTConst10")
local <- order_20(summary_features[2,nDT],4)
oDT <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b4<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+
  ylim(0,0.0125)
#scale_fill_manual(values= "blue")

local <- order_20(summary_features[4,nDT],4)
behavior <- class_vect(local)
df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b5 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.0125)
#scale_fill_manual(values= "blue")


local <- order_20(summary_features[6,nDT],4)
behavior <- class_vect(local)
df <- data.frame(x = oDT, y = abs(local), behavior = behavior )
b6<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oDT))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.08)

nCO <- c("CO1","CO3","CO5","CO10","CO20","CO30","CO50","CO70","CO100")
local <- order_20(summary_features[2,nCO],9)
oCO <- names(local)
behavior <- class_vect(local)

df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b7<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+
  ylim(0,0.0125)
#scale_fill_manual(values= "blue")

local <- order_20(summary_features[4,nCO],9)
behavior <- class_vect(local)
df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b8 <- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  ylim(0,0.0125)
#scale_fill_manual(values= "blue")


local <- order_20(summary_features[6,nCO],9)
behavior <- class_vect(local)
df <- data.frame(x = oCO, y = abs(local), behavior = behavior )
b9<- ggplot(data=df, aes(x=x, y=y, fill = behavior)) +
  coord_flip() +
  xlab("") +
  ylab("")+
  #   ggtitle("")+
  geom_bar(colour="black", stat="identity")+
  scale_x_discrete(limits=rev(oCO))+  
  theme(legend.position="none")+ 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  #scale_fill_manual(values= "blue")+
  ylim(0,0.08)

png('5 Rapport/Open Data Variables/Plot/Batiment/CorrelationSpearman.png',width = 839, height = 721)
multiplot(b1,b4,b7,b2,b5,b8,b3,b6,b9, cols = 3)
dev.off()


nBat <- c(nNB,nDTConst,nCO)

df <- as.data.frame(CO[,c(9)])
ggplot(melt(df), aes(x = value, fill = variable)) + geom_density(alpha = .3)

snBat <- substr(nBat,1,2)
vect_bat <- summary_features[1,nBat]

order_20 <- function(vect,NVar){
  local <-abs(vect)
  local2 <- local[order(local, decreasing = TRUE)]
  col <- names(local2[1:NVar])
  sortie <- vect[col]
  return(sortie)
}


class <- (snBat)
name <- (names(vect_bat))
y <- (abs(vect_bat))

df <- data.frame(name,class,y)

ggplot(df, aes(y = V2, x = reorder(V3,V2)))+ 
  geom_segment(aes(yend=V3), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=snBat)) +
#   geom_bar(stat="identity",aes(yend=V3), xend=0) +
#   coord_flip() +#+
 scale_colour_brewer(palette="Set1", limits=c("NL","AL","AB"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(snBat ~ ., scales="free_y", space="free_y")
  #facet_grid(snBat ~ .)


ggplot(df, aes(y = y, x = reorder(name,y)))+ 
   geom_bar(stat="identity") +
  facet_grid(class~.)
  #   coord_flip() +#+
  scale_colour_brewer(palette="Set1", limits=c("NL","AL","AB"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(snBat ~ ., scales="free_y", space="free_y")
#facet_grid(snBat ~ .)


fig13 <- rel_var_Rapport( "Correlation",summary_features[1,colPOI],10)  + ggtitle("POI Prime Pure \n") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #+  theme(legend.position="none")
fig14 <- rel_var_Rapport( "Spearman Correlation",summary_features[2,colPOI],10)  +  ggtitle("") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig1 <- rel_var_Rapport( "Correlation",summary_features[3,colPOI],10) + ggtitle("POI Frequency \n") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig2 <- rel_var_Rapport( "Spearman Correlation",summary_features[4,colPOI],10)  + ggtitle("") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig3 <- rel_var_Rapport( "Correlation",summary_features[5,colPOI],10)  + ggtitle("POI Severity \n") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig4 <- rel_var_Rapport( "Spearman Correlation",summary_features[6,colPOI],10)  + ggtitle("") + 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#500 - 391

png('5 Rapport/Open Data Variables/Plot/POI/POI_Total_Claim.png',width = 500, height = 500)
multiplot(fig13,fig14,cols = 1)
dev.off()

png('5 Rapport/Open Data Variables/Plot/POI/POI_Frequency.png',width = 500, height = 500)
multiplot(fig1,fig2,cols = 1)
dev.off()

png('5 Rapport/Open Data Variables/Plot/POI/POI_Severity.png',width = 500, height = 500)
multiplot(fig3,fig4,cols = 1)
dev.off()




#########correlation matrix

rev_matrix <- function(V){
  
  return(V[, rev(seq_len(ncol(V)))])
}

cor_matrix <- function( bool = TRUE, V){
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  
  if ( bool == TRUE){
  fig <- corrplot(cor(V), method="shade", shade.col=NA, tl.col="black", tl.srt=45,
                  col=col(200),addCoef.col="black", addcolorlabel="no", order="AOE")
  }
  if ( bool == FALSE){
    fig <- corrplot(cor(rev_matrix(V)), method="shade", shade.col=NA, tl.col="black", tl.srt=45,
                    col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")
  }
  
  return(fig)
}
order_20 <- function(vect,NVar){
  local <-abs(vect)
  local2 <- local[order(local, decreasing = TRUE)]
  col <- names(local2[1:NVar])
  sortie <- vect[col]
  return(sortie)
}
subset_freq_X <-c("freq_100e_VOL","freq_500e_VOL","freq_1000e_VOL","freq_2000e_VOL","freq_5000e_VOL",
                  "freq_7000e_VOL","freq_10000e_VOL","freq_15000e_VOL","freq_20000e_VOL","freq_50000e_VOL",
                  "freq_1e+05e_VOL","freq_2e+05e_VOL")
subset_Extend_POI_Predictors <- names(order_20(summary_features[1,colnames(Extend_POI_Predictors)],12))
features <- list(Extend_POI_Predictors = Extend_POI_Predictors[,subset_Extend_POI_Predictors],
                 DTConst = DTConst,
                 NB = NB,
                 CO = CO,
                 Densite_dist_client = Densite_dist_client[,4:6],
                 freq_X = freq_X[,subset_freq_X],
                 CM_Xe_VOL = CM_Xe_VOL)

nfeatures <- c("Extend_POI_Predictors","DTConst","NB","CO","Densite_dist_client","freq_X","CM_Xe_VOL")
rev_bool <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE)

for(i in 1:7){
  png(paste('5 Rapport/Open Data Variables/Plot/Correlation/',nfeatures[i],'.png', sep = ''),width = 500, height = 500)
  print(cor_matrix(bool = rev_bool[i],features[[i]]))
  dev.off()
  
}

features2 <- cbind(Extend_POI_Predictors/Densite_dist_client[,6],DTConst,NB,CO,Densite_dist_client[,4:6],freq_X,CM_Xe_VOL)
png('5 Rapport/Open Data Variables/Plot/Correlation/Correlation2.png',width = 3000, height = 3000, pointsize=50)
corrplot(cor(features2), method="shade", shade.col=NA, tl.col="white",addgrid.col = "grey", tl.srt=45 
         , title = "\n \n \n \n \n \n \n \n \n \n \n \n \n \n                         POI                       DT NB    CO D        fréquence            CM",mar=c(0,0,2,0))

dev.off()


DGGLMOD <-cbind((Extend_POI_Predictors/Densite_dist_client[,"1000e Voisin"]),DTConst,NB,CO,Densite_km_client,Densite_dist_client,Densite_km,Densite_perc,Densite_CM,freq_1000,freq_X,CM_Xe_VOL)



}

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


map_freq_espace_creatif<- function(){
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
    theme_xkcd <- theme(panel.grid.major =  element_blank(),
                        panel.grid.minor =  element_blank(),
                        panel.background =  element_blank(),
                        #panel.background = element_rect(fill="white"),
                        axis.ticks = element_line(colour=NA),
                        panel.grid = element_line(colour=NA),
                        axis.text.y = element_blank(),
                        axis.text.x = element_blank(),
                        text = element_text(size=16, family="Humor Sans"),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank()
    )
    
    p <- ggplot(data=Paris)
    p <- p + geom_line(  aes(x=V8, y=V9, group = paste(Paris[,"V3"],Paris[,"V5"])),colour="black", fill="white" )
    p<-p+ggtitle(title)
    p<-p + xlab("")
    p<-p+ ylab("")
    p<-p+theme_xkcd
    p <- p + geom_line(data = Departement,  aes(x=V8, y=V9, group = paste(Departement[,"V3"],Departement[,"V5"])),colour="black", fill="white" )
    return(p)
  }
  PlotParis("test")
  
  PlotMap <- function(bool = TRUE,value,title, color){
    load("0 Data Cleaning/Actuaire/Targets.RData")
    load("0 Data Cleaning/Actuaire/VarAdd.RData")
    load("2 Data Visualization/Carte/MapParis.RData")
    
    PlotParis <- function(title){
      theme_xkcd <- theme(panel.grid.major =  element_blank(),
                          panel.grid.minor =  element_blank(),
                          panel.background =  element_blank(),
                          #panel.background = element_rect(fill="white"),
                          axis.ticks = element_line(colour=NA),
                          panel.grid = element_line(colour=NA),
                          axis.text.y = element_blank(),
                          axis.text.x = element_blank(),
                          text = element_text(size=16, family="Humor Sans"),
                          axis.ticks = element_blank(),
                          axis.text.x = element_blank()
      )
      
      p <- ggplot(data=Paris)
      p <- p + geom_line(  aes(x=V8, y=V9, group = paste(Paris[,"V3"],Paris[,"V5"])),colour="black", fill="white" )
      p<-p+ggtitle(title)
      p<-p + xlab("")
      p<-p+ ylab("")
      p<-p+theme_xkcd
      p <- p + geom_line(data = Departement,  aes(x=V8, y=V9, group = paste(Departement[,"V3"],Departement[,"V5"])),colour="black", fill="white" )
      return(p)
    }
    
    p <- PlotParis(title)
    
    
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
  
  for(i in nfreq_X){
    
    png(paste('2 Data Visualization/Plot Espace Creatif/',sprintf(" %6d ",number[i]),'.png',sep = ''),width =1920, height = ceiling(1080*1.51817))
#     par(asp = 1.51817) 
    print(PlotMap(TRUE,freq_X[,i],paste("Moyenne des fréquences de vol sur : ",number[i] ," voisins",sep = ""),rainbow(7))+theme(legend.position="none"))
    dev.off()
  }



  
library(ggplot2)
library(lattice)
load("0 Data Cleaning/Densite Vol/CM_Xe_VOL.RData")
load("2 Data Visualization/Carte/MapParis.RData")



nCM_X <- colnames(CM_Xe_VOL)

number <- c(1,5,10,20,50,70,100,150,200,500,1000,2000)
names(number) <- nCM_X 



for(i in nCM_X){
  
  png(paste('2 Data Visualization/Plot Espace Creatif/CM/',sprintf(" %6d ",number[i]),'.png',sep = ''),width =1920, height = ceiling(1080*1.51817))
  print(PlotMap(TRUE,CM_Xe_VOL[,i],paste("Moyenne des Couts Moyens de vol : ",number[i] ," voisins volés",sep = ""),rainbow(7)))
  dev.off()
}
}

