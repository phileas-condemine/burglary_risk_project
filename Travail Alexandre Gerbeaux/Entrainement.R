setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/")
library(ggplot2)
library(reshape2)
library(corrplot)
library(rpart)
library(cplm)
library(randomForest)
library(stringr)
library(rpart.plot)
load("3 Entrainement/OpenData.RData")
load("0 Data Cleaning/Actuaire/Targets.RData")
#load("0 Data Cleaning/Actuaire/VarAdd.RData")

data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))



test <- function(){
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
tree_cout_moyen <- rpart(CoutMoyenVOL~.,data_cout_moyen,control=rpart.control(cp=0.011),minbucket = 50)


var_supp <- -c(68,69,70,71,72,96)
data_freq <- data.frame(cbind(OpenData[,var_supp], freqVOL))
tree_freq <- rpart(freqVOL~.,data_freq)



heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rev(rainbow(99, end=.36))[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(tree, branch.col=cols, box.col=cols, ...)
}

heat.tree(tree_cout_moyen,type = 4)

split.fun <- function(x, labs, digits, varlen, faclen)
{
  gsub(" = ", ":\n", labs)
}


predict(tree_cout_moyen,data_cout_moyen)

sum(residuals(tree_cout_moyen)^2/length(residuals(tree_cout_moyen)))


ggplot(data_cout_moyen, aes(x = data_cout_moyen$freq_500e_VOL))+ geom_line(stat="density")

ggplot(data_cout_moyen)+
  geom_line(stat="density", aes(x = data_cout_moyen$CM_2000e_VOL))+
  geom_vline(aes(xintercept = 2456), colour="#990000")+
  geom_vline(aes(xintercept = 2444), colour="#990000")


plot(predict(tree_cout_moyen,data_cout_moyen),data_cout_moyen$CoutMoyenVOL)

aggregate(by = list(pred = predict(tree_cout_moyen,data_cout_moyen)),x =list(y = data_cout_moyen$CoutMoyenVOL),mean)
sortie <- aggregate(by = list(pred = predict(tree_cout_moyen,data_cout_moyen)),x =list(y = data_cout_moyen$CoutMoyenVOL),mean)

output <- predict(tree_cout_moyen,data_cout_moyen)
filtre <- output > 14000

mean(data_cout_moyen$CoutMoyenVOL[filtre])
CV <- rbinom(length(CoutMoyenVOL),1,0.8) == TRUE

rF_cout_moyen <- randomForest(x = data_cout_moyen[CV,-108],
             y  = data_cout_moyen[CV,108],
             xtest = data_cout_moyen[!CV,-108],
             ytest  = data_cout_moyen[!CV,108],
             ntree = 200,
             maxnodes = 25,
             mtry = 100
             )
cor(rF_cout_moyen$test$predicted,data_cout_moyen$CoutMoyenVOL[!CV], method = "spearman")
cor(rF_cout_moyen$predicted,data_cout_moyen$CoutMoyenVOL[CV], method = "spearman")

cor(rF_cout_moyen$test$predicted,data_cout_moyen$CoutMoyenVOL[!CV])
cor(rF_cout_moyen$predicted,data_cout_moyen$CoutMoyenVOL[CV])

plot(rF_cout_moyen$test$mse, type ="l")

plot(rF_cout_moyen$predicted,data_cout_moyen$CoutMoyenVOL[CV])
plot(rF_cout_moyen$test$predicted,data_cout_moyen$CoutMoyenVOL[!CV])



}


plot_maxnodes <- function(N){
  plot_m <- list()
  for(i in c(1:N)){
    
    R <- 2000 %/% N
    
    RF_cm <- randomForest(x = data_cout_moyen[CV,-108],
                          y  = data_cout_moyen[CV,108],
                          xtest = data_cout_moyen[!CV,-108],
                          ytest  = data_cout_moyen[!CV,108],
                          ntree = 100,
                          mtry = 30, 
                          maxnodes = R*i)
    
    plot_m[[i]] <- RF_cm$test$mse
    
  }
  
  library(ggplot2)
  library(reshape2)
  #theme = theme_set(theme_bw())
  theme = theme_set(theme_bw())
  
  # original data in a 'wide' format
  x  <- seq(1, 100)
  # y1 <- RF_cm1$test$mse
  # y2 <- RF_cm2$test$mse
  # df <- data.frame(x, y1, y2)
  df <- data.frame(x,plot_m)
  local <- paste("y", c(1:N))
  colnames(df) <- c("x",local)
  
  # melt the data to a long format
  df2 <- melt(data = df, id.vars = "x")
  
  colnames(df2)<-c("x","maxnodes","value")
  # plot, using the aesthetics argument 'colour'
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  
  cols = gg_color_hue(N)
  
  p <- ggplot(data = df2, aes(x = x, y = value, colour = maxnodes)) + geom_line(alpha = 0.6) +
    xlab("number of trees") + 
    ylab("Mean Square Error") +
    ggtitle("MSE on a test set varying with maxnodes")
  p<- p + 
    theme(legend.position="none")
  #scale_colour_manual(name = "maxnodes",labels = R*c(1:N), values = cols)
  return(p)
}


OptimisationParameter <- function(Nn,Nm,smooth){
  
  n_tree <- 50
  vect_n <- c(1:Nn) * (150 %/% Nn)
  vect_m <- c(1:Nm) * ((ncol(OpenData)-80) %/% Nm) 
  matrix_CV <- matrix(NA,nrow(data_cout_moyen),smooth)
  for(i in 1:smooth){
    matrix_CV[,i]<-  rbinom(length(CoutMoyenVOL),1,0.8) == TRUE
    
  }
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
  print("ok RF")
  }
   
  iRF_o <- function(n,m){
  
    output <- matrix(NA,n_tree,smooth)
    for(i in 1:smooth){
      CV <- matrix_CV[,i]
      output[,i] <- RF_o(n,m,CV)
    }
    return(apply(output,1,mean))
    print("ok mean")
    
  }
  
  colnames_combi <- function(v){
    n <- v[1]
    m <- v[2]
    return(paste("(",n,",",m,")", sep = ""))
    
  }
  #return(mapply(iRF_o, combi[,1],combi[,2]))
  output <- mcmapply(iRF_o, combi[,1],combi[,2],mc.cores = 16))
  
  colnames(output) <- apply(combi,1,colnames_combi)
  
  
 save(output, file = "ParametreCM.RData")
    
}



tuneRF(x = data_cout_moyen[,-108],
       y = data_cout_moyen[,108],
       mtryStart = 70,
       ntreeTry = 200)



############Version CLUSTER##############

##Copier Coller ça :


OptimisationParameterCluster <- function(){
setwd("/data/agerbeaux/Entrainement")
load("OpenData.RData")
load("Targets.RData")


library(randomForest)
library(parallel)
library(cvTools)
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
# Nn <- 10
# Nm <- 10
smooth <- 5
n_tree <- 200
# vect_n <- c(1:Nn) * (150 %/% Nn)
# vect_m <- c(1:Nm) * ((ncol(OpenData)) %/% Nm) 

vect_n <- c(2:11)*4
vect_m <- c(37:50)*2
combi <- expand.grid(vect_n,vect_m)


matrix_CV <- matrix(NA,nrow(data_cout_moyen),5)
cvFold <- cvFolds(nrow(data_cout_moyen), type = "random", R=2)
for(i in 1:5){
  matrix_CV[cvFold$subsets[,1],i]<- !(cvFold$which == i)
  
}

fMSE <- function(v1,v2){
  vect <- v1-v2
  vect <- as.matrix(vect)
  return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
}

RF_o <- function(n,m,CV){
  rF_cout_moyen <- randomForest(x = data_cout_moyen[CV,-108],
                                y  = data_cout_moyen[CV,108],
                                ntree = n_tree,
                                maxnodes = n,
                                mtry = m)
  return(fMSE(predict(rF_cout_moyen, newdata = data_cout_moyen[!CV,-108]),data_cout_moyen[!CV,108]))
}
iRF_o <- function(n,m){
  
  output <- matrix(NA,1,smooth)
  for(i in 1:smooth){
    CV <- matrix_CV[,i]  
    output[,i] <- RF_o(n,m,CV)
  }
  return(mean(output))
  
}

colnames_combi <- function(v){
  n <- v[1]
  m <- v[2]
  return(paste("(",n,",",m,")", sep = ""))
  
}
#return(mapply(iRF_o, combi[,1],combi[,2]))
output1 <- mcmapply(iRF_o, combi[,1],combi[,2],mc.cores = 22)

names(output1) <- apply(combi,1,colnames_combi)


save(output1, file = "ParametreCM1.RData")


matrix_CV <- matrix(NA,nrow(data_cout_moyen),5)
for(i in 1:5){
  matrix_CV[cvFold$subsets[,2],i]<- !(cvFold$which == i)
  
}


RF_o <- function(n,m,CV){
  rF_cout_moyen <- randomForest(x = data_cout_moyen[CV,-108],
                                y  = data_cout_moyen[CV,108],
                                ntree = n_tree,
                                maxnodes = n,
                                mtry = m)
  return(rF_cout_moyen$test$mse)
}
iRF_o <- function(n,m){
  
  output <- matrix(NA,1,smooth)
  for(i in 1:smooth){
    CV <- matrix_CV[,i]  
    output[,i] <- RF_o(n,m,CV)
  }
  return(mean(output))  
}

colnames_combi <- function(v){
  n <- v[1]
  m <- v[2]
  return(paste("(",n,",",m,")", sep = ""))
  
}
#return(mapply(iRF_o, combi[,1],combi[,2]))
output2 <- mcmapply(iRF_o, combi[,1],combi[,2],mc.cores = 18)


names(output2) <- apply(combi,1,colnames_combi)


save(output2, file = "ParametreCM2.RData")







setwd("/data/agerbeaux/Entrainement")
load("OpenData.RData")
load("Targets.RData")


library(randomForest)
library(parallel)
library(cvTools)
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
Nn <- 10
Nm <- 10
smooth <- 5
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



###output du Cluster

outputCluster <- function(){
  
  
  
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
#scale_colour_manual(values=gg_color_hue(4))







#Analyse fréquentielle :


FrequencyAnalysis <- function(){


data_freq <- data.frame(cbind(OpenData,freqVOL))
tree_freq <- rpart(freqVOL~.,data_freq,
                   control=rpart.control(minbucket=15000, cp=0.00001))
heat.tree(tree_freq)

heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(tree, branch.col=rev(cols), box.col=rev(cols), ...)
}
}








##########Entrainement d'un modèle en cross validation
load("filtre")







##########Creation des features

##Creation du filtre 3 fold pour la creation de features

####Creation du filtre
# set.seed(23)
# cvFold <- cvFolds(length(freqVOL), K = 3, type = "random", R=1)
# 
# filtre_feat <- matrix(NA,length(freqVOL),3)
# for(i in 1:3){
#   filtre_feat[cvFold$subsets,i]<- !(cvFold$which == i)
#   
# }




#######Création des features pour la fréquence

FrequencyCreationCluster <- function(){
library(parallel)
setwd("/data/agerbeaux/Entrainement")

load("VarAdd.RData")
load("Targets.RData")
load("filtre_features.RData")


Coord_LII0 <- Coord_LII
freqVOL0 <- freqVOL
for(i in 1:3){

  filtre_local <- filtre_feat[,i]
  Coord_LII <- Coord_LII0[filtre_local,]
  freqVOL <- freqVOL0[filtre_local]
  
table <- as.data.frame(cbind(Coord_LII,freqVOL))
table_lapply <- as.data.frame(rbind(t(as.data.frame(Coord_LII)), id = 1:(nrow(Coord_LII))))

fV1<- c(1,5,10,100,200,500,700,1000,
        2000,3000,5000,6000,7000,8000,9000,10000,
        12000,15000,17000,20000,25000,30000,40000,50000,70000,100000,
        150000,190000)


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
save(freq_X, file = paste("freq_X",i,".RData", sep = ""))

}

}
###########Création des features pour le Cout Moyen

SeverityCreationCluster <- function(){

setwd("/data/agerbeaux/Entrainement")

load("VarAdd.RData")
load("Targets.RData")
load("filtre_features.RData")
load("Targets.RData")
load("distance_client_vol.RData")
filtre_VOL <- freqVOL > 0

filtre_feat_CM <- matrix(NA,length(CoutMoyenVOL),3)
for(i in 1:3){
  filtre_feat_CM[,i] <- filtre_feat[,i][filtre_VOL]
  
}

save(filtre_feat_CM, file = "filtre_feat_CM.RData")


setwd("/data/agerbeaux/Entrainement")
load("VarAdd.RData")
load("Targets.RData")
load("distance_client_vol.RData")
load("filtre_features.RData")
load("filtre_feat_CM.RData")

library(parallel)
filtre_VOL <- freqVOL > 0
table <- data.frame(t(distance_client_vol))
for(i in 1:3){
table_local<-table[filtre_feat_CM[,i],filtre_feat[,i]]
CoutMoyenVOL_local <- CoutMoyenVOL[filtre_feat_CM[,i]]
filtre_VOL_local <- freqVOL[filtre_feat[,i]] > 0
X_gui_CM <- c(1,2,5,10,20,30,50,60,70,80,90,100,120,150,170,200,250,300,400,500,700,1000,1500)

CM_Xe_VOL_vect<-function(vect){

CM_Xe_VOL <- function(x){
    local <- rank(vect, ties.method = c("first"))
    filtre <- local < (x+1)
    return(mean(CoutMoyenVOL_local[filtre]))
  }
  return(sapply(X_gui_CM,CM_Xe_VOL))
}

#CM_X <- mcmapply(CM_Xe_VOL_vect, table_local ,mc.cores = 22)

CM_X <- mapply(CM_Xe_VOL_vect, table_local)

table_local_adj<-table_local[,filtre_VOL_local]


CM_Xe_VOL_vect_adj<-function(vect){
  
  CM_Xe_VOL_adj <- function(x){
    local <- rank(vect, ties.method = c("first"))
    filtre <- (local < (x+2))&(local > 1)
    return(mean(CoutMoyenVOL_local[filtre]))
  }
  return(sapply(X_gui_CM,CM_Xe_VOL_adj))
}

#CM_X_adj <- mcmapply(CM_Xe_VOL_vect_adj, table_local_adj ,mc.cores = 22)
CM_X_adj <- mapply(CM_Xe_VOL_vect_adj, table_local_adj)



CM_X[,filtre_VOL_local]<-CM_X_adj

CM_X <- t(CM_X)

colnames(CM_X) <- X_gui_CM

save(CM_X, file = paste("CM_X",i,".RData", sep = ""))

}






}


}



StabilityResultCluster <- function(){
##Stabilité des résultats en KFOLD

fMSE <- function(v1,v2){
  
  (sum((v1-v2)^2)/length(v1))
  
}




library(randomForest)
library(parallel)
library(cvTools)
load("3 Entrainement/OpenData.RData")
load("0 Data Cleaning/Actuaire/Targets.RData")
#load("0 Data Cleaning/Actuaire/VarAdd.RData")
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))

#set.seed(24)
cvFold <- cvFolds(nrow(data_cout_moyen), type = "random", R=2)

matrix_CV_1 <- matrix(NA,nrow(data_cout_moyen),5)
for(i in 1:5){
  matrix_CV_1[cvFold$subsets[,1],i]<- !(cvFold$which == i)
  
}

matrix_CV_2 <- matrix(NA,nrow(data_cout_moyen),5)
for(i in 1:5){
  matrix_CV_2[cvFold$subsets[,2],i]<- !(cvFold$which == i)
  
}



output_RF <- function(CV){
  rF_cout_moyen <- randomForest(x = data_cout_moyen[CV,-108],
                                y  = data_cout_moyen[CV,108],
                                xtest = data_cout_moyen[!CV,-108],
                                ytest  = data_cout_moyen[!CV,108],
                                ntree = 200,
                                maxnodes = 32,
                                mtry = 96
  )
  

  output <- matrix(NA,6,1)
  output[1] <- cor(rF_cout_moyen$test$predicted,data_cout_moyen$CoutMoyenVOL[!CV], method = "spearman")
  output[2] <- cor(rF_cout_moyen$predicted,data_cout_moyen$CoutMoyenVOL[CV], method = "spearman")
  
  output[3] <- cor(rF_cout_moyen$test$predicted,data_cout_moyen$CoutMoyenVOL[!CV])
  output[4] <- cor(rF_cout_moyen$predicted,data_cout_moyen$CoutMoyenVOL[CV])
  
  output[5] <- rF_cout_moyen$test$mse[200]
  output[6] <- rF_cout_moyen$mse[200]
  
  return(output)
}

check1 <- apply(matrix_CV_1,2,output_RF)
check2 <- apply(matrix_CV_2,2,output_RF)


}



####

RF_interleaved <- function(){
  
  
  data_cout_moyen2 <- data_cout_moyen[order(data_cout_moyen[,108]),]
  
  data_cout_moyen2[,108] <- rank(data_cout_moyen2[,108])
  
  
  ##Stabilité des résultats en KFOLD
  
  fMSE <- function(v1,v2){
    
    (sum((v1-v2)^2)/length(v1))
    
  }
  
  
  
  
  library(randomForest)
  library(parallel)
  library(cvTools)
  load("3 Entrainement/OpenData.RData")
  load("0 Data Cleaning/Actuaire/Targets.RData")
  #load("0 Data Cleaning/Actuaire/VarAdd.RData")
  
  #set.seed(24)
  cvFold <- cvFolds(nrow(data_cout_moyen2), K = 5,type = "interleaved")
  
  matrix_CV_1 <- matrix(NA,nrow(data_cout_moyen2),5)
  for(i in 1:5){
    matrix_CV_1[,i]<- !(cvFold$which == i)
    
  }
  
  
  matrix_CV_2 <- matrix(NA,nrow(data_cout_moyen2),5)
  for(i in 1:5){
    matrix_CV_2[cvFold$subsets[,2],i]<- !(cvFold$which == i)
    
  }
  
  
  
  
  
  
  
  
  
  output_RF <- function(CV){
    rF_cout_moyen <- randomForest(x = data_cout_moyen2[CV,-108],
                                  y  = data_cout_moyen2[CV,108],
                                  xtest = data_cout_moyen2[!CV,-108],
                                  ytest  = data_cout_moyen2[!CV,108],
                                  ntree = 200,
                                  maxnodes = 32,
                                  mtry = 96
    )
    
    
    
    
    output <- matrix(NA,6,1)
    output[1] <- cor(rF_cout_moyen$test$predicted,data_cout_moyen2$CoutMoyenVOL[!CV], method = "spearman")
    output[2] <- cor(rF_cout_moyen$predicted,data_cout_moyen2$CoutMoyenVOL[CV], method = "spearman")
    
    output[3] <- cor(rF_cout_moyen$test$predicted,data_cout_moyen2$CoutMoyenVOL[!CV])
    output[4] <- cor(rF_cout_moyen$predicted,data_cout_moyen2$CoutMoyenVOL[CV])
    
    output[5] <- rF_cout_moyen$test$mse[200]
    output[6] <- rF_cout_moyen$mse[200]
    
    return(output)
  }
  
  check1 <- apply(matrix_CV_1,2,output_RF)
  
}


ChargementDataTroisFold <- function(){
  load("3 Entrainement/OpenData.RData")
  load("3 Entrainement/filtre_features.RData")
  
  
  ChargementDataUnFold <- function(i){
  filtre_local <- filtre_feat[,i]
  load(paste("3 Entrainement/NewFeatures/freq_X",i,".RData",sep = ""))
  load(paste("3 Entrainement/NewFeatures/CM_X",i,".RData",sep = ""))
  freq_X <- t(freq_X)
  number_freq<- c(1,5,10,100,200,500,700,1000,
             2000,3000,5000,6000,7000,8000,9000,10000,
             12000,15000,17000,20000,25000,30000,40000,50000,70000,100000,
             150000,200000)
  
  nfreq_X <- paste(rep("freq_",28), number_freq,rep("e",28), sep = "")
  colnames(freq_X) <- nfreq_X

  freq_X <- t(freq_X)
  number_CM <- c(1,2,5,10,20,30,50,60,70,80,90,100,120,150,170,200,250,300,400,500,700,1000,1500)
  nCM_X <- paste(rep("CM_",24), number_CM,rep("e_VOL",24), sep = "")
  colnames(CM_X) <- nCM_X
  
  OpenData_1 <- data.frame(cbind(OpenData[filtre_local,1:67],freq_X,CM_X))
  return(data.frame(cbind(OpenData[filtre_local,1:67],freq_X,CM_X)))
  }
  
  OpenData_1 <- ChargementDataUnFold(1)
  save(OpenData_1, file = "3 Entrainement/NewFreatures/OpenData_1.RData")
  OpenData_2 <- ChargementDataUnFold(2)
  save(OpenData_1, file = "3 Entrainement/NewFreatures/OpenData_2.RData")
  OpenData_3 <- ChargementDataUnFold(3)
  save(OpenData_1, file = "3 Entrainement/NewFreatures/OpenData_3.RData")
  
  
  
  
  
}


###Analyse des Résultats






library(randomForest)
library(parallel)
library(cvTools)
load("3 Entrainement/OpenData.RData")
load("0 Data Cleaning/Actuaire/Targets.RData")
#load("0 Data Cleaning/Actuaire/VarAdd.RData")




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





setwd("/data/agerbeaux/Entrainement")
library(randomForest)
library(parallel)
load("Targets.RData")
load("OpenDataAct.RData")
load("filtre_metriques2.RData")
data_cout_moyen <- data.frame(cbind(OpenDataAct[freqVOL >0,], CoutMoyenVOL))
data_freq <- data.frame(cbind(OpenDataAct,freqVOL))
data_prime_pure <- data.frame(cbind(OpenDataAct,SinistreVOL))




Targ <- 107

output_RF <- function(X){
  CV1<- X$CV1
  CV2<- X$CV2
  rF_cout_moyen <- randomForest(x = data_cout_moyen[CV1,-Targ],
                                y  = data_cout_moyen[CV1,Targ],
                                ntree = 200,
                                maxnodes = 32,
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



output_ODAct<- mcmapply(output_RF,list_matrix , mc.cores= 22)
save(output_ODAct, file = "output_metrique_ODActbis2.RData")



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
  rF_cout_moyen2 <- randomForest(x = data_cout_moyen[CV1,-Targ],
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


output_OD<- mcmapply(output_RF,list_matrix , mc.cores= 22)
save(output_OD, file = "output_metrique_102freq.RData")






setwd("/data/agerbeaux/Entrainement")
library(randomForest)
library(parallel)
load("Targets.RData")
load("OpenData.RData")
load("filtre_metriques2.RData")
###Chargement des Data
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
data_freq <- data.frame(cbind(OpenData,freqVOL))
data_prime_pure <- data.frame(cbind(OpenData,SinistreVOL))

output_REF <-function(X){
  CV1<- X$CV1
  CV2<- X$CV2
fMSE <- function(v1,v2){
  vect <- v1-v2
  vect <- as.matrix(vect)
  return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
}

output_metrique_ref <- function(data,CV){
  output <- matrix(NA,2,1)
  y.true.t <- data[!CV,108]
  y.true.e <- data[CV,108]
  output[1] <- fMSE(mean(y.true.t),y.true.t)
  output[2] <- fMSE(mean(y.true.e),y.true.e)
  rownames(output) <- c("MSE test","MSE train")
  return(output)
}


output_ref <- rbind(output_metrique_ref(data_cout_moyen, CV1),
                    output_metrique_ref(data_freq, CV2),
                    output_metrique_ref(data_prime_pure, CV2))

return(output_ref)


}

output_ref<- mcmapply(output_REF,list_matrix , mc.cores= 22)


test <- function(X){
  
  return(X$v1+X$v2)
  
}

V1 <- cbind(matrix(1,3,1),matrix(5,3,1))
V2 <- cbind(matrix(4,3,1),matrix(2,3,1))

list_m <- list()
for(i in 1:2){
  list_m[[i]]<-list(v1 = V1[,i],v2 = V2[,i])
  
}
rf <- foreach(ntree = rep(10,4), .combine = combine) %do% 
  +  randomForest(x = data_cout_moyen[,-108],
                  y  = data_cout_moyen[,108],
                  ntree = ntree,
                  maxnodes = 32,
                  mtry = 92)
  
rf <- foreach(ntree=rep(10,4), .combine=combine, .multicombine=TRUE,
              .packages='randomForest') %dopar% {
                randomForest(x , y , ntree=ntree)
              }

rf <- foreach(ntree=rep(10,20), .combine=combine, .packages='randomForest') %dopar%
  + randomForest(x, y, ntree=ntree)



####Initilisation des Split
set.seed(25)
cvFold <- cvFolds(nrow(data_freq), type = "random", R=200)
matrix_CV <- matrix(NA,nrow(data_freq),1)
matrix_CV_local <- matrix(NA,nrow(data_freq),5)
for(j in 1:200){
  for(i in 1:5){
    matrix_CV_local[cvFold$subsets[,j],i]<- !(cvFold$which == i)
    print(j)
  }
  matrix_CV <- cbind(matrix_CV,matrix_CV_local)
}
matrix_CV <- matrix_CV[,2:1001]


filtre_VOL <- freqVOL > 0
matrix_CV_VOL <- matrix(NA,nrow(data_cout_moyen),1000)
for (i in 1:1000){
  matrix_CV_VOL[,i] <-  matrix_CV[,i][filtre_VOL]
  print(i)
}
##########################

length(apply(matrix_CV_VOL,2,table)[1,][(apply(matrix_CV_VOL,2,table)[1,] %in% c(559,560,561))])








list_matrix <- list()
for (i in 1:50){
  list_matrix[[i]] <- list(CV1 = matrix_CV_VOL[,i], CV2 = matrix_CV[,i])
  
}


filtre_VOL <- freqVOL > 0

set.seed(25)
cvFold <- cvFolds(nrow(data_cout_moyen), type = "random", R=10)
matrix_CV_VOL <- matrix(NA,nrow(data_cout_moyen),1)
matrix_CV_local <- matrix(NA,nrow(data_cout_moyen),5)
for(j in 1:10){
  for(i in 1:5){
    matrix_CV_local[cvFold$subsets[,j],i]<- !(cvFold$which == i)
  }
  matrix_CV_VOL <- cbind(matrix_CV_VOL,matrix_CV_local)
}
matrix_CV_VOL <- matrix_CV_VOL[,2:51]


cvFold2 <- cvFolds((nrow(data_freq)-nrow(data_cout_moyen)), type = "random", R=10)
matrix_CV_nVOL <- matrix(NA,(nrow(data_freq)-nrow(data_cout_moyen)),1)
matrix_CV_local <- matrix(NA,(nrow(data_freq)-nrow(data_cout_moyen)),5)
for(j in 1:10){
  for(i in 1:5){
    matrix_CV_local[cvFold2$subsets[,j],i]<- !(cvFold2$which == i)
  }
  matrix_CV_nVOL <- cbind(matrix_CV_nVOL,matrix_CV_local)
}
matrix_CV_nVOL <- matrix_CV_nVOL[,2:51]

matrix_CV <- matrix(NA,nrow(data_freq),50)

for(i in 1:50){
  matrix_CV[!filtre_VOL,i] <- matrix_CV_nVOL[,i]
  matrix_CV[filtre_VOL,i] <- matrix_CV_VOL[,i]
  
}




library("doMC")
library("randomForest")
data(iris)

registerDoMC(20) #number of cores on the machine
darkAndScaryForest <- foreach(y=seq(40), .combine=combine ) %dopar% {
  set.seed(y) # not really needed
  rf <- randomForest(Species ~ ., iris, ntree=1000, norm.votes=FALSE)
}



library(randomForest)
data(iris)
mydata = iris


###############################################################################
### sequential
###############################################################################
randomForest(Species ~ ., data = mydata, ntree = 1000)


###############################################################################
### parallel with snowfall
###############################################################################
library(snowfall)

# inititalize the cluster with 2 CPUs, export libraries and data and setup the
# RNG
sfInit(parallel = TRUE, cpus = 2)
sfLibrary(randomForest)
sfExport("mydata")
sfClusterSetupRNG(seed = 1)

# fit a forest on exported "mydata" with n trees
fit = function(n) {
  randomForest(Species ~ ., data = mydata, ntree = n)
}

# fit 10 forests with 100 trees each
res = sfLapply(rep(10000, 10), fit)

# stop the cluster
sfStop()

# reduce results with randomForest::combine to a forest with 1000 trees
Reduce(combine, res)

###############################################################################
### parallel with mclapply
###############################################################################
library(randomForest)
data(iris)
mydata = iris
library(parallel)
fit = function(n) {
  randomForest(Species ~ ., data = mydata, ntree = n)
}
res = mclapply(rep(50000, 20), fit, mc.cores = 20)
Reduce(combine, res)


OptimisationParameterFreq <- function(){
  library(randomForest)
  library(parallel)
  setwd("/data/agerbeaux/Entrainement")
  load("Targets.RData")
  load("OpenData.RData")
  load("filtre_metriques2.RData")
  
  
  fMSE <- function(v1,v2){
    vect <- v1-v2
    vect <- as.matrix(vect)
    return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
  }
  
  output_metrique <- function(RF,data,CV){
    output <- matrix(NA,6,1)
    y.fit.t <- predict(RF, newdata = data[!CV,-108])
    y.fit.e <- predict(RF, newdata = data[CV,-108])
    y.true.t <- data[!CV,108]
    y.true.e <- data[CV,108]
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
  
  
  ###Chargement des Data
  data_freq <- data.frame(cbind(OpenData,freqVOL))
  output_freq <- matrix(NA,6,5)
  for(i in 1:5){
  CV <- matrix_CV[,i]
  
  fit = function(n){
  randomForest(x = data_freq[CV,-108],
                          y  = data_freq[CV,108],
                          ntree = n,
                          maxnodes = 16,
                          mtry = 92)
  }
  res = mclapply(rep(2,20),fit, mc.cores = 20)
  rF_freq <- Reduce(combine,res)
  
  output_freq[,i] <- output_metrique(rF_freq,data_freq,CV)
  
  
  }
  output_total <- apply(output_freq,1,mean)
  
}

Visualisation_Output <- function(){
  load("3 Entrainement/VisualisationMetriques/output_metrique.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_ref.RData")
  
  data_preparation <- output[c(1,2),]
  
  
  ############Comparison of Models for total Claim on training set
  data1 <- melt(t(output[c(1,2),]))
  data1 <- data1[,-1]
  colnames(data1) <- c("Models","values")
  Models <- rep(c("Test","Train"), each = 50)
  data1[,1] <- Models 
  
  
  
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
  
  fig1 <- box_viz("Severity Correlation","Correlation",data1)
  
  figure_list <- list()
  Models <- rep(c("Test","Train"), each = 50)
  
  title <- paste(rep(c("Severity","Frequency","Prime Pure"), each = 3),rep(c("Correlation","Correlation Spearman","MSE"),times = 3))
  metrique <- rep(c("Correlation","Correlation Spearman","MSE"),times = 3)
  for(i in 1:9){
    data <- melt(t(output[c(2*i-1,2*i),]))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list[[i]] <- box_viz(title[i],metrique[i],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
      #stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
  }
  figure_list_norm <- list()
  title <- c("Severity MSE normalisé","Frequency MSE normalisé","Prime Pure MSE normalisé")
  metrique <- c("MSE","MSE","MSE")
  for(i in 1:3){
    data <- melt(t(output[c(6*i-1,6*i),]/(output_ref[c(2*i-1,2*i),])))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_norm[[i]] <- box_viz(title[i],metrique[i],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
    
  }
  
  
  
}


###7 Enquête sur les MSE :

setwd("/data/agerbeaux/Entrainement")
library(randomForest)
library(parallel)
load("Targets.RData")
load("OpenData.RData")
load("filtre_metriques2.RData")
load("DataComparaison.RData")
data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
data_freq <- data.frame(cbind(OpenData,freqVOL))

X <- list_matrix[[7]]
CV1<- X$CV1
CV2<- X$CV2
rF_cout_moyen <- randomForest(x = data_cout_moyen[CV1,-108],
                              y  = data_cout_moyen[CV1,108],
                              ntree = 200,
                              maxnodes = 32,
                              mtry = 92
)


fit = function(n){
  randomForest(x = data_freq[CV2,-108],
               y  = data_freq[CV2,108],
               ntree = n,
               maxnodes = 16,
               mtry = 92)
}

res = mclapply(rep(2,25),fit, mc.cores = 20)
rF_freq <- Reduce(combine,res)

rF_cout_moyen_act <- randomForest(x = data_cm_RF[CV1,-6],
                              y  = data_cm_RF[CV1,6],
                              ntree = 50,
                              maxnodes = 200,
                              mtry = 3
)

fit = function(n){
  randomForest(x = data_freq_RF[CV2,-6],
               y  = data_freq_RF[CV2,6],
               ntree = n,
               maxnodes = 100,
               mtry = 3)
}

res = mclapply(rep(2,25),fit, mc.cores = 20)
rF_freq_act <- Reduce(combine,res)




save(rF_cout_moyen,rF_freq,rF_cout_moyen_act,rF_freq_act)



fMSE <- function(v1,v2){
  vect <- v1-v2
  vect <- as.matrix(vect)
  return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
}


###test données actuarielles

setwd("/data/agerbeaux/Entrainement")
library(randomForest)
library(parallel)
load("Targets.RData")
load("OpenData.RData")
load("filtre_metriques2.RData")
load("DataComparaison.RData")
data_sinistre_RF <- data_sinistre[,c(2,3,4,5,6,8)]

output_RF <- function(X){
  CV1<- X$CV1
  CV2<- X$CV2
  rF_cout_moyen <- randomForest(x = data_cm_RF[CV1,-6],
                                y  = data_cm_RF[CV1,6],
                                ntree = 50,
                                maxnodes = 200,
                                mtry = 3
  )
  
  rF_freq <- randomForest(x = data_freq_RF[CV2,-6],
                          y  = data_freq_RF[CV2,6],
                          ntree = 50,
                          maxnodes = 100,
                          mtry = 3
  )
  
  
  output <- matrix(NA,18,1)
  
  
  fMSE <- function(v1,v2){
    vect <- v1-v2
    vect <- as.matrix(vect)
    return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
  }
  
  output_metrique <- function(RF,data,CV){
    output <- matrix(NA,6,1)
    y.fit.t <- predict(RF, newdata = data[!CV,-6])
    y.fit.e <- predict(RF, newdata = data[CV,-6])
    y.true.t <- data[!CV,6]
    y.true.e <- data[CV,6]
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
    y.fit.t <- predict(RF1, newdata = data[!CV,-6]) * predict(RF2, newdata = data[!CV,-6]) 
    y.fit.e <- predict(RF1, newdata = data[CV,-6]) * predict(RF2, newdata = data[CV,-6]) 
    y.true.t <- data[!CV,6]
    y.true.e <- data[CV,6]
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
  
  
  
  
  output <- rbind(output_metrique(rF_cout_moyen,data_cm_RF, CV1),
                  output_metrique(rF_freq,data_freq_RF, CV2),
                  output_metrique_agg(rF_cout_moyen,rF_freq,data_sinistre_RF,CV2))
  
  
  return(output)
}





output_act<- mcmapply(output_RF,list_matrix , mc.cores= 22)
save(output_act, file = "output_metrique_act.RData")


Visualisation2_Output <- function(){
  
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

Visualisation3_Output <- function(){
  
  load("3 Entrainement/VisualisationMetriques/output_metrique_102bis.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_ODAct.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_ref.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_act.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_act_zonier.RData")
  
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
  
  
  figure_list_ODAct <- list()
  Models <- rep(c("OpenData","Actuary","Mix"), each = 50)
  
  title <- paste(rep(c("Severity","Frequency","Prime Pure"), each = 6),rep(c("Correlation Test","Correlation Train","Correlation Spearman Test","Correlation Spearman Train","MSE Test","MSE Train"),times = 3))
  metrique <- rep(c("Correlation","Correlation Spearman","MSE"),times = 3,each = 2)
  #for(i in 1:18){
  for(i in 1:6){
    data <- melt((cbind(output[i,],output_act_zonier[i,],output_ODAct[i,])))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_ODAct[[i]] <- box_viz(title[i],metrique[i],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+  
      scale_x_discrete(limits=(c("Actuary","OpenData","Mix")))
      #scale_fill_manual(values = color_axa2, guide=FALSE)
    
  }
  figure_list_norm_ODAct <- list()
  title <- c("Severity MSE test normalisé","Severity MSE train normalisé",
             "Frequency MSE test normalisé","Frequency MSE train normalisé",
             "Prime Pure MSE test normalisé", "Prime Pure MSE train normalisé")
  metrique <- c("MSE","MSE","MSE","MSE","MSE","MSE")
  Models <- rep(c("OpenData","Actuary","Mix"), each = 50)
  
  #ci <- c(5,6,11,12,17,18)
  ci <- c(5,6)
  j<-1
  for(i in ci){
    data <- melt((cbind(output[i,]/(output_ref[j,]),output_act_zonier[i,]/(output_ref[j,]),output_ODAct[i,]/(output_ref[j,]))))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_norm_ODAct[[j]] <- box_viz(title[j],metrique[j],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
    j<-j+1
  }
  
  
  
}

Visualisation2_Output_Zonier <- function(){
  
  load("3 Entrainement/VisualisationMetriques/output_metrique_102bis.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_ref.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_act.RData")
  load("3 Entrainement/VisualisationMetriques/output_metrique_act_zonier.RData")
  
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
  for(i in 1:6){
    data <- melt((cbind(output[i,],output_act_zonier[i,])))
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
  
  ci <- c(5,6)
  j<-1
  for(i in ci){
    data <- melt((cbind(output[i,]/(output_ref[j,]),output_act_zonier[i,]/(output_ref[j,]))))
    data <- data[,-1]
    colnames(data) <- c("Models","values")
    data[,1] <- Models 
    
    figure_list_norm_act[[j]] <- box_viz(title[j],metrique[j],data)+
      stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    
    j<-j+1
  }
  
  
  
}


SaveBoxplot <- function(){
  
  w <- 771
  h <- 721
  
  for(i in 1:9){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenData',i,'.png',sep = ''),width =w, height = h)
    print(figure_list[[i]])
    dev.off()
  }

  
  for(i in 1:3){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenDataNorm',i,'.png',sep = ''),width =w, height = h)
    print(figure_list_norm[[i]])
    dev.off()
  }
  
  
  for(i in 1:18){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenDataActuary0509',i,'.png',sep = ''),width =w, height = h)
    print(figure_list_act[[i]])
    dev.off()
  }
  
  for(i in 1:6){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenDataActuaryNorm0509',i,'.png',sep = ''),width =w, height = h)
    print(figure_list_norm_act[[i]])
    dev.off()
  }
  
  for(i in 1:18){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenDataActuaryODAct',i,'.png',sep = ''),width =w, height = h)
    print(figure_list_ODAct[[i]])
    dev.off()
  }
  
  for(i in 1:6){
    
    png(paste('3 Entrainement/VisualisationMetriques/OpenDataActuaryNormODAct',i,'.png',sep = ''),width =w, height = h)
    print(figure_list_norm_ODAct[[i]])
    dev.off()
  }
}

Investigation <- function(){
  s
  fMSE <- function(v1,v2){
    vect <- v1-v2
    vect <- as.matrix(vect)
    return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
  }
  
  library(randomForest)
  library(parallel)
  load("0 Data Cleaning/Actuaire/Targets.RData")
  load("3 Entrainement/OpenData.RData")
  load("3 Entrainement/filtre_metriques2.RData")
  load("4 Comparaison de Modeles/DataComparaison.RData")
  data_cout_moyen <- data.frame(cbind(OpenData[freqVOL >0,], CoutMoyenVOL))
  data_freq <- data.frame(cbind(OpenData,freqVOL))
  data_prime_pure <- data.frame(cbind(OpenData,SinistreVOL))

y.fit <- predict(rF_cout_moyen, newdata = data_cout_moyen[!CV1,-108])
y.true <- data_cout_moyen[!CV1,108]

y.fit <- predict(rF_freq, newdata = data_freq[!CV2,-108])
y.true <- data_freq[!CV2,108]

y.fit <- predict(rF_cout_moyen_act, newdata = data_cm_RF[!CV1,-6])
y.true <- data_cm_RF[!CV1,6]

y.fit <- predict(rF_freq_act, newdata = data_freq_RF[!CV2,-6])
y.true <- data_freq_RF[!CV2,6]


y.fit <- predict(rF_cout_moyen, newdata = data_freq[!CV2,-108])

y.fit <- predict(rF_cout_moyen_act, newdata = data_freq_RF[!CV2,-6])


##agre
y.fit <- predict(rF_cout_moyen, newdata = data_freq[!CV2,-108]) * predict(rF_freq, newdata = data_freq[!CV2,-108])
y.fit <- predict(rF_cout_moyen_act, newdata = data_freq_RF[!CV2,-6]) * predict(rF_freq_act, newdata = data_freq_RF[!CV2,-6])
y.true <- data_prime_pure[!CV2,108]
  
plot(y.fit,
     y.true)
plot(rank(y.fit),rank(y.true))
plot(y.true,y.fit-y.true)
cor(y.fit,
    y.true)
cor(y.fit,
    y.true, method = "spearman")
fMSE(y.fit,y.true)

#Remarque sur une métrique :
y.fit.o <- y.fit[order(y.fit)]

sum(y.fit.o[(length(y.fit.o)-10000):length(y.fit.o)])/sum(y.fit.o[1:10000])

}


OptimisationFrequence <- function(){
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



CM_Act_zonier <- function(){
  
  load("0 Data Cleaning/Actuaire/Targets.RData")
  load("4 Comparaison de Modeles/DataComparaison.RData")
  load("3 Entrainement/filtre_metriques2.RData")
  load("0 Data Cleaning/Actuaire/Zonier.RData")
  library(randomForest)
  library(parallel)
 Zonier <- Zonier[freqVOL > 0 ]
  data_cm_RF <- cbind(data_cm_RF[,-6], Zonier,data_cm_RF[,6])
  
  output_RF <- function(X){
    CV1<- X$CV1
  
    rF_cout_moyen <- randomForest(x = data_cm_RF[CV1,-7],
                                  y  = data_cm_RF[CV1,7],
                                  ntree = 50,
                                  maxnodes = 200,
                                  mtry = 3
    )
    
#     rF_freq <- randomForest(x = data_freq_RF[CV2,-6],
#                             y  = data_freq_RF[CV2,6],
#                             ntree = 50,
#                             maxnodes = 100,
#                             mtry = 3
#     )
    
    
    output <- matrix(NA,6,1)
    
    
    fMSE <- function(v1,v2){
      vect <- v1-v2
      vect <- as.matrix(vect)
      return(sum(sapply(vect,function(x)(x^2)))/(length(vect)))
    }
    
    output_metrique <- function(RF,data,CV){
      output <- matrix(NA,6,1)
      y.fit.t <- predict(RF, newdata = data[!CV,-7])
      y.fit.e <- predict(RF, newdata = data[CV,-7])
      y.true.t <- data[!CV,7]
      y.true.e <- data[CV,7]
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
      y.fit.t <- predict(RF1, newdata = data[!CV,-6]) * predict(RF2, newdata = data[!CV,-6]) 
      y.fit.e <- predict(RF1, newdata = data[CV,-6]) * predict(RF2, newdata = data[CV,-6]) 
      y.true.t <- data[!CV,6]
      y.true.e <- data[CV,6]
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
    
    
    
    
    output <- rbind(output_metrique(rF_cout_moyen,data_cm_RF, CV1))
    
    
    return(output)
  }
  
output_act_zonier<- mapply(output_RF,list_matrix)
save(output_act_zonier, file = "output_metrique_act_zonier.RData")

  


}




