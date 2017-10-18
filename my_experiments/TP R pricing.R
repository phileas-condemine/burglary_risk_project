library(data.table)
library(tree)
library(BiocGenerics)
library(gdata)
library(ggdendro)
library(mgcv)
library(ggplot2)
library(MASS)
library(gbm)
library(caret)
library(ggvis)
library(randomForestSRC)
#C5.0 t creates a Boosting ensemble of C5.0 decision trees and rule models (function
#C5.0 in the hononymous package), with and without winnow (feature selection),
#tuning the number of boosting trials in f1, 10, 20g
# library(LibSVM)
# MultiBoostAB LibSVM w uses LibSVM base classiers with the optimal C and
# Gaussian kernel spread selected by the svm C classier (see classier #48). We included
# it for comparison with previous papers (Vanschoren et al., 2012), although a
# strong classier as LibSVM is in principle not recommended to use as base classifier.
library(randomForest)
# MultiBoostAB_RandomForest_w combines RandomForest base classiers. We
# tried this classier for comparison with previous papers (Vanschoren et al., 2012),
# despite of RandomForest is itself an ensemble, so it seems not very useful to learn a
# MultiBoostAB ensemble of RandomForest ensembles.



#********************DATA preparation for training models************************
loadtrain=function(){
data=fread("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres2013.csv")
# Construction de la DB des observations passées
datapast=fread("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres2012.csv")
year="2012"
setnames(datapast,which(grepl(year,colnames(datapast))),gsub(year,"",colnames(datapast))[grepl(year,colnames(datapast))])
for (year in c("2011","2010","2009","2008")){
temp=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres",year,".csv",sep=""))
setnames(temp,which(grepl(year,colnames(temp))),gsub(year,"",colnames(temp))[grepl(year,colnames(temp))])
datapast=rbind(datapast,temp)
}
datapast$NMCNT=as.numeric(datapast$NMCNT)

t = subset(datapast, datapast$chargeec > 0)
datapast2 = t[, list(nbsinBDGpast = sum(nbsinBDG, na.rm=T),
                     nbsinVOLpast = sum(nbsinVOL, na.rm=T),
                     nbsinCLIMpast = sum(nbsinCLIM, na.rm=T),
                     nbsinRCpast = sum(nbsinRC, na.rm=T),
                     nbsinAUTRESpast = sum(nbsinAUTRES, na.rm=T),
                     nbsinCATNATpast = sum(nbsinCATNAT, na.rm=T),
                     nbsinDDEpast = sum(nbsinDDE, na.rm=T), 
                     nbsinINCpast = sum(nbsinINC, na.rm=T), 
                     nbsinpast=sum(nbsin,na.rm=T),
                     chargeecBDGpast = sum(chargeecBDG, na.rm=T), 
                     chargeecVOLpast = sum(chargeecVOL, na.rm=T), 
                     chargeecCLIMpast = sum(chargeecCLIM, na.rm=T), 
                     chargeecRCpast = sum(chargeecRC, na.rm=T), 
                     chargeecAUTRESpast = sum(chargeecAUTRES, na.rm=T), 
                     chargeecCATNATpast = sum(chargeecCATNAT, na.rm=T), 
                     chargeecpast = sum(chargeec, na.rm=T), 
                     chargeecDDEpast = sum(chargeecDDE, na.rm=T), 
                     chargeecINCpast = sum(chargeecINC, na.rm=T),
                     recul=min(annee,na.rm=T)),
              by="NMCNT"]

police = fread("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat2013.csv")
# year="2012"
# policepast=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat",year,".csv",sep=""))
# for (year in c("2011","2010","2009","2008")){
#   temp=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat",
#                    year,".csv",sep=""),verbose=T)
#   policepast=rbind(policepast,temp)
# }

# policepast$NUMCNT = as.numeric(policepast$NUMCNT)
# policepast = policepast[!is.na(policepast$NUMCNT)]
# setnames(policepast, "NUMCNT", "NMCNT")
# recul=policepast[,list(recul=min(annee)),by="NMCNT"]
# colnames(policepast)
data$NMCNT = as.numeric(data$NMCNT)
data2 = data[, list(nbsinDDE = sum(nbsinDDE, na.rm=T), 
                    nbsinINC = sum(nbsinINC, na.rm=T), 
                    chargeecDDE = sum(chargeecDDE, na.rm=T), 
                    chargeecINC = sum(chargeecINC, na.rm=T), 
                    UP = UP[1]),by="NMCNT"]
data2=subset(data2, data2$UP %in% c("INC","DDE"))

police$NUMCNT = as.numeric(police$NUMCNT)
police = police[!is.na(police$NUMCNT)]
setnames(police, "NUMCNT", "NMCNT")

train = merge(police, data2, by="NMCNT", all.x=T)
train = merge(train, datapast2, by="NMCNT", all.x=T)


# option agression
train$IDAGR001=as.factor(train$IDAGR001)

# type de propriété P=propriétaire, C=copropriétaire, L=locataire
train$CDQUALP=as.factor(train$CDQUALP)

# M=maison, A=appartement, R=rez-de-chaussée, sinon Autre
train$CDHABIT=as.factor(train$CDHABIT)

# type de résidence (secondaire S ou principale P)
train$CDRESID=as.factor(train$CDRESID)

#Nombre de pièces, les niveaux sont [|1;18|] ainsi que unimodales sur 28 29 42 55 90 99
train=subset(train,NBPIECS<20)
train$NBPIECS=ifelse(train$NBPIECS>7,7,train$NBPIECS)


# On aggrège les variables d'adresse+nom du risque/souscripteur
train$ADRL=paste(train$ADRLA,train$ADRLB,train$ADRLC,train$ADRLD,train$ADRLE)

train=train[,-which(colnames(train) %in% c("ADRLA","ADRLB","ADRLC","ADRLD","ADRLE")),with=F]

train$ADRQ=paste(train$ADRQA001,train$ADRQB001,train$ADRQC001,train$ADRQD001)

train=train[,-which(colnames(train) %in% c("ADRQA001","ADRQB001","ADRQC001","ADRQD001")),with=F]

# On supprime les variables vides et unimodales
train=train[,-which(apply(train,2,function(x) sum(is.na(x)))==length(train$NMCNT)),with=FALSE]

train=train[,-which(apply(train,2,function(x) sum(x==x[1]))==length(train$NMCNT)),with=FALSE]


# On remplace les valeurs NA issues du merge en 0 (charge et nbsin)
f_dowle3 = function(DT) {

  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}
f_dowle3(train)


# On supprime les valeurs de charge abérantes ie négatives
train=subset(train,train$chargeecDDE>=0)


# L'ancienneté du contrat en mois

train$anciennete=2014-as.numeric(substr(train$DTFAN,7,11))-as.numeric(substr(train$DTFAN,4,5))/12
train$anciennete=ifelse(train$anciennete>10,10,train$anciennete)
train=subset(train, train$anciennete>0)

# 0 = NA, 1 = <5 ans, 2 = 5 à 10 ans, 3 = >10 ans
summary(train$ANCLG001)

# Qu'est ce ?
train$BDGET001=ifelse(train$BDGET001=="N",0,ifelse(train$BDGET001=="O",1,-1))
# INSERT
train$INSER001=ifelse(train$INSER001=="",0,1)
View(train)
return(train)
}


#********************DATA preparation for testing next year************************
loadtest=function(){
datatest=fread("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres2014.csv")
# Construction de la DB des observations passées
datatestpast=fread("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres2013.csv")
year="2013"
setnames(datatestpast,which(grepl(year,colnames(datatestpast))),gsub(year,"",colnames(datatestpast))[grepl(year,colnames(datatestpast))])
for (year in c("2012","2011","2010","2009","2008")){
  temp=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV sinistres",year,".csv",sep=""))
  setnames(temp,which(grepl(year,colnames(temp))),gsub(year,"",colnames(temp))[grepl(year,colnames(temp))])
  datatestpast=rbind(datatestpast,temp)
}
datatestpast$NMCNT=as.numeric(datatestpast$NMCNT)

t = subset(datatestpast, datatestpast$chargeec > 0)
datatestpast2 = t[, list(nbsinBDGpast = sum(nbsinBDG, na.rm=T),
                     nbsinVOLpast = sum(nbsinVOL, na.rm=T),
                     nbsinCLIMpast = sum(nbsinCLIM, na.rm=T),
                     nbsinRCpast = sum(nbsinRC, na.rm=T),
                     nbsinAUTRESpast = sum(nbsinAUTRES, na.rm=T),
                     nbsinCATNATpast = sum(nbsinCATNAT, na.rm=T),
                     nbsinDDEpast = sum(nbsinDDE, na.rm=T), 
                     nbsinINCpast = sum(nbsinINC, na.rm=T), 
                     nbsinpast=sum(nbsin,na.rm=T),
                     chargeecBDGpast = sum(chargeecBDG, na.rm=T), 
                     chargeecVOLpast = sum(chargeecVOL, na.rm=T), 
                     chargeecCLIMpast = sum(chargeecCLIM, na.rm=T), 
                     chargeecRCpast = sum(chargeecRC, na.rm=T), 
                     chargeecAUTRESpast = sum(chargeecAUTRES, na.rm=T), 
                     chargeecCATNATpast = sum(chargeecCATNAT, na.rm=T), 
                     chargeecpast = sum(chargeec, na.rm=T), 
                     chargeecDDEpast = sum(chargeecDDE, na.rm=T), 
                     chargeecINCpast = sum(chargeecINC, na.rm=T),
                     recul=min(annee,na.rm=T)),
              by="NMCNT"]

policetest = fread("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat2014.csv")
# year="2012"
# policetestpast=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat",year,".csv",sep=""))
# for (year in c("2011","2010","2009","2008")){
#   temp=fread(paste("C://Users//p-condemine//Documents//agpc 8102014//CSV reducecontrat",
#                    year,".csv",sep=""),verbose=T)
#   policetestpast=rbind(policetestpast,temp)
# }

# policetestpast$NUMCNT = as.numeric(policetestpast$NUMCNT)
# policetestpast = policetestpast[!is.na(policetestpast$NUMCNT)]
# setnames(policetestpast, "NUMCNT", "NMCNT")
# recul=policetestpast[,list(recul=min(annee)),by="NMCNT"]
# colnames(policetestpast)
datatest$NMCNT = as.numeric(datatest$NMCNT)
datatest2 = datatest[, list(nbsinDDE = sum(nbsinDDE, na.rm=T), nbsinINC = sum(nbsinINC, na.rm=T), chargeecDDE = sum(chargeecDDE, na.rm=T), chargeecINC = sum(chargeecINC, na.rm=T), UP = UP[1]),by="NMCNT"]
datatest2=subset(datatest2, datatest2$UP %in% c("INC","DDE"))

policetest$NUMCNT = as.numeric(policetest$NUMCNT)
policetest = policetest[!is.na(policetest$NUMCNT)]
setnames(policetest, "NUMCNT", "NMCNT")

test = merge(policetest, datatest2, by="NMCNT", all.x=T)
test = merge(test, datatestpast2, by="NMCNT", all.x=T)


# option agression
test$IDAGR001=as.factor(test$IDAGR001)

# type de propriété P=propriétaire, C=copropriétaire, L=locataire
test$CDQUALP=as.factor(test$CDQUALP)

# M=maison, A=appartement, R=rez-de-chaussée, sinon Autre
test$CDHABIT=as.factor(test$CDHABIT)

# type de résidence (secondaire S ou principale P)
test$CDRESID=as.factor(test$CDRESID)

#Nombre de pièces, les niveaux sont [|1;18|] ainsi que unimodales sur 28 29 42 55 90 99
test=subset(test,NBPIECS<20)
test$NBPIECS=ifelse(test$NBPIECS>7,7,test$NBPIECS)


# On aggrège les variables d'adresse+nom du risque/souscripteur
test$ADRL=paste(test$ADRLA,test$ADRLB,test$ADRLC,test$ADRLD,test$ADRLE)

test=test[,-which(colnames(test) %in% c("ADRLA","ADRLB","ADRLC","ADRLD","ADRLE")),with=F]

test$ADRQ=paste(test$ADRQA001,test$ADRQB001,test$ADRQC001,test$ADRQD001)

test=test[,-which(colnames(test) %in% c("ADRQA001","ADRQB001","ADRQC001","ADRQD001")),with=F]

# On supprime les variables vides et unimodales
test=test[,-which(apply(test,2,function(x) sum(is.na(x)))==length(test$NMCNT)),with=FALSE]

test=test[,-which(apply(test,2,function(x) sum(x==x[1]))==length(test$NMCNT)),with=FALSE]


# On remplace les valeurs NA issues du merge en 0 (charge et nbsin)
f_dowle3 = function(DT) {
  
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}
f_dowle3(test)


# On supprime les valeurs de charge abérantes ie négatives
test=subset(test,test$chargeecDDE>=0)


# L'ancienneté du contrat en mois

test$anciennete=2015-as.numeric(substr(test$DTFAN,7,11))-as.numeric(substr(test$DTFAN,4,5))/12
test$anciennete=ifelse(test$anciennete>10,10,test$anciennete)
test=subset(test, test$anciennete>0)

# 0 = NA, 1 = <5 ans, 2 = 5 à 10 ans, 3 = >10 ans
summary(test$ANCLG001)

# Qu'est ce ?
test$BDGET001=ifelse(test$BDGET001=="N",0,ifelse(test$BDGET001=="O",1,-1))
# INSERT
test$INSER001=ifelse(test$INSER001=="",0,1)
View(test)
return(test)
}

train=loadtrain()
test=loadtest()



#********************UNIV STAT************************
{
# index = grepl("chargeec", colnames(data))
# 
# chargeec = apply(data[,which(index)[-1],with=F],1,sum)
# 
# hist(data$chargeec2013[ data$chargeec2013 > 0 & data$chargeec2013 < 10000])
# 
# hist(data$nbsin2013)
# 
# for(i in 1:ncol(data))
# {
#   if(class(data[[i]]) == "character")
#   {
#     print(colnames(data)[i])
#     print(length(unique(data[[i]])))
#   }
#     
# }
# 
# t = data[chargeec2013 > 0 & UP == "DDE", list(Chargeec = mean(chargeec2013, na.rm=T),freq=sum(nbsin2013, na.rm=T)), by='dep']
# 
# hist(data$chargeecINC[ data$chargeecINC > 0 & data$chargeecINC < 10000] )
# 
# # Etude des types de voies 
# colnames(train)
# sum(as.numeric(train$nbsinDDE))
# TypeVoie=c("rue","avenue"," av ","impasse","place","voie"," bd ","boulevard","allee"," pont ","lotissement","parc","chemin"," ch "," vallon "," cite "," res ","residence"," quai "," mail "," mas "," ZI "," square "," villa "," route "," passage "," domaine ")
# Cout=c()
# NbSin=c()
# Taille=c()
# resid=train
# for (a in TypeVoie){
# #On préfère connaître l'adresse du risque, s'il 'nest pas renseigné c'est que le souscripteur déclare l'adresse du risque directement   
# x=train$chargeecDDE[grepl(a,ifelse(train$ADRQ=="   ",train$ADRL,train$ADRQ),perl=TRUE,ignore.case=TRUE)]
# resid=subset(resid,!grepl(a,ifelse(resid$ADRQ=="   ",resid$ADRL,resid$ADRQ),perl=TRUE,ignore.case=TRUE))
# NbSin=length(x[x>0])
# Taille=c(Taille,length(x))
# x=subset(x,!is.na(x))
# Cout=c(Cout,sum(x))
# }
# CMTypeVoie=data.frame(TypeVoie,Taille,NbSin,Cout)
# sum(CMTypeVoie$Taille)/length(train$chargeecDDE)
# CMTypeVoie
# ifelse(resid$ADRQ=="   ",resid$ADRL,resid$ADRQ)
# 


}
#*******************CLEANING THE PRICING TABLE**************

# variables explicatives
spectrainDDE=function(){
varall=c("nbsinDDEpast","chargeecDDEpast","nbsinpast","chargeecpast","MTCAPASS",
         "NBPIECS","IDAGR001","CDQUALP","CDHABIT","CDRESID","INSER001","anciennete",
         "ANCLG001","BDGET001")

varexp=c("CDHABIT","IDAGR001","chargeecDDEpast","chargeecpast","nbsinpast",
"CDQUALP","nbsinDDEpast","NBPIECS","MTCAPASS","anciennete")

varuseless=varall[which(!(varall %in% varexp))]

trainDDE=train[,c("chargeecDDE","nbsinDDE",varexp),with=F]


set(trainDDE,j="chargeecDDE",value=floor(trainDDE[["chargeecDDE"]]/100)*100)
set(trainDDE,j="chargeecpast",value=floor(trainDDE[["chargeecpast"]]/500)*500+1)
set(trainDDE,j="chargeecDDEpast",value=floor(trainDDE[["chargeecDDEpast"]]/500)*500+1)
set(trainDDE,j="MTCAPASS",value=floor(trainDDE[["MTCAPASS"]]/500)*500)
set(trainDDE,j="nbsinpast",value=ifelse(trainDDE[["nbsinpast"]]==0,0,1))
set(trainDDE,j="nbsinDDEpast",value=ifelse(trainDDE[["nbsinDDEpast"]]==0,0,1))

varfact=c(3,4,7:9)


for (i in colnames(trainDDE)[varfact]){
  set(trainDDE,j=i,value=as.factor(trainDDE[[i]]))
}
print(trainDDE)
return(trainDDE)
}

spectestDDE=function(){
varall=c("nbsinDDEpast","chargeecDDEpast","nbsinpast","chargeecpast","MTCAPASS",
         "NBPIECS","IDAGR001","CDQUALP","CDHABIT","CDRESID","INSER001","anciennete",
         "ANCLG001","BDGET001")

varexp=c("CDHABIT","IDAGR001","chargeecDDEpast","chargeecpast","nbsinpast",
         "CDQUALP","nbsinDDEpast","NBPIECS","MTCAPASS","anciennete")

varuseless=varall[which(!(varall %in% varexp))]

testDDE=test[,c("chargeecDDE","nbsinDDE",varexp),with=F]
set(testDDE,j="chargeecDDE",value=floor(testDDE[["chargeecDDE"]]/100)*100)
set(testDDE,j="chargeecpast",value=floor(testDDE[["chargeecpast"]]/500)*500+1)
set(testDDE,j="chargeecDDEpast",value=floor(testDDE[["chargeecDDEpast"]]/500)*500+1)
set(testDDE,j="MTCAPASS",value=floor(testDDE[["MTCAPASS"]]/500)*500)
set(testDDE,j="nbsinpast",value=ifelse(testDDE[["nbsinpast"]]==0,0,1))
set(testDDE,j="nbsinDDEpast",value=ifelse(testDDE[["nbsinDDEpast"]]==0,0,1))
varfact=c(3,4,7:9)
for (i in colnames(testDDE)[varfact]){
  set(testDDE,j=i,value=as.factor(testDDE[[i]]))
}
print(testDDE)
return(testDDE)
}

trainDDE=spectrainDDE()
testDDE=spectestDDE()

# c<-ggplot(train,aes(chargeecDDEpast,chargeecDDE))
# c+stat_smooth()+geom_point()

#********************PRICING METHODS************************
# GLM
model1cost<-glm(log(chargeecDDE)~(.-nbsinDDE)^2,family=Gamma(link="identity"),data=subset(trainDDE,trainDDE$chargeecDDE>0))
model1cost<-stepAIC(model1cost,verbose=TRUE)
#Trop lent pour intégrer des interactions massivement 
model1freq<-glm(ifelse(nbsinDDE>0,1,0)~.-chargeecDDE,family=binomial(link="logit"),data=trainDDE)
model1freq<-stepAIC(model1freq,verbose=TRUE)
predcost1=exp(predict.glm(model1cost,testDDE,type="response"))
predfreq1=predict.glm(model1freq,testDDE,type="response")
pGLM=predcost1*predfreq1
CA1=sum(predcost1*predfreq1)-sum(testDDE$chargeecDDE)


# James Bond

modelCGF<-glm(log(chargeecDDE)~.^2,family=Gamma(link="identity"),data=subset(trainDDE,trainDDE$chargeecDDE>0))
modelCGF<-stepAIC(modelCGF,verbose=F)
pCGF=exp(predict.glm(modelCGF,testDDE,type="response"))

# 
# # GAM
# model2cost=gam(log(chargeecDDE)~s(NBPIECS,k=4)+s(chargeecDDEpast)+s(MTCAPASS,k=6)+
#              nbsinpast+CDHABIT+s(anciennete,k=8)+CDQUALP+IDAGR001,data=subset(trainDDE,
#             trainDDE$chargeecDDE>0),family=Gamma(link="identity"),method="REML",selection=T)
# model2freq=gam(nbsinDDE~s(NBPIECS,k=4)+s(chargeecDDEpast)+s(MTCAPASS,k=6)+
#                  nbsinpast+CDHABIT+s(anciennete,k=8)+CDQUALP+IDAGR001,data=trainDDE,
#                family=nb(link="log"),method="REML",selection=T)
#   vis.gam(model2cost,theta=30,phi=30)
# # ici on pourrait utiliser ggvis pour faire défiler theta et phi ?
# predcost2=exp(predict.gam(model2cost,testDDE,type="response"))
# predfreq2=predict.gam(model2freq,testDDE,type="response")
# CA2=sum(predcost2*predfreq2)-sum(testDDE$chargeecDDE)




varfact=c(1,3,4,7:9)
for (i in colnames(trainDDE)[-varfact]){
  set(trainDDE,j=i,value=as.factor(trainDDE[[i]]))
}
for (i in colnames(testDDE)[-varfact]){
  set(testDDE,j=i,value=as.factor(testDDE[[i]]))
}



# Gradient Boosting

grid=expand.grid(100,c(5,10,15,20,25,30),c(10,100,1000,10000),c(0.001,0.005,0.01,0.03,0.06,0.1),0.8)
CA<-function(param){
model3=gbm(chargeecDDE~.-nbsinDDE,data=trainDDE,distribution="gaussian",n.trees=param[1],
           interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
           verbose=F,bag.fraction=param[5],train.fraction=0.7,n.cores=4)
reponse=predict.gbm(object=model3,newdata=testDDE,type="response")
# Modification de l'erreur quadratique à chaque iteration
VE=model3$valid.error
m=length(VE)
DVE=VE[1:(m-1)]-VE[2:m]
y=DVE/DVE[1]
x=c(y[y<0.1],min(y))
best=which(y==x[1])
SqErLoss=model3$oobag.improve
n=length(SqErLoss)
# De combien le gain par itération a diminué. Si 100% alors on overfit probablement. Si 0% on n'a servi à rien
gainSEpct=100*(1-max(SqErLoss[c(n-5,n)])/SqErLoss[1])
# chiffre d'affaire si on applique le tarif l'année suivante
charges=sum(as.numeric(testDDE$chargeecDDE))
# la variable valid.error donne la déviance itération par itération, ici on a donc le gain cumulé de déviance et la déviance totale
rep=c(param,sum(reponse)-charges,gainSEpct,SqErLoss[1],model3$valid.error[1],1-model3$valid.error[best]/model3$valid.error[1],best,x[1])
print(rep)
return(rep)
}
charges=sum(as.numeric(testDDE$chargeecDDE))
chargespast=sum(as.numeric(trainDDE$chargeecDDE))
trainDDE$annee
# http://stackoverflow.com/questions/8722247/r-caret-and-gbm-cant-find-ntrees-input
result=apply(grid,1,CA)
gridopti=t(result)
gridopti=data.frame(gridopti)
setnames(gridopti,c("ntrees","depth","nbobsnode","shrinkage","bag.frac","CA","SqErrGain","FirstGainErr",
                    "devianceValidation","gainDevianceValidation","bestIter","baisseGainDeviance"))
gridopti=gridopti[(gridopti$baisseGainDeviance<0.1)&(gridopti$baisseGainDeviance>-0.05),]
gridopti$SP=charges/(gridopti$CA+charges)

plot(gridopti$bestIter,gridopti$CA)
which(gridopti$CA==max(gridopti$CA))

###################################################################
data <- data.frame(year=rep(2000:2002, each=23), 
                  x=rnorm(23*3,10), y=rnorm(23*3,10),
                  count=c(rnorm(23,2), rnorm(23,4), rnorm(23,6)))
data %>% 
  ggvis(~x, ~y, size=~count) %>% 
  layer_points(opacity=input_slider(min(data$year), max(data$year), step=1, 
                                    map=function(x) ifelse(data$year == x, 1, 0)))

#########################
CA(param)

param=c(30,20,2000,0.02,0.8)
# tdist, laplace, gaussian
model3=gbm(chargeecDDE~.-nbsinDDE,data=trainDDE,distribution="gaussian",n.trees=param[1],
           interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
           verbose=T,bag.fraction=param[5],train.fraction=0.7)
reponse=predict.gbm(object=model3,newdata=testDDE,type="response")


# Séparons fréquence et coût parce que la loi Gaussienne n'est clairement pas bonne pour un échantillon >=0 zero inflated
param=c(45,20,50,0.02,0.8)
gbmcost=gbm(chargeecDDE~.-nbsinDDE,data=trainDDE[trainDDE$chargeecDDE>0],distribution="gaussian",n.trees=param[1],
            interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
            verbose=T,bag.fraction=param[5],train.fraction=0.7)
repcost=predict.gbm(object=gbmcost,newdata=testDDE,type="response")

param=c(30,20,100,0.05,0.8)
gbmfreq=gbm(ifelse(nbsinDDE==0,0,1)~.-chargeecDDE,data=trainDDE,distribution="bernoulli",n.trees=param[1],
            interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
            verbose=T,bag.fraction=param[5],train.fraction=0.7)
repfreq=predict.gbm(object=gbmfreq,newdata=testDDE,type="response")
pGBM=repcost*repfreq

# On va tenter un nouveau gbm sur plus de données
TrainGBM=function(){
train2=train
for (i in colnames(train2)){
  set(train2,j=i,value=as.factor(train2[[i]]))
}

varuseless=c("DTFAN","DTMAN","DTFRS","DTMRS","COPHT","COPTTC","DTFRP","DTMRP","NMPORT","CDIVILRQ","CDUVENVI","INSER001","inhab","chargeecCLIMpast",
             "chargeecAUTRESpast","chargeecCATNATpast","nbsinAUTRESpast","nbsinCATNATpast","nbsinCLIMpast","ADRQ","ADRL","UP")
varuseful=colnames(train)[which(!(colnames(train) %in% varuseless))]
train2=train2[,varuseful,with=F]
typesvoies<-fread("C:/Users/p-condemine/Dropbox/Phileas/retraitement adresses/2013_contrats_type_voie.csv")
typesvoies$NUMCNT=as.character(typesvoies$NUMCNT)
typesvoies$NUMCNT=as.numeric(typesvoies$NUMCNT)
train2$NMCNT=as.character(train2$NMCNT)
train2$NMCNT=as.numeric(train2$NMCNT)
setnames(typesvoies,"NUMCNT","NMCNT")
typesvoies=unique(typesvoies,by="NMCNT")
train2=merge(x=train2,y=typesvoies,by="NMCNT",all.x=T)
varuseful=colnames(train2)[which(!(colnames(train2) %in% c("adr")))]
train2=train2[,varuseful,with=F]

for (i in c("chargeecDDE","chargeecpast","chargeecDDEpast","chargeecBDGpast","chargeecVOLpast",
            "chargeecRCpast","chargeecINCpast","MTCAPASS","nbsinpast","nbsinDDEpast","nbsinINCpast",
            "nbsinRCpast","nbsinVOLpast","nbsinBDGpast")){
  set(train2,j=i,value=as.character(train2[[i]]))
  set(train2,j=i,value=as.numeric(train2[[i]]))
if (grepl("chargeec",i)){
set(train2,j=i,value=floor(train2[[i]]/100)*100)}
if(i=="MTCAPASS"){
set(train2,j=i,value=floor(train2[[i]]/500)*500)}
if (grepl("nbsin",i)){
set(train2,j=i,value=ifelse(train2[[i]]==0,0,1))}
}
set(train2,j="type voie",value=as.factor(train2[["type voie"]]))
return(train2)
}
TestGBM=function(){
  test2=test
  for (i in colnames(test2)){
    set(test2,j=i,value=as.factor(test2[[i]]))
  }
  
  varuseless=c("DTFAN","DTMAN","DTFRS","DTMRS","COPHT","COPTTC","DTFRP","DTMRP","NMPORT","CDIVILRQ","CDUVENVI","INSER001","inhab","chargeecCLIMpast",
               "chargeecAUTRESpast","chargeecCATNATpast","nbsinAUTRESpast","nbsinCATNATpast","nbsinCLIMpast","ADRQ","ADRL","UP")
  varuseful=colnames(test)[which(!(colnames(test) %in% varuseless))]
  test2=test2[,varuseful,with=F]
  typesvoies<-fread("C:/Users/p-condemine/Dropbox/Phileas/retraitement adresses/2013_contrats_type_voie.csv")
  typesvoies$NUMCNT=as.character(typesvoies$NUMCNT)
  typesvoies$NUMCNT=as.numeric(typesvoies$NUMCNT)
  test2$NMCNT=as.character(test2$NMCNT)
  test2$NMCNT=as.numeric(test2$NMCNT)
  setnames(typesvoies,"NUMCNT","NMCNT")
  typesvoies=unique(typesvoies,by="NMCNT")
  test2=merge(x=test2,y=typesvoies,by="NMCNT",all.x=T,all.y=F)
  varuseful=colnames(test2)[which(!(colnames(test2) %in% c("adr")))]
  test2=test2[,varuseful,with=F]
  
  for (i in c("chargeecDDE","chargeecpast","chargeecDDEpast","chargeecBDGpast","chargeecVOLpast",
              "chargeecRCpast","chargeecINCpast","MTCAPASS","nbsinpast","nbsinDDEpast","nbsinINCpast",
              "nbsinRCpast","nbsinVOLpast","nbsinBDGpast")){
    set(test2,j=i,value=as.character(test2[[i]]))
    set(test2,j=i,value=as.numeric(test2[[i]]))
    if (grepl("chargeec",i)){
      set(test2,j=i,value=floor(test2[[i]]/100)*100)}
    if(i=="MTCAPASS"){
      set(test2,j=i,value=floor(test2[[i]]/500)*500)}
    if (grepl("nbsin",i)){
      set(test2,j=i,value=ifelse(test2[[i]]==0,0,1))}
  }
  set(test2,j="type voie",value=as.factor(test2[["type voie"]]))
  return(test2)
}
train2=TrainGBM()
test2=TestGBM()

param=c(35,20,200,0.02,0.6)
model4=gbm(chargeecDDE~.-nbsinDDE-chargeecINC,data=train2,distribution="gaussian",n.trees=param[1],
           interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
           verbose=T,bag.fraction=param[5],train.fraction=0.7)
gbm2=predict.gbm(object=model4,newdata=test2,type="response")

#Encore une fois on va décomposer fréquence et coût
grid=expand.grid(ntree=100,depth=c(5,10,15),nbobs=c(5,20,50),shrinkage=c(0.01,0.025))

library(parallel)
library(foreach)
library(doSNOW)
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)
# train2$nbsinDDE=1*(as.numeric(as.character(train2$nbsinDDE))>0)
x<-foreach(k=1:length(grid[,1]),.packages="gbm",.combine=rbind) %dopar% 
  {
  gbmfreq2=gbm(nbsinDDE~.-chargeecDDE-chargeecINC,data=train2,distribution="bernoulli",n.trees=grid[k,1],
               interaction.depth=grid[k,2],n.minobsinnode=grid[k,3],shrinkage=grid[k,4],
               verbose=T,bag.fraction=0.6,train.fraction=0.7)
# repcost2=predict.gbm(object=gbmfreq2,newdata=test2,type="response")

indmin=which(gbmfreq2$valid.error==min(gbmfreq2$valid.error))
return(c(grid[k,],
  indmin=indmin,opti=(gbmfreq2$valid.error[1]-gbmfreq2$valid.error[indmin])/(gbmfreq2$train.error[1]-gbmfreq2$train.error[indmin]),
  validIni=gbmfreq2$valid.error[1],trainIni=gbmfreq2$train.error[1],
  validOpti=gbmfreq2$valid.error[indmin],trainOpti=gbmfreq2$train.error[indmin]))
}
stopCluster(cl)
x=data.frame(x)
View(x[order(unlist(lapply(x$opti, '[[', 1)),decreasing=T),])



param=c(100,10,50,0.001,0.6)
gbmcost2=gbm(chargeecDDE~.-nbsinDDE-chargeecINC,data=train2cost,distribution="gaussian",n.trees=param[1],
             interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
             verbose=T,bag.fraction=param[5],train.fraction=0.8)
repcost2=predict.gbm(object=gbmcost2,newdata=test2,type="response")

param=c(100,5,5,0.025,0.6)
gbmfreq2=gbm(ifelse(nbsinDDE==0,0,1)~.-chargeecDDE-chargeecINC,data=train2,distribution="bernoulli",n.trees=param[1],
            interaction.depth=param[2],n.minobsinnode=param[3],shrinkage=param[4],
            verbose=T,bag.fraction=param[5],train.fraction=0.8)
repfreq2=predict.gbm(object=gbmfreq2,newdata=test2,type="response")
pGBM2=repfreq2*repcost2
# RandomForest
# 
# 
#  
# setnames(train2,"type voie","typevoie")
# setnames(test2,"type voie","typevoie")
# set(train2,j="anciennete",value=as.character(train2[["anciennete"]]))
# set(train2,j="anciennete",value=as.numeric(train2[["anciennete"]]))
# set(test2,j="anciennete",value=as.character(test2[["anciennete"]]))
# set(test2,j="anciennete",value=as.numeric(test2[["anciennete"]]))
# set(train2,j="SURFDEP",value=as.character(train2[["SURFDEP"]]))
# set(train2,j="SURFDEP",value=as.numeric(train2[["SURFDEP"]]))
# set(test2,j="SURFDEP",value=as.character(test2[["SURFDEP"]]))
# set(test2,j="SURFDEP",value=as.numeric(test2[["SURFDEP"]]))
# 
# set(train2,j="chargeecINC",value=as.character(train2[["chargeecINC"]]))
# set(train2,j="chargeecINC",value=as.numeric(train2[["chargeecINC"]]))
# set(test2,j="chargeecINC",value=as.character(test2[["chargeecINC"]]))
# set(test2,j="chargeecINC",value=as.numeric(test2[["chargeecINC"]]))
# 
# summary(nbsin)
# set(train2,j="typevoie",value=as.factor(ifelse(train2[["typevoie"]] %in% c("BD","BL","BLD","BOU","BOULEVARD","BV","BVD"),"BOULEVARD",
#                                                ifelse(train2[["typevoie"]] %in% c("AL","ALLEE"),"ALLEE",
#                                                       ifelse(train2[["typevoie"]] %in% c("AV","AVENUE"),"AVENUE",
#                                                              ifelse(train2[["typevoie"]] %in% c("ANCIEN CH","CH","CHEMIN"),"CHEMIN", 
#                                                                     ifelse(train2[["typevoie"]] %in% c("PL","PLACE"),"PLACE",
#                                                                            ifelse(train2[["typevoie"]] %in% c("QRT","QU","QUARTIER"),"QUARTIER",
#                                                                                   ifelse(train2[["typevoie"]] %in% c("R","RUE","RU"),"RUE",
#                                                                                          ifelse(train2[["typevoie"]] %in% c("SQ","SQUARE","SUARE"),"SQUARE",
#                                                                                                 ifelse(train2[["typevoie"]] %in% c("TRA","TRAV","TRSE","TRV","TSE","TSRE"),"TRAVERSE",
#                                                                                                        ifelse(train2[["typevoie"]] %in% c("VILLA","VLA"),"VILLA",
#                                                                                                               ifelse(train2[["typevoie"]] %in% c("13","16","17","18","19","20","21","22","23","24",
#                                                                                                                                                  "25","26","27","28","29","30","31","34","35","38","40","41","42","43","44","47","48","58","61"),"ERREUR",
#                                                                                                                      train2[["typevoie"]])))))))))))))
# train2=subset(train2,CDDEPAGT %in% c("13","75","76","77","78","84","91","92","93","94","95","14","33","45","50","59","60","69"))
# test2=subset(test2,CDDEPAGT %in% c("13","75","76","77","78","84","91","92","93","94","95","14","33","45","50","59","60","69"))
# train2=drop.levels(train2)
# test2=drop.levels(test2)
# set(train2,j="CDDEPAGT",value=as.character(train2[["CDDEPAGT"]]))
# set(train2,j="CDDEPAGT",value=as.numeric(train2[["CDDEPAGT"]]))
# 
# summary(train2$CDDEPAGT)
# 
# 
# RFcost=randomForest(chargeecDDE~.-nbsinDDE-chargeecINC,data=train2[train2$chargeecDDE>0],ntree=1000,do.trace=TRUE,na.action=na.omit,importance=T)
# RFfreq=randomForest(ifelse(nbsinDDE==0,0,1)~.-chargeecDDE-chargeecINC,data=train2,ntree=10,do.trace=TRUE,na.action=na.omit,importance=T)
# resRFcost=predict(RFcost,test2,type="response")
# resRFfreq=predict(RFfreq,test2,type="response")

# TREE
# plot(RF)
# text(RF)
# tree_data <- dendro_data(RF)
# ggplot(segment(tree_data)) +
#   geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n),colour="lightblue") +
#   scale_size("n") +
#   geom_text(data=label(tree_data),
#             aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
#   geom_text(data=leaf_label(tree_data),
#             aes(x=x, y=y, label=label), vjust=0.5, size=3) +
#   theme_dendro()





# Maintenant on compare les PP proposées

Price=testDDE[,c("NMCNT","chargeecDDE"),with=F]
# Price$pGBM=reponse
Price$pGLM=predcost1*predfreq1
Price$pGenius=pCGF
# Price$pRF=resRF
# Price$pGBM2=gbm2
Price$pGBM=repcost*repfreq
Price$pGBM2=repcost2*repfreq2
# Price$chargeecDDE=sum(trainDDE$chargeecDDE)/sum(testDDE$chargeecDDE)*Price$chargeecDDE
# sum(Price$pGBM)/sum(Price$pGLM)


# sum(testDDE$chargeecDDE)

Price$alloc1=ifelse(Price$pGBM<Price$pGLM,1,0)

CAgbm=sum(Price$alloc1*(Price$pGBM-Price$chargeecDDE))
CAglm=sum((1-Price$alloc1)*(Price$pGLM-Price$chargeecDDE))
SPgbm=sum(Price$alloc1*Price$chargeecDDE)/sum(Price$alloc1*Price$pGBM)
SPglm=sum((1-Price$alloc1)*Price$chargeecDDE)/sum((1-Price$alloc1)*Price$pGLM)

Price1=Price[order(Price$pGLM,decreasing=TRUE)]
Price1$ChargeCum=cumsum(Price1$chargeecDDE)/sum(Price1$chargeecDDE)
AUC1=mean(Price1$ChargeCum)-1/2


Price2=Price[order(Price$pGBM,decreasing=TRUE)]
Price2$ChargeCum=cumsum(Price2$chargeecDDE)/sum(Price2$chargeecDDE)
AUC2=mean(Price2$ChargeCum)-1/2

Price3=Price[order(Price$pGBM2,decreasing=TRUE)]
Price3$ChargeCum=cumsum(Price3$chargeecDDE)/sum(Price3$chargeecDDE)
AUC3=mean(Price3$ChargeCum)-1/2


# Tarification pour celui qui sait QUI aura un sinistre mais ne sait pas avec certitude combien il coutera
PriceG=Price[order(Price$pGenius,decreasing=TRUE)]
PriceG$ChargeCum=cumsum(PriceG$chargeecDDE)/sum(PriceG$chargeecDDE)
AUCG=mean(PriceG$ChargeCum)-1/2

# Tarification actuelle
PriceReal=Price[order(test$COPHT,decreasing=TRUE)]
PriceReal$ChargeCum=cumsum(PriceReal$chargeecDDE)/sum(PriceReal$chargeecDDE)
AUC4=mean(PriceReal$ChargeCum)-1/2

reference=data.frame(1:length(Price1$ChargeCum),(1:length(Price1$ChargeCum))/length(Price1$ChargeCum))
setnames(reference,c("x","y"))
# Price$alloc2=rmultinom(1,1,prob=1/c(Price$pGBM,Price$pGLM,Price$pRF))



ggplot()+
  geom_line(data=Price1,aes(x=1:length(Price1$ChargeCum),colour="GLM",y=ChargeCum))+
   geom_line(data=Price2,aes(x=1:length(Price2$ChargeCum),colour="GBM",y=ChargeCum))+
   geom_line(data=Price3,aes(x=1:length(Price3$ChargeCum),colour="GBM2",y=ChargeCum))+
   geom_line(data=PriceG,aes(x=1:length(PriceG$ChargeCum),colour="Genius",y=ChargeCum))+
   geom_line(data=PriceReal,aes(x=1:length(PriceReal$ChargeCum),colour="Current",y=ChargeCum))+
   geom_line(data=reference,aes(x=x,y=y,colour="Random"))




# quatres objectifs prioritaires : 
#   1)) faire une fonction qui transforme un tree ou glm en prime
#   2)) faire une fonction qui transforme un set de n primes en 
# une décision de souscription par comparaison selon la distance entre les
# primes proposées => CA, S/P
#   3)) projection de exposition * prime sur le volume de sinistres avérés => CA, S+C/P,
#   4)) construction de courbes de Lorenz et risque et tests de robustess : ggvis ou shiny pour intégrer des choses random et autres déviations
