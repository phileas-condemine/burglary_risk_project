library(fields)
library(reshape)
library(ggplot2)
library(stargazer)
#setwd("C:/Users/a-gerbeaux/Documents/Documents/data pour boxplot")


# 
# main <- function(){
#   load("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/OutputComparaison 2014 08 14 18 20 07 RData")
#   
# }
# 
# 
# #############################
# ##Comparaison des 4 modèles##
# #############################
# CompModeles <- function(x){
# output <- output_metrique
# 
# #color_axa <- c("#A33F1F","#752864","#005293", "#427730")
# color_axa <- c("#005293","#A33F1F","#427730","#752864")
# color_axa2 <- c("#005293","#427730")
# legend_models <- c("Gamma", "Tweedie","GLM Frequency/Severity","Decision Tree Frequency/Severity")
# 
# 
# c1 <- ggplot(melt(output[,c(7,13,19,25)]/output[,c(1)]), aes(X2,value, fill = X2)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa, labels =legend_models) +
#   ggtitle("Mean Square Error on test set")
# 
# c2 <- ggplot(melt(output[,c(9,15,21,27)]/output[,c(3)]), aes(X2,value, fill = X2)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa, labels = legend_models) +
#   ggtitle("Mean Absolute Error on test set")
# 
# c3 <- ggplot(melt(output[,c(11,17,23,29)]), aes(X2,value, fill = X2)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa, labels = legend_models) +
#   ggtitle("Spearman Correlation on test set")
# 
# c4<- ggplot(melt(output[,c(12,18,24,30)]), aes(X2,value, fill = X2)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa, labels = legend_models) +
#   ggtitle("Spearman Correlation on training set")
# 
# 
# 
# 
# 
# return(x)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #############################
# #####Comparaison GLM/RF######
# #############################
# 
# GLMRF <- function(){
# 
# filtre_GLMRF <- read.csv("filtre_FCRF.csv")
# filtre_GLMRF <- filtre_GLMRF[,-1]
# GLMRF <- read.csv("FCRF.csv")
# colnames(GLMRF) <- c("RF MSEt","RF MSEe","RF MAEt","RF MAEe","RF spearmant","RF spearmane",
#                       "FC MSEt","FC MSEe","FC MAEt","FC MAEe","FC spearmant","FC spearmane")
# boxplot(GLMRF[,c(5,11)], main = "Spearman Correlation")
# 
# 
# color_axa2 <-c("#005293", "#006643")
# legend_models2 <- c("GLM Frequency/Severity","Random Forest Frequency/Severity")
#   
# gr1 <-ggplot(melt(GLMRF[,c(7,1)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Mean Square Error on test set")
# 
# gr2 <-ggplot(melt(GLMRF[,c(9,3)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Mean Absolute Error on test set")
# 
# gr3 <-ggplot(melt(GLMRF[,c(12,5)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Spearman Correlation on test set")
# }
# #############################
# #####Comparaison freq/CM#####
# #############################
# Freq <- function(){
# filtre_FCM <- read.csv("filtre_FC.csv")
# Comp_CM_e <- read.csv("Comp_CM_e.csv")
# Comp_CM_t <- read.csv("Comp_CM_t.csv")
# Comp_freq_e <- read.csv("Comp_freq_e.csv")
# Comp_freq_t <- read.csv("Comp_freq_t.csv")
# CM <- cbind(Comp_CM_t[1:100,],Comp_CM_t[101:200,])
# CM <- CM[,c(-1,-5)]
# colnames <- c("MSE GLM", "MAE GLM", "spearman GLM","MSE RF", "MAE RF", "spearman RF")
# colnames(CM)<- colnames
# boxplot(CM[,c(1,4)], main = "Severity: MSE on train test")
# boxplot(CM[,c(2,5)], main = "Severity: MAE on train test")
# boxplot(CM[,c(3,6)], main = "Severity: Spearman Correlation on train test")
# 
# Freq<- cbind(Comp_freq_t[1:100,],Comp_freq_t[101:200,])
# Freq <- Freq[,c(-1,-5)]
# Freq<- Freq[c(-20,-63,-92),]
# colnames <- c("MSE GLM", "MAE GLM", "spearman GLM","MSE RF", "MAE RF", "spearman RF")
# colnames(Freq)<- colnames
#           # boxplot(Freq[,c(1,4)], main = "Frequency: MSE on train test")
#           # boxplot(Freq[,c(2,5)], main = "Frequency: MAE on train test")
#           # boxplot(Freq[,c(3,6)], main = "Frequency: Spearman Correlation on train test")
# }
# ####CM
# 
# CM<- function{
# cm1 <- ggplot(melt(CM[,c(1,4)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Severity: Mean Square Error on test set")
# 
# cm2 <- ggplot(melt(CM[,c(2,5)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Severity: Mean Absolute Error on test set")
# 
# cm3 <- ggplot(melt(CM[,c(2,5)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "Models", values = color_axa2, labels =legend_models2) +
#   ggtitle("Severity: Mean Absolute Error on test set")
# 
# p1 <- ggplot(melt(Freq[,c(1,4)]), aes(variable,value, fill = variable)) + geom_boxplot() +
#   scale_fill_manual(name = "This is my title", values = c("darkblue", "darkgreen"), labels = c("GLM", "RandomForest"))
# p2 <- ggplot(melt(Freq[,c(2,5)]), aes(variable,value)) + geom_boxplot()
# p3 <- ggplot(melt(Freq[,c(3,6)]), aes(variable,value)) + geom_boxplot()
# multiplot(p1, p3, cols=1)
# }




####################
##Meta Paramètre Selection##
####################
setwd("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles")
load("DataComparaison.RData")
#load("OutputComparaison 2014 08 14 18 20 07 RData")
load("filtre 2014 08 14 18 20 07 .RData")
i <- 1
filtre_entrainement <- filtre[,i]
filtre_CM <- filtre_entrainement[data_freq[,"nbVOL"]>0]
library(randomForest)
plot_maxnodes <- function(N){
  plot_m <- list()
  for(i in c(1:N)){
    
    R <- 500 %/% N
    
    RF_cm <- randomForest(formula = CoutMoyenVOL~.,
                          data = data_cm_RF[filtre_CM,],
                          ntree = 100,
                          mtry = 2, 
                          maxnodes = R*i,
                          xtest = data_cm_RF[!filtre_CM,1:5],
                          ytest = data_cm_RF[!filtre_CM,6])
    
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










############################
####boxplot################
############################


load("C:/Users/a-gerbeaux/Documents/0 PROJET/4 Comparaison de Modeles/OutputComparaison 2014 08 14 18 20 07 RData")



###Rapport Latex

theme = theme_set(theme_grey())
color_axa <- c("#005293","#A33F1F","#427730","#752864")
color_axa2 <- c("#005293","#427730")

output <- output_metrique

############Comparison of Models for total Claim on training set
data_preparation <- melt(output[,c(12,18,24,30)])
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma", "Tweedie","Poisson/Gamma","Random Forest"), each = 50)
data_preparation[,1] <- Models 

fig1<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for total Claim on training set") +
  scale_x_discrete(limits=c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")) +
  scale_fill_manual(values = color_axa, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")



data_preparation <- melt(output[,c(11,17,23,29)])
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma", "Tweedie","Poisson/Gamma","Random Forest"), each = 50)
data_preparation[,1] <- Models 

fig2<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for total Claim on test set") +
  scale_x_discrete(limits=c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")) +
  scale_fill_manual(values = color_axa, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")


data_preparation <- melt(output[,c(7,13,19,25)]/output[,c(1)])
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma", "Tweedie","Poisson/Gamma","Random Forest"), each = 50)
data_preparation[,1] <- Models 

fig3<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for total Claim on test set") +
  scale_x_discrete(limits=c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")) +
  scale_fill_manual(values = color_axa, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")


data_preparation <-melt(output[,c(8,14,20,26)]/output[,c(2)])
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma", "Tweedie","Poisson/Gamma","Random Forest"), each = 50)
data_preparation[,1] <- Models 

fig4<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for total Claim on training set") +
  scale_x_discrete(limits=c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")) +
  scale_fill_manual(values = color_axa, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")

#########CM

o1 <- output_metrique_test_cm
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(1,4)]
colnames(o1) <- c("Gamma","Random Forest for Severity")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma","Random Forest for Severity"), each = 50)
data_preparation[,1] <- Models 

fig5<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Severity on test set") +
  scale_x_discrete(limits=c("Gamma","Random Forest for Severity")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")

o1 <- output_metrique_test_cm
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(3,6)]
colnames(o1) <- c("Gamma","Random Forest for Severity")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Gamma","Random Forest for Severity"), each = 50)
data_preparation[,1] <- Models 

fig6<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Severity on test set") +
  scale_x_discrete(limits=c("Gamma","Random Forest for Severity")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")


o1 <- output_metrique_train_cm
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(1,4)]
colnames(o1) <- c("Gamma","Random Forest for Severity")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
#Models <- rep("Poisson/Gamma","Random Forest"), each = 50)
#data_preparation[,1] <- Models 

fig7<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Severity on train set") +
  scale_x_discrete(limits=c("Gamma","Random Forest for Severity")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")

o1 <- output_metrique_train_cm
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(3,6)]
colnames(o1) <- c("Gamma","Random Forest for Severity")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
# Models <- rep("Poisson/Gamma","Random Forest"), each = 50)
# data_preparation[,1] <- Models 

fig8<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Severity on train set") +
  scale_x_discrete(limits=c("Gamma","Random Forest for Severity")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")



#########freq

o1 <- output_metrique_test_freq
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(1,4)]
colnames(o1) <- c("Poisson","Random Forest for frequency")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Poisson","Random Forest for frequency"), each = 50)
data_preparation[,1] <- Models 

fig9<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Frequency on test set") +
  scale_x_discrete(limits=c("Poisson","Random Forest for frequency")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")

o1 <- output_metrique_test_freq
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(3,6)]
colnames(o1) <- c("Poisson","Random Forest for frequency")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
Models <- rep(c("Poisson","Random Forest for frequency"), each = 50)
data_preparation[,1] <- Models 

fig10<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Frequency on test set") +
  scale_x_discrete(limits=c("Poisson","Random Forest for frequency")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")


o1 <- output_metrique_train_freq
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(1,4)]
colnames(o1) <- c("Poisson","Random Forest for frequency")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
#Models <- rep("Poisson/Gamma","Random Forest"), each = 50)
#data_preparation[,1] <- Models 

fig11<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Frequency on train set") +
  scale_x_discrete(limits=c("Poisson","Random Forest for frequency")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Mean Square Error")

o1 <- output_metrique_train_freq
o1 <- cbind(o1[1:50,], o1[51:100,])
o1 <- o1[,c(3,6)]
colnames(o1) <- c("Poisson","Random Forest for frequency")

data_preparation <-melt(o1)
data_preparation <- data_preparation[,-1]
colnames(data_preparation) <- c("Models","values")
# Models <- rep("Poisson/Gamma","Random Forest"), each = 50)
# data_preparation[,1] <- Models 

fig12<- ggplot(data_preparation, aes(Models,values, fill = Models), axis.ticks.x = element_blank()) + 
  geom_boxplot() +
  ggtitle("Comparison of Models for Frequency on train set") +
  scale_x_discrete(limits=c("Poisson","Random Forest for frequency")) +
  scale_fill_manual(values = color_axa2, guide=FALSE)+ 
  xlab("Predictive Models") + 
  ylab("Spearman Correlation")



# 
# fig5
# fig6
# fig7
# fig8
# fig9
# fig10
# fig11
# fig12

multiplot(fig5,fig6, fig7, fig8, cols = 2)

multiplot(fig9,fig10, fig11, fig12, cols = 2)



# multiplot(fig5,fig6)

Result <- matrix(NA,2,4)

Result[1,] <- apply(output[,c(7,13,19,25)],2, median)

Result[2,] <- apply(output[,c(8,14,20,26)],2, median)

rownames(Result) <- c("Test","Entrainement")

colnames(Result) <- c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")

Result2 <- matrix(NA,2,4)

Result2[1,] <- apply(output[,c(11,17,23,29)],2, median)

Result2[2,] <- apply(output[,c(12,18,24,30)],2, median)

rownames(Result2) <- c("Test","Entrainement")

colnames(Result2) <- c("Gamma", "Tweedie","Poisson/Gamma","Random Forest")

Result3 <- matrix(NA,2,4)
rownames(Result3) <- c("Test","Entrainement")
colnames(Result3) <- c("Gamma MSE", "RF MSE","Gamma Spearman","RF Spearman")

local <- output_metrique_test_cm
local <- cbind(local[1:50,], local[51:100,])
local <- apply(local,2,median)
Result3[1,1] <- local[1]
Result3[1,2] <- local[4]
Result3[1,3] <- local[3]
Result3[1,4] <- local[6]
local <- output_metrique_train_cm
local <- cbind(local[1:50,], local[51:100,])
local <- apply(local,2,median)
Result3[2,1] <- local[1]
Result3[2,2] <- local[4]
Result3[2,3] <- local[3]
Result3[2,4] <- local[6]

stargazer(Result3)

Result4 <- matrix(NA,2,4)

rownames(Result4) <- c("Test","Entrainement")

colnames(Result4) <- c("Poisson MSE", "RF MSE","Poisson Spearman","RF Spearman")
local <- output_metrique_test_freq
local <- cbind(local[1:50,], local[51:100,])
local <- apply(local,2,median)
Result4[1,1] <- local[1]
Result4[1,2] <- local[4]
Result4[1,3] <- local[3]
Result4[1,4] <- local[6]
local <- output_metrique_train_freq
local <- cbind(local[1:50,], local[51:100,])
local <- apply(local,2,median)
Result4[2,1] <- local[1]
Result4[2,2] <- local[4]
Result4[2,3] <- local[3]
Result4[2,4] <- local[6]

stargazer(Result4)


#####################################
#############Multiplot###############
#####################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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