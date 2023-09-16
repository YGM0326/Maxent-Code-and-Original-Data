##################################The correlation heatmap of the environmental variables retained in the model predicting the suitability of wild boar habitats and risk of Damage

################Calculate the correlation between variables in ArcMAP10.6 using SDMtoolbox 2.0 toolbox (Brown 2014), and extract the correlation coefficient matrix between variables
install.packages("colorRampPalette")
library(tidyverse)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(grDevices)
library(RColorBrewer)
######Wild boar habitat
setwd("C:/Users/10284/Desktop/maxent/var/COR/Hcor")
a<-read.csv("h_cor.csv",row.names=1)
c<-as.matrix(a)
corrplot(c,method = "pie",type = 'upper',
         tl.pos="lt", tl.cex=1, tl.col="black",
         insig="label_sig",
         pch.cex = 0.8,pch.col = "black",order="hclust",col = brewer.pal(n = 10, name = 'Set3'), family = "Times New Roman")
corrplot(c, method = "number",
         type="lower",add=TRUE,
         tl.pos = "n",diag=FALSE,
         number.digits = 2,cl.pos="n",number.cex = 0.8,number.font = NULL, col = "black")


######Wild boar damage
setwd("C:/Users/10284/Desktop/maxent/var/COR/Dcor")
a<-read.csv("d_cor.csv",row.names=1)
c<-as.matrix(a)
corrplot(c,method = "pie",type = 'upper',
         tl.pos="lt", tl.cex=1, tl.col="black",
         insig="label_sig",
         pch.cex = 0.8,pch.col = "black",order="hclust",col = brewer.pal(n = 10, name = 'Set3'), family = "Times New Roman")
corrplot(c, method = "number",
         type="lower",add=TRUE,
         tl.pos = "n",diag=FALSE,
         number.digits = 2,cl.pos="n",number.cex = 0.8,number.font = NULL, col = "black")






################################################################Selection of optimal parameters and plotting for the Maxent model.
install.packages("rJava")
install.packages("ENMeval")
install.packages("raster")
install.packages('ecospat')
install.packages('colorspace')
library(rJava)
library(ENMeval)
library(raster)
library(ecospat)
library(colorspace)
library(devtools)
##############################################################Wild boar habitat
setwd('C:/Users/10284/Desktop/maxent')
system.file("java", package="dismo")

env.files <- list.files(path ="C:/Users/10284/Desktop/maxent/var/H/Habtat_remove_var", pattern = ".asc", full.names = TRUE)
env.files <- stack(env.files)
A<- read.csv("C:/Users/10284/Desktop/maxent/F_H/F/SR_Foraging/F/Habitat_point.csv",header = TRUE)
A
occ_ <- A[,-1]
occ_
length(which(!is.na(values(subset(env.files, 1)))))
env.files
enmeval_results <- ENMevaluate(occ_, env.files, partitions = "randomkfold", partition.settings = list(kfolds = 10), n.bg=20000,RMvalues = seq(0.5, 4, 0.5), fc = c("L", "LQ", "LQP"),algorithm='maxent.jar')

write.csv(enmeval_results@results, "habitat_enmeval_results.csv")#######Save the output results as a CSV file
enmeval_results@results
aicmods <- which(enmeval_results@results$AICc == min(na.omit(enmeval_results@results$AICc)))
enmeval_results@results[aicmods,]

enmeval_results@results  ### Render the output results in TIFF format.
tiff(filename = "C:/Users/10284/Desktop/maxent/habitat_AICc vs rm and fc.tiff",   
     width = 6, height = 4, unit = "in", res = 600,bg = "white") #######C:/Users/10284/Desktop/maxent is the path to save the images, and habitat_AICc vs rm and fc.tiff is the name given to the completed image
evalplot.stats(enmeval_results , stats = "AICc", x.var = "rm", color.var = "fc", error.bars = T)
dev.off()#######Complete the image rendering.

pdf(file = "C:/Users/10284/Desktop/maxent/AICc vs rm and fc.pdf",
    width = 6, height = 4)
evalplot.stats(enmeval_results , stats = "AICc", x.var = "rm",
               color.var = "fc", error.bars = TRUE)
dev.off()####Output as a PDF.

################################################################Wild boar damage
env.files <- list.files(path ="C:/Users/10284/Desktop/maxent/var/F/Damage_remove_var", pattern = ".asc", full.names = TRUE)
env.files <- stack(env.files)
A<- read.csv("C:/Users/10284/Desktop/maxent/F_H/F/SR_Foraging/F/Damage_point.csv",header = TRUE)
A
occ_ <- A[,-1]
occ_
length(which(!is.na(values(subset(env.files, 1)))))
env.files
enmeval_results <- ENMevaluate(occ_, env.files, partitions = "randomkfold", partition.settings = list(kfolds = 10), n.bg=20000,RMvalues = seq(0.5, 4, 0.5), fc = c("L", "LQ", "LQP"),algorithm='maxent.jar')

write.csv(enmeval_results@results, "Damage_enmeval_results.csv")
enmeval_results@results
aicmods <- which(enmeval_results@results$AICc == min(na.omit(enmeval_results@results$AICc)))
enmeval_results@results[aicmods,]

enmeval_results@results##Render the output results in TIFF format.
tiff(filename = "C:/Users/10284/Desktop/maxent/Damage_AICc vs rm and fc.tiff",
     width = 6, height = 4, unit = "in", res = 600,bg = "white") 
evalplot.stats(enmeval_results , stats = "AICc", x.var = "rm", color.var = "fc", error.bars = T)
dev.off()


pdf(file = "C:/Users/10284/Desktop/maxent/AICc vs rm and fc.pdf",
    width = 6, height = 4)
evalplot.stats(enmeval_results , stats = "AICc", x.var = "rm",
               color.var = "fc", error.bars = TRUE)
dev.off()






#######################################Construct a single-scale multivariate model of wild boar habitat and damage
setwd('C:/Users/10284/Desktop/maxent') 
system.file("java", package="dismo")
A<- read.csv("C:/Users/10284/Desktop/maxent/D_H/D_Biasfile/habitat_point.csv",header = TRUE) ## Read species occurrence data
susocc<-A[,-1]    #### Remove the first column of species data.
susocc  ####View species data. 

#########################################################Modeling the suitability of wild boar habitats on a 500m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/500_var",
                       pattern = "asc", full.names = T)) #######Read environmental data
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)    #####Construct the model
sus@results    ####Review the results.
write.csv(sus@results, "result_H500.csv")#####Save the results, in the file below the set path.
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    ####Select the best model where delta.AICc == 0.
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   ####Make predictions with the model. 
plot(pred)  ######Plot. 
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8]  ####Optimize the average test AUC of the Maxent model. 
AUC ###Review the AUC value.

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)#####Calculate variable importance.
var.imp   
write.csv(var.imp, "H500_var.imp.csv")   ####Save the results

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),###Calculate the model's TSS value. 
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)####Obtain indices like kappa, TSS etc of the model.

########################################Modeling the suitability of wild boar habitats on a 1000m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/1000_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)  

sus@results    
write.csv(sus@results, "result_H1000.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))  
plot(pred)  
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8] 
AUC 

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp
write.csv(var.imp, "H1000_var.imp.csv") 

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

#####################################################Modeling the suitability of wild boar habitats on a 1500m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/1500_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)   

sus@results    
write.csv(sus@results, "result_H1500.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   
plot(pred) 
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8]  
AUC 

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H1500_var.imp.csv")   

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

###############################################Modeling the suitability of wild boar habitats on a 2000m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/2000_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)   

sus@results    
write.csv(sus@results, "result_H2000.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   
plot(pred) 
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8] 
AUC

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H2000_var.imp.csv")   

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

###################################################Modeling the suitability of wild boar habitats on a2 500m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/2500_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)   

sus@results    
write.csv(sus@results, "result_H2500.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   
plot(pred)  
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8]  
AUC

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H2500_var.imp.csv")  

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

##########################################Modeling the suitability of wild boar habitats on a 3000m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/3000_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)  

sus@results    
write.csv(sus@results, "result_H3000.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))  
plot(pred)  
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8]  
AUC 

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H3000_var.imp.csv")   

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

#######################################Modeling the suitability of wild boar habitats on a 3500m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/3500_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)    

sus@results  
write.csv(sus@results, "result_H3500.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]   
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   
plot(pred)  
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8] 
AUC 
var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H3500_var.imp.csv")   

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)

#############################################Modeling the suitability of wild boar habitats on a 4000m spatial scale
env<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/4000_var",
                       pattern = "asc", full.names = T)) 
sus <- ENMevaluate(occ = susocc, 
                   envs = env, 
                   tune.args = list(fc = c("LQP"), rm = 1),
                   partitions = "randomkfold",
                   partition.settings = list(kfolds = 10),n.bg=20000,
                   doClamp = T,
                   algorithm = "maxent.jar", 
                   bin.output = FALSE,
                   parallel = TRUE)   

sus@results   
write.csv(sus@results, "result_H4000.csv")
optmodel <- sus@models[[which(sus@results$delta.AICc == 0)]]    
pred <- predict(sus@models[[which(sus@results$delta.AICc == 0)]],
                env,  args = c("outputformat=logistic"))   
plot(pred)  
writeRaster(pred, filename=file.path( "pre.tif"), Format = 'GTiff',overwrite=TRUE)
AUC <- sus@results[which(sus@results$delta.AICc == 0), 8]  
AUC 

var.imp <- ecospat.maxentvarimport(optmodel, dfvar = optmodel@absence, nperm = 5)
var.imp   
write.csv(var.imp, "H4000_var.imp.csv")  

e <- dismo::evaluate(p = susocc, a = randomPoints(env[[1]],20000),
                     model = optmodel, x = env)

TSS <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
max(e@TPR+e@TNR-1)


#######################Multi-scale multivariate model of the suitability of wild boar habitats
envmuilt<- stack(list.files(path = "C:/Users/10284/Desktop/maxent/var/H/remove_H",
                            pattern = "asc", full.names = T))
susmuilt <- ENMevaluate(occ = susocc, 
                        envs = envmuilt, 
                        tune.args = list(fc = c("LQP"), rm = 0.5),
                        partitions = "randomkfold",
                        partition.settings = list(kfolds = 10),n.bg=20000,
                        doClamp = T,
                        algorithm = "maxent.jar", 
                        bin.output = FALSE,
                        parallel = TRUE)    
susmuilt@results   
write.csv(susmuilt@results, "h_muilt.csv")
optmodelmuilt <- susmuilt@models[[which(susmuilt@results$delta.AICc == 0)]]  
predmuilt <- predict(susmuilt@models[[which(susmuilt@results$delta.AICc == 0)]],
                     envmuilt,  args = c("outputformat=logistic")) 
plot(predmuilt)  

writeRaster(predmuilt, filename=file.path( "premuilt.tif"), Format = 'GTiff',overwrite=TRUE)


AUCmuilt<- susmuilt@results[which(susmuilt@results$delta.AICc == 0), 8] 
AUCmuilt 

var.imp <- ecospat.maxentvarimport(optmodelmuilt, dfvar = optmodelmuilt@absence, nperm = 5)
write.csv(var.imp, "habitatmulti_var.impmuilt.csv")   

e <- dismo::evaluate(p = susocc, a = randomPoints(envmuilt[[1]],20000),
                     model = optmodelmuilt, x = envmuilt)  
e@kappa
TSSmuilt <- cbind(e@t, e@TNR, e@TPR, e@TPR+e@TNR-1)
TSSmuilt
max(e@TPR+e@TNR-1)

#####################The construction of the single scale multivariate model of wild boar damage risk (500m, 1000m, 1500m, 2000m, 2500m, 3000m, 3500m, 4000m) and the multi-scale multivariate model is the same as the above modeling methods.





#######################################################################Different optimization methods (GK/SR) optimize the calculation of Kappa and TSS of the model at different spatial scales.
#######The calculation method of Kappa and TSS for a single optimization model is as follows, the calculation method for other optimization models can be repeated by following these steps (taking the multi-scale model of wild boar habitat as an example)
setwd("C:/Users/10284/Desktop/maxent/result/result_H/youhua/multi")  
list.files()
backgroundpredictions <- read.csv("Sus_0_backgroundPredictions.csv")   #####Extract the "xxx_backgroundPredictions.csv" file generated by the maxent model.
x <- length(backgroundpredictions)
backgroundclog <- backgroundpredictions[,x]
samplepredictions <- read.csv("Sus_0_samplePredictions.csv")#####Extract the "xxx_samplePredictions.csv" file generated by the maxent model. 
x2 <- length(samplepredictions)
sampleclog <- samplepredictions[,x2]
n <- 20000
th <- 0.2998   ###########Extract the "xxx_samplePredictions.csv" file generated by the maxent model. 
TSS_calculations <- function (sample_clog, prediction_clog, n, th) {
  
  xx <- sum(sample_clog > th)
  yy <- sum(prediction_clog > th)
  xxx <- sum(sample_clog < th)
  yyy <- sum(prediction_clog < th)
  
  ncount <- sum(xx,yy,xxx,yyy)
  
  overallaccuracy <- (xx + yyy)/ncount 
  sensitivity <- xx / (xx + xxx)
  specificity <- yyy / (yy + yyy)
  tss <- sensitivity + specificity - 1
  
  #kappa ???? 
  a <- xx + xxx
  b <- xx + yy
  c <- yy + yyy
  d <- xxx + yyy
  e <- a * b
  f <- c * d
  g <- e + f
  h <- g / (ncount * ncount)
  hup <- overallaccuracy - h
  hdown <- 1 - h
  
  kappa <- hup/hdown
  Po <- (xx + yyy) / ncount
  Pe <- ((b/ncount) * (a/ncount)) + ((d/ncount) * (c/ncount))
  Px1 <- Po - Pe
  Px2 <- 1 - Pe
  Px3 <- Px1/Px2
  
  tx1 <- xx + yyy
  tx2 <- 2 * a * c
  tx3 <- a - c
  tx4 <- xx - yyy
  tx5 <- ncount * (( 2 * a ) - tx4)
  tx6 <- ncount * tx1
  
  kappamax <- (tx6 - tx2 - (tx3 * tx4)) / ((tx5 - tx3) - (tx3 * tx4))
  
  cat(" Maxent results for model with\n",a,"test sample predictions\n",c ,"background predicitons\n\n TSS value:        ", tss,"\n Overall accuracy: ",overallaccuracy,"\n Sensitivity:      ",sensitivity,"\n Specificity:      ",specificity,"\n Kappa:            ",kappa,"\n Kappa max:        ",kappamax)
  
  
}
TSS_calculations(sampleclog,backgroundclog,n,th)  





#########################################################Calculate the Schoener's D index and the Pearson rank correlation between the suitability of the wild boar's habitat and the risk model of damage.
install.packages("ENMTools")
library(ENMTools)
library(raster)
setwd("C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif")
tif_file_name1 <- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Hsr.asc)"    ####Calculate the Schoener's D index and the Pearson rank correlation between the suitability of the wild boar's habitat and the risk model of damage.

tif_file1 <- raster(tif_file_name1)
plot(tif_file1)   #
tif_file_name2 <- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Dsr.asc)"####Calculate the Schoener's D index and the Pearson rank correlation between the suitability of the wild boar's habitat and the risk model of damage.


tif_file2 <- raster(tif_file_name2)
raster.overlap(tif_file1, tif_file2, verbose = FALSE)####Calculate the Schoener's D index and the Pearson rank correlation index.


tif_file_name3 <- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Hgk.asc)"    ####Read the predictive layer of the final model of the wild boar's habitat suitability after building sample bias (GK) optimization. 

tif_file3 <- raster(tif_file_name3)
plot(tif_file3)   #
tif_file_name4 <- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Dgk.asc)"####Read the predictive layer of the final model of the wild boar's habitat suitability after building sample bias (GK) optimization. 


tif_file4 <- raster(tif_file_name4)
raster.overlap(tif_file3, tif_file4, verbose = FALSE)



tif_file_name5 <- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Hgk.asc)"    ####Read the predictive layer of the unoptimized multi-scale model of the wild boar's habitat suitability.

tif_file5 <- raster(tif_file_name5)
plot(tif_file5)   #
tif_file_name6<- r"(C:/Users/10284/Desktop/maxent/ENMTools/ENMTools/ENMTools_1.3/overlap/tif/Dgk.asc)"#### Read the predictive layer of the unoptimized multi-scale damage risk model for wild boars
tif_file6 <- raster(tif_file_name6)
raster.overlap(tif_file5, tif_file6, verbose = FALSE)