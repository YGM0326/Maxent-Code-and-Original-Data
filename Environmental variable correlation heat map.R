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
