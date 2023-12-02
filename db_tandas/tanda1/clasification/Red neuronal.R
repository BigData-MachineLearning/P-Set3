#1.Paquetes--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
install.packages("tidyverse")
install.packages("pacman")
install.packages("osmdata")
install.packages("httr2")
install.packages("sf")
install.packages("nngeo")
install.packages("spdep")
install.packages("missForest")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("Metrics")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("cowplot")
library(cowplot)
library(dplyr)
library(ggplot2)
library(Metrics)
library(caret)
library(tidyverse)
library(missForest)
library(spdep)
library(nngeo)
library(sf)
library(httr2)
library(osmdata)
library(pacman)
require(pacman)
p_load(here,knitr,tidyverse,ggthemes,fontawesome,kableExtra)
p_load(tidyverse,rio,viridis,sf, leaflet, tmaptools)
p_load(tidyverse, fastDummies, caret, glmnet, MLmetrics)

#Elastic net
# Model Building : Elastic Net Regression
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = TRUE)
set.seed(12345)
en <- train(price~.,
            data=train_ols,
            method='glmnet',
            tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                  lambda = seq(0.0001,0.2,length=5)),
            trControl=custom,
            preProcess = c("center", "scale")
)
#-------1 Resultados 
en
mean(en$resample$RMSE) 0.5141376
mean(en$results$MAE) 0.4131547
mean(en$results$Rsquared) 0.4233886

alpha      lambda    RMSE       Rsquared   MAE      
0.0000000  0.000100  0.5148004  0.4453253  0.3946089
Fitting alpha = 1, lambda = 1e-04 on full training set

#--------2 Ploting EN
plot(en, main = "Elastic Net Regression")
#plotting important variables
plot(varImp(en,scale=TRUE))

#--------3 Resultados en test 
y_en_test<-predict(en,newdata=test_rf)
RMSE1<-rmse(log(test_rf$price), y_en_test)
MAE1<-mae(log(test_rf$price), y_en_test)
RSQUARE(log(test_rf$price), y_en_test)