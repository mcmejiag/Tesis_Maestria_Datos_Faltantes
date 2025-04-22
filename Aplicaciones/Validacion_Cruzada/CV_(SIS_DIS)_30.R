# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Codes Final")

getwd()



library(naniar)
library(readr)
library(ISLR)
library(formattable)
install.packages("ggplot2", dep=T)
library(ggplot2)


#Read#Reading data into R

library(readr)
Ensayocovid <- read_delim("Ensayo 5.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ensayocovid)
dim(Ensayocovid)
summary(Ensayocovid)



#### Primero debemos remover las dos primeras columnas, las cuales no hacen parte de nuestras variables#######

Ensayo_1=subset(Ensayocovid, select = -c(medical_record))
head(Ensayo_1)
str(Ensayo_1)
View(Ensayo_1)

Ensayocovid_1 <- Ensayo_1[-(25:95),]
Ensayocovid_2 <- Ensayocovid_1[-(115:185),]
Ensayocovid_3 <- Ensayocovid_2[-(940:985),]
Ensayocovid_4 <- Ensayocovid_3[-(720:845),]
Ensayocovid_5 <- Ensayocovid_4[-(201:278),]
Ensayocovid_6 <- Ensayocovid_5[-(602:648),]
Ensayocovid1 <- Ensayocovid_6[-(125:193),]

View(Ensayocovid1)
dim(Ensayocovid1)

### Como podemos observar tenemos 5 variables chr, debemos convertirlas a factor#####
### intubation, icu_care,estado_general,sex,death,estado_geral_c#########

Ensayocovid1$intubation <- factor(Ensayocovid1$intubation)
Ensayocovid1$icu_care <- factor(Ensayocovid1$icu_care)
Ensayocovid1$estado_geral <- factor(Ensayocovid1$estado_geral)
Ensayocovid1$sex <- factor(Ensayocovid1$sex)
Ensayocovid1$death <- factor(Ensayocovid1$death)
Ensayocovid1$estado_geral_c<- factor(Ensayocovid1$estado_geral_c)



## Checking the levels and checking the dummy coeding#######
######## Intubation nao=0, sim=1###
levels(Ensayocovid1$intubation)
is.factor(Ensayocovid1$intubation)
contrasts(Ensayocovid1$intubation)
######## icu_care solo tiene sim###
levels(Ensayocovid1$icu_care)
is.factor(Ensayocovid1$icu_care)
######### sex masculino=1 femenino=0 ######
levels(Ensayocovid1$sex)
is.factor(Ensayocovid1$sex)
contrasts(Ensayocovid1$sex)
######### death sim=1 nao=0 ######
levels(Ensayocovid1$death)
is.factor(Ensayocovid1$death)
contrasts(Ensayocovid1$death)

######## estado_general BEG,MEG,REG ###
levels(Ensayocovid1$estado_geral)
is.factor(Ensayocovid1$estado_geral)
contrasts(Ensayocovid1$estado_geral)


Ensayocovid2=subset(Ensayocovid1, select = -c(icu_care,estado_geral_c,record_id))
View(Ensayocovid2)
dim(Ensayocovid2)
summary(Ensayocovid2)


# Load necessary packages
library(dplyr)

# Set a seed for reproducibility (optional)
set.seed(321)



# Specify the proportion of missing values you want
prop_missingintubation <- 0.064  #6.04% missing values
prop_missingsaps3 <- 0.3843  # 38.43% missing values
prop_missingsao <- 0.0023  # 0.23% missing values
prop_missingicu <- 0.3047  # 30.47% missing values
prop_missingestado <- 0.3133  # 31.33% missing values
prop_missingsystolic <- 0.0184  # 1.84% missing values
prop_missingdiastolic <- 0.0181 # 1.81% missing values


# Add missing values to some variables
Ensayocovid2$intubation[sample(nrow(Ensayocovid2), prop_missingintubation * nrow(Ensayocovid2))] <- NA
Ensayocovid2$saps3[sample(nrow(Ensayocovid2), prop_missingsaps3 * nrow(Ensayocovid2))] <- NA
Ensayocovid2$sao2_admission[sample(nrow(Ensayocovid2), prop_missingsao * nrow(Ensayocovid2))] <- NA
Ensayocovid2$icu_los_2[sample(nrow(Ensayocovid2), prop_missingicu * nrow(Ensayocovid2))] <- NA
Ensayocovid2$estado_geral[sample(nrow(Ensayocovid2), prop_missingestado * nrow(Ensayocovid2))] <- NA
Ensayocovid2$systolic_bp_admission[sample(nrow(Ensayocovid2), prop_missingsystolic * nrow(Ensayocovid2))] <- NA
Ensayocovid2$diastolic_bp_admission[sample(nrow(Ensayocovid2), prop_missingdiastolic * nrow(Ensayocovid2))] <- NA
# Print the modified data frame
View(Ensayocovid2)
dim(Ensayocovid2)
summary(Ensayocovid2)

sapply(Ensayocovid2,function(x) sum(is.na(x)))
sapply(Ensayocovid2, function(x) length(unique(x)))


vis_miss(Ensayocovid2) 
gg_miss_var(Ensayocovid2) +
  labs(x = "Variables", y = "Missing Values", title = "Generated Missing Values")


################################################################################################################
###############################################################################################################
########################### Validacion Cruzada PMM ################################################################

library(mice)
tempData <- mice(Ensayocovid2,m=4,maxit=50,meth='pmm',seed=500)
summary(tempData)

Data1 <- complete(tempData,1)
summary(Data1)
View(Data1)

Data2 <- complete(tempData,2)
summary(Data2)
View(Data2)

Data3 <- complete(tempData,3)
summary(Data3)


Data4 <- complete(tempData,4)
summary(Data4)
dim(Data4)

# First, let's create a copy of the original dataset to modify
NewDataset <- Ensayocovid2
View(NewDataset)

# Replace the first 200 rows with Data1
NewDataset[1:200, ] <- Data1[1:200, ]

# Replace the next 200 rows (201 to 400) with Data2
NewDataset[201:400, ] <- Data2[201:400, ]

# Replace the next 200 rows (401 to 600) with Data3
NewDataset[401:600, ] <- Data3[401:600, ]

# Finally, replace the last 200 rows (601 to 800) with Data4
NewDataset[601:800, ] <- Data4[601:800, ]

View(NewDataset)

table(NewDataset$death)
library(plyr)
library(caret)
set.seed(123)
trainCovid1 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid1)
trainingCovid1 <- NewDataset[trainCovid1, ]
testingCovid1 <- NewDataset[-trainCovid1, ]

table(NewDataset$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid1)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

set.seed(123)
# Perform cross-validation
ctrl1 <- trainControl(method = "cv",  # Use cross-validation
                      number = 10,
                      savePredictions="all",
                      classProbs = TRUE)   
# Fit your model using cross-validation
model1 <- train(death ~ .,                  
                data = trainingCovid1,                
                method = "glm", 
                family=binomial, 
                trControl = ctrl1)          

# Print the model results
print(model1)
summary(model1)


library(pROC)

pred_prob1 <- predict(model1, newdata = testingCovid1, type = "prob")
roc_obj1 <- roc(response = testingCovid1$death, predictor = pred_prob1[, "sim"])

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p1<- plot(1 - roc_obj1$specificities, roc_obj1$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(roc_obj1)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj1), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(roc_obj1)

## AUC= 0.8276
## Variance=0.001085965



################################################################################################################
###############################################################################################################
########################### Validacion Cruzada BBPMM ################################################################

library(mice)
library(MASS)
library(BaBooN)

Ensayocovid2$intubation <- as.numeric(Ensayocovid2$intubation)
Ensayocovid2$estado_geral<-as.numeric(Ensayocovid2$estado_geral)
Ensayocovid2$sex<-as.numeric(Ensayocovid2$sex)
str(Ensayocovid2)

imputation.data<-BBPMM(Ensayocovid2,nIter=4)
imputation<-imputation.data$impdata
summary(imputation)


### Now from the imputation (1) I want to use the subset 1###

Data1<- complete(imputation[[1]])
summary(Data1)
dim(Data1)

## Now from the second imputation I want to use subset 2####
Data2<- complete(imputation[[2]])
summary(Data2)
dim(Data2)

## Now from the third imputation I want to use subset 3 ####

Data3<- complete(imputation[[3]])
summary(Data3)
dim(Data3)

### Now from the four imputation I want to use subset 4######

Data4<- complete(imputation[[4]])
summary(Data4)
dim(Data4)

# First, let's create a copy of the original dataset to modify
NewDataset <- Ensayocovid2
View(NewDataset)

# Replace the first 200 rows with Data1
NewDataset[1:200, ] <- Data1[1:200, ]

# Replace the next 200 rows (201 to 400) with Data2
NewDataset[201:400, ] <- Data2[201:400, ]

# Replace the next 200 rows (401 to 600) with Data3
NewDataset[401:600, ] <- Data3[401:600, ]

# Finally, replace the last 200 rows (601 to 800) with Data4
NewDataset[601:800, ] <- Data4[601:800, ]

View(NewDataset)

table(NewDataset$death)
library(plyr)
library(caret)
set.seed(123)
trainCovid2 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid2)
trainingCovid2 <- NewDataset[trainCovid2, ]
testingCovid2 <- NewDataset[-trainCovid2, ]

table(trainingCovid2$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid2)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

set.seed(123)
# Perform cross-validation
ctrl2 <- trainControl(method = "cv",  # Use cross-validation
                      number = 10,
                      savePredictions="all",
                      classProbs = TRUE)   
# Fit your model using cross-validation
model2 <- train(death ~ .,                  
                data = trainingCovid2,                
                method = "glm", 
                family=binomial, 
                trControl = ctrl2)          

# Print the model results
print(model2)
summary(model2)


library(pROC)

pred_prob2 <- predict(model2, newdata = testingCovid2, type = "prob")
roc_obj2 <- roc(response = testingCovid2$death, predictor = pred_prob2[, "sim"])

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p2<- plot(1 - roc_obj2$specificities, roc_obj2$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(roc_obj2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj2), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(roc_obj2)

## AUC= 0.80
## Variance=0.00117592


################################################################################################################
###############################################################################################################
########################### Validacion Cruzada EMM ################################################################

Ensayocovid2$intubation <- as.numeric(Ensayocovid2$intubation)
Ensayocovid2$estado_geral<-as.numeric(Ensayocovid2$estado_geral)
Ensayocovid2$sex<-as.numeric(Ensayocovid2$sex)
str(Ensayocovid2)

library(Amelia)
library(missForest)

categorical_vars <- c("death", "sex", "estado_geral", "intubation")

amelia_fit <- amelia(Ensayocovid2, m=4, parallel = "multicore", noms = categorical_vars, p2s=0)
amelia_fit
summary(amelia_fit)


### Now from the imputation (1) I want to use the subset 1###

Data1<- amelia_fit$imputations[[1]]
summary(Data1)
dim(Data1)

## Now from the second imputation I want to use subset 2####
Data2<- amelia_fit$imputations[[2]]
summary(Data2)
dim(Data2)

## Now from the third imputation I want to use subset 3 ####
Data3<- amelia_fit$imputations[[3]]
summary(Data3)
dim(Data3)

### Now from the four imputation I want to use subset 4######

Data4<- amelia_fit$imputations[[4]]
summary(Data4)
dim(Data4)

# First, let's create a copy of the original dataset to modify
NewDataset <- Ensayocovid2
View(NewDataset)

# Replace the first 200 rows with Data1
NewDataset[1:200, ] <- Data1[1:200, ]

# Replace the next 200 rows (201 to 400) with Data2
NewDataset[201:400, ] <- Data2[201:400, ]

# Replace the next 200 rows (401 to 600) with Data3
NewDataset[401:600, ] <- Data3[401:600, ]

# Finally, replace the last 200 rows (601 to 800) with Data4
NewDataset[601:800, ] <- Data4[601:800, ]

View(NewDataset)

table(NewDataset$death)
library(plyr)
library(caret)
set.seed(123)
trainCovid3 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid3)
trainingCovid3 <- NewDataset[trainCovid3, ]
testingCovid3 <- NewDataset[-trainCovid3, ]

table(trainingCovid3$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid3)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)


set.seed(123)
# Perform cross-validation
ctrl3 <- trainControl(method = "cv",  # Use cross-validation
                      number = 4,
                      savePredictions="all",
                      classProbs = TRUE)   
# Fit your model using cross-validation
model3 <- train(death ~ .,                  
                data = trainingCovid3,                
                method = "glm", 
                family=binomial, 
                trControl = ctrl3)          

# Print the model results
print(model3)
summary(model3)


library(pROC)

pred_prob3 <- predict(model3, newdata = testingCovid3, type = "prob")
roc_obj3 <- roc(response = testingCovid3$death, predictor = pred_prob3[, "sim"])

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p3<- plot(1 - roc_obj3$specificities, roc_obj3$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(roc_obj3)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj3), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(roc_obj3)

## AUC= 0.7982
## Variance=0.001239588



# Assuming you have the test_roc objects for p4, p6, p8 already calculated
# Plot the first ROC curve (p4)
plot(1 - roc_obj1$specificities, roc_obj1$sensitivities, type = "l", col = "#d35967",
     xlab = "1 - Especificidad", ylab = "Sensibilidad",
     main = "",
     cex.axis = 1.3, cex.lab = 1.3, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add the second ROC curve (p6)
lines(1 - roc_obj2$specificities, roc_obj2$sensitivities, type = "l", col = "#0072B2", lwd = 2)

# Add the third ROC curve (p8)
lines(1 - roc_obj3$specificities, roc_obj3$sensitivities, type = "l", col = "#009E73", lwd = 2)

# Add legends and AUC values
legend("bottomright", legend = c(paste("AUC PMM   =", round(auc(roc_obj1), 2)),
                                 paste("AUC BPMM.=", round(auc(roc_obj2), 2)),
                                 paste("AUC EM      =", round(auc(roc_obj3), 2))),
       col = c("#d35967", "#0072B2", "#009E73"), lwd = 2)

# Adding diagonal line for reference
abline(0, 1, col = "black", lwd = 0.5)




############################## Regression Logistica sin Datos Faltantes Validacion Cruzada ############################
View(Ensayocovid2)

Ensayocovid3=subset(Ensayocovid1, select = -c(icu_care,estado_geral_c,record_id))
View(Ensayocovid3)



table(Ensayocovid3$death)
library(plyr)
library(caret)
set.seed(123)
trainCovid4 <- createDataPartition(Ensayocovid3$death, p=.80, list=FALSE,times=1)
head(trainCovid4)
trainingCovid4 <- Ensayocovid3[trainCovid4, ]
testingCovid4 <- Ensayocovid3[-trainCovid4, ]

table(Ensayocovid3$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid4)

Ensayocovid3$intubation <- factor(Ensayocovid3$intubation)
Ensayocovid3$estado_geral <- factor(Ensayocovid3$estado_geral)
Ensayocovid3$sex <- factor(Ensayocovid3$sex)
str(Ensayocovid3)

levels(Ensayocovid3$intubation)
levels(Ensayocovid3$estado_geral)
levels(Ensayocovid3$sex)

set.seed(123)
# Perform cross-validation
ctrl4 <- trainControl(method = "cv",  # Use cross-validation
                      number = 10,
                      savePredictions="all",
                      classProbs = TRUE)   
# Fit your model using cross-validation
model4 <- train(death ~ .,                  
                data = trainingCovid4,                
                method = "glm", 
                family=binomial, 
                trControl = ctrl4)          

# Print the model results
print(model4)
summary(model4)


library(pROC)

pred_prob4 <- predict(model4, newdata = testingCovid4, type = "prob")
roc_obj4 <- roc(response = testingCovid4$death, predictor = pred_prob4[, "sim"])

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p4<- plot(1 - roc_obj4$specificities, roc_obj4$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(roc_obj4)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj4), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(roc_obj4)

## AUC= 0.818
## Variance=0.001180257

