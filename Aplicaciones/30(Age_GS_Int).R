# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Codes Final")

getwd()


####### Some package could be useful##########

library(naniar)
library(readr)
library(ISLR)
library(formattable)
install.packages("ggplot2", dep=T)
library(ggplot2)


#Read#Reading data into R
library(readr)
Ensayocovid <- read_delim("/Users/ycd455/Desktop/University Camila 2024/Summer 2024/Proyecto de Maestria/Codes CrossValidation/20 Porcentage Missing/Ensayo 5.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(Ensayocovid)
dim(Ensayocovid)
summary(Ensayocovid)



#### Primero debemos remover las dos primeras columnas, las cuales no hacen parte de nuestras variables#######

Ensayo_1=subset(Ensayocovid, select = -c(medical_record,record_id))
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


Ensayocovid2=subset(Ensayocovid1, select = -c(icu_care,estado_geral_c))
View(Ensayocovid2)
dim(Ensayocovid2)
summary(Ensayocovid2)



# Load necessary packages
library(dplyr)

# Set a seed for reproducibility (optional)
set.seed(321)

# Specify the proportion of missing values you want
prop_missingage <- 0.30
prop_missingestado <- 0.30  
prop_missingintubation <- 0.30

# Add missing values to some variables
Ensayocovid2$age[sample(nrow(Ensayocovid2), prop_missingage * nrow(Ensayocovid2))] <- NA
Ensayocovid2$estado_geral[sample(nrow(Ensayocovid2), prop_missingestado * nrow(Ensayocovid2))] <- NA
Ensayocovid2$intubation[sample(nrow(Ensayocovid2), prop_missingintubation * nrow(Ensayocovid2))] <- NA


View(Ensayocovid2)
dim(Ensayocovid2)
summary(Ensayocovid2)

sapply(Ensayocovid2,function(x) sum(is.na(x)))
sapply(Ensayocovid2, function(x) length(unique(x)))


vis_miss(Ensayocovid2) 

#########################################
##Logistic Regression Missing Values
#########################################

table(Ensayocovid2$death)
library(plyr)
library(caret)
set.seed(123)
trainCovid_miss <- createDataPartition(Ensayocovid2$death, p=.80, list=FALSE,times=1)
head(trainCovid_miss)
trainingCovid_miss <- Ensayocovid2[trainCovid_miss, ]
testingCovid_miss <- Ensayocovid2[-trainCovid_miss, ]

table(trainingCovid_miss$death)



##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid_miss)

str(Ensayocovid2)
set.seed(321)
mylogitcovid_miss <- glm(death ~., data=trainingCovid_miss, family = "binomial")
summary(mylogitcovid_miss)


library(pROC)

test_prob_miss = predict(mylogitcovid_miss, newdata = testingCovid_miss, type = "response")
test_roc_miss = roc(response = testingCovid_miss$death, predictor = test_prob_miss)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p_miss<- plot(1 - test_roc_miss$specificities, test_roc_miss$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc_miss)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc_miss), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc_miss)



###################################################################################################################################
######################## REGRESION LOGISTICA SUBCONJUNTOS PREDICTIVE MEAN MATCHING ###############################################
##################################################################################################################################
View(Ensayocovid2)

View(Data1)

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

table(trainingCovid1$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid1)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid1)

str(NewDataset)
set.seed(321)
mylogitcovid1 <- glm(death ~., data=trainingCovid1, family = "binomial")
summary(mylogitcovid1)


library(pROC)

test_prob1 = predict(mylogitcovid1, newdata = testingCovid1, type = "response")
test_roc1 = roc(response = testingCovid1$death, predictor = test_prob1)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p1<- plot(1 - test_roc1$specificities, test_roc1$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc1)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc1), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc1)




###################################################################################################################################
###################################################################################################################################
######################## REGRESION LOGISTICA SUBCONJUNTOS BAYESIAN BOOSTRAP PREDICTIVE MEAN MATCHING ##############################
##################################################################################################################################

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
trainCovid6 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid6)
trainingCovid6 <- NewDataset[trainCovid6, ]
testingCovid6 <- NewDataset[-trainCovid6, ]

table(trainingCovid6$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid6)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid6)

str(NewDataset)
set.seed(321)
mylogitcovid6 <- glm(death ~., data=trainingCovid6, family = "binomial")
summary(mylogitcovid6)


library(pROC)

test_prob6 = predict(mylogitcovid6, newdata = testingCovid6, type = "response")
test_roc6 = roc(response = testingCovid6$death, predictor = test_prob6)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p6<- plot(1 - test_roc6$specificities, test_roc6$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc6)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc6), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc6)



###################################################################################################################################
###################################################################################################################################
######################## REGRESION LOGISTICA SUBCONJUNTOS EXPECTATION MAXIMIZATION ###############################################
##################################################################################################################################
View(Ensayocovid2)

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
trainCovid8 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid8)
trainingCovid8 <- NewDataset[trainCovid8, ]
testingCovid8 <- NewDataset[-trainCovid8, ]

table(trainingCovid8$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid8)

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid8)

str(NewDataset)
set.seed(321)
mylogitcovid8 <- glm(death ~., data=trainingCovid8, family = "binomial")
summary(mylogitcovid8)


library(pROC)

test_prob8 = predict(mylogitcovid8, newdata = testingCovid8, type = "response")
test_roc8 = roc(response = testingCovid8$death, predictor = test_prob8)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p8<- plot(1 - test_roc8$specificities, test_roc8$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc8)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc8), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc8)




###############################################################################################################################################
############################################ GRAPH ROC CURVES TOGETHER #######################################################################
##############################################################################################################################################


# Plotting the ROC curves
plot(1 - test_roc_miss$specificities, test_roc_miss$sensitivities, type = "l", col = "black",
     xlab = "1 - Especificidad", ylab = "Sensibilidad",
     main = "",
     cex.axis = 1.3, cex.lab = 1.3, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add the second ROC curve (p1)
lines(1 - test_roc1$specificities, test_roc1$sensitivities, type = "l", col = "#d35967", lwd = 2)

# Add the third ROC curve (p6)
lines(1 - test_roc6$specificities, test_roc6$sensitivities, type = "l", col = "#0072B2", lwd = 2)

# Add the fourth ROC curve (p8)
lines(1 - test_roc8$specificities, test_roc8$sensitivities, type = "l", col = "#009E73", lwd = 2)

# Adding a legend with AUC values
legend("bottomright", legend = c(paste("Faltantes =", round(auc(test_roc_miss), 2)),
                                 paste("PMM =", round(auc(test_roc1), 2)),
                                 paste("BPMM =", round(auc(test_roc6), 2)),
                                 paste("EM =", round(auc(test_roc8), 2))),
       col = c("black", "#d35967", "#0072B2", "#009E73"), lwd = 2, cex = 0.8)

# Adding diagonal line for reference
abline(0, 1, col = "grey", lwd = 0.5)
