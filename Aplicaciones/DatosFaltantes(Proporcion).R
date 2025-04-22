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
Ensayocovid <- read_delim("Ensayo 5.csv", delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE)

View(Ensayocovid)
dim(Ensayocovid)
summary(Ensayocovid)



#### Primero debemos remover las dos primeras columnas, las cuales no hacen parte de nuestras variables#######

Ensayo_1=subset(Ensayocovid, select = -c(record_id,medical_record))
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



###### Training the Data ########
table(Ensayocovid2$death)
library(caret)
set.seed(123)
trainCovid <- createDataPartition(Ensayocovid2$death, p=.80, list=FALSE,times=1)
head(trainCovid)
trainingCovid1 <- Ensayocovid2[trainCovid, ]
testingCovid1 <- Ensayocovid2[-trainCovid, ]

table(trainingCovid1$death)




#########################################
##Logistic Regression sin Insertar Valores Faltantes
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid1)

str(Ensayocovid2)
set.seed(123)
mylogitcovid1 <- glm(death ~., data=trainingCovid1, family = "binomial")
summary(mylogitcovid1)


library(pROC)
test_prob1 = predict(mylogitcovid1, newdata = testingCovid1, type = "response")
test_roc1 = roc(testingCovid1$death ~ test_prob1, plot = TRUE, print.auc = TRUE)

dev.off()

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

######## AUC: 0.82 Variance:0.001180257

#################################################################################################################################################
########################################### CONTINUE CODE #######################################################################################


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




####################################################################################################################################################
#################### Regresion Logistica sin tecnica de imputacion pero con valores faltantes###############################################################################
####################################################################################################################################################

###### Training the Data ########
table(Ensayocovid2$death)
library(caret)
set.seed(123)
trainCovid2 <- createDataPartition(Ensayocovid2$death, p=.80, list=FALSE,times=1)
head(trainCovid2)
trainingCovid2 <- Ensayocovid2[trainCovid2, ]
testingCovid2 <- Ensayocovid2[-trainCovid2, ]

table(trainingCovid2$death)


#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid2)

str(Ensayocovid2)
set.seed(321)
mylogitcovid2 <- glm(death ~., data=trainingCovid2, family = "binomial")
summary(mylogitcovid2)




library(pROC)

test_prob2 = predict(mylogitcovid2, newdata = testingCovid2, type = "response")
test_roc2 = roc(response = testingCovid2$death, predictor = test_prob2)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p2<- plot(1 - test_roc2$specificities, test_roc2$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc2)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc2), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc2)


########## AUC: 0.77. Variance:0.004955057


############################################################################################
######################### Plotting to graphs together#######################################
############################################################################################

library(pROC)

# Assuming test_roc1 and test_roc2 are the results of your ROC analysis for the two models
# First ROC Curve
test_roc1 <- roc(response = testingCovid1$death, predictor = predict(mylogitcovid1, newdata = testingCovid1, type = "response"))
# Second ROC Curve
test_roc2 <- roc(response = testingCovid2$death, predictor = predict(mylogitcovid2, newdata = testingCovid2, type = "response"))

# Plotting the first ROC curve
plot(1 - test_roc1$specificities, test_roc1$sensitivities, type = "l", col = "#d35967",
     xlab = "1 - Especificidad", ylab = "Sensibilidad",
     main = "",
     cex.axis = 1.3, cex.lab = 1.3, lwd = 2)

# Adding the second ROC curve
lines(1 - test_roc2$specificities, test_roc2$sensitivities, type = "l", col = "#0072B2", lwd = 2)

# Add legends and AUC values
legend("bottomright", legend = c(paste("AUC Datos Completos =", round(auc(test_roc1), 2)),
                                 paste("AUC Datos Incompletos =", round(auc(test_roc2), 2))),
       col = c("#d35967", "#0072B2"), lwd = 2)

# Adding diagonal line for reference
abline(0, 1, col = "black", lwd = 0.5)



############################      PREDICTIVE MEAN MATCHING ##################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
############################################################################################## 
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################



##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
## CODIGO DEL PASADO CON MICE SIN CONSIDERAR LOS SUBCONJUTNOS ################################
################### Graficar la Curva ROC de los dos metodos #################################


?mice()
library(mice)
tempData <- mice(Ensayocovid2,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)

summary(completedData)


###### Training the Data ########
table(completedData$death)
library(caret)
set.seed(123)
trainCovid3 <- createDataPartition(completedData$death, p=.80, list=FALSE,times=1)
head(trainCovid3)
trainingCovid3 <- completedData[trainCovid3, ]
testingCovid3 <- completedData[-trainCovid3, ]

table(trainingCovid3$death)


#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid3)

str(completedData)
set.seed(123)
mylogitcovid3 <- glm(death ~., data=trainingCovid3, family = "binomial")
summary(mylogitcovid3)


library(pROC)

test_prob3 = predict(mylogitcovid3, newdata = testingCovid3, type = "response")
test_roc3 = roc(response = testingCovid3$death, predictor = test_prob3)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p3<- plot(1 - test_roc3$specificities, test_roc3$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3, 
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc3)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc3), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc3)

## AUC:0.83 Variance:0.001095592

##################################################################################################################################
########################## IMPUTACION MULTIPLE CON LOS SUBCONJUNTOS ##############################################################
##################################################################################################################################

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
trainCovid4 <- createDataPartition(NewDataset$death, p=.80, list=FALSE,times=1)
head(trainCovid4)
trainingCovid4 <- NewDataset[trainCovid4, ]
testingCovid4 <- NewDataset[-trainCovid4, ]

table(NewDataset$death)


xtabs(~death+intubation+estado_geral+sex, data = trainingCovid4)

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

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid4)

str(NewDataset)
set.seed(321)
mylogitcovid4 <- glm(death ~., data=trainingCovid4, family = "binomial")
summary(mylogitcovid4)


library(pROC)

test_prob4 = predict(mylogitcovid4, newdata = testingCovid4, type = "response")
test_roc4 = roc(response = testingCovid4$death, predictor = test_prob4)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p4<- plot(1 - test_roc4$specificities, test_roc4$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3,
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc4)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc4), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc4)

#write.csv(CombinedData1, "combineddata1.csv", row.names = FALSE)

####### AUC : 0.8276, Variance: 0.001085965


############## BAYESIAN BOOSTRAP PREDICTIVE MEAN MATCHING ###################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################




#############################################################################################
## CODIGO DEL PASADO SIN CONSIDERAR LOS SUBCONJUTNOS ################################

View(Ensayocovid2)
dim(Ensayocovid2)
str(Ensayocovid2)
Ensayocovid2$intubation <- as.numeric(Ensayocovid2$intubation)
Ensayocovid2$estado_geral<-as.numeric(Ensayocovid2$estado_geral)
Ensayocovid2$sex<-as.numeric(Ensayocovid2$sex)
str(Ensayocovid2)

library(mice)
library(MASS)
library(BaBooN)

imputation.data1<-BBPMM(Ensayocovid2,nIter=4)
imputation1<-imputation.data$impdata
summary(imputation1)

completedData<- complete(imputation1[[1]])
summary(completedData)
dim(completedData)


###### Training the Data ########
table(completedData$death)
library(caret)
set.seed(321)
trainCovid5 <- createDataPartition(completedData$death, p=.80, list=FALSE,times=1)
head(trainCovid5)
trainingCovid5 <- completedData[trainCovid5, ]
testingCovid5 <- completedData[-trainCovid5, ]

table(trainingCovid5$death)


#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid5)

str(completedData)
set.seed(123)
mylogitcovid5 <- glm(death ~., data=trainingCovid5, family = "binomial")
summary(mylogitcovid5)


library(pROC)

test_prob5 = predict(mylogitcovid5, newdata = testingCovid5, type = "response")
test_roc5 = roc(response = testingCovid5$death, predictor = test_prob5)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p5<- plot(1 - test_roc5$specificities, test_roc5$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3, 
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc5)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc5), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc5)

######### AUC : 0.82 Variance: 0.00104146

##################################################################################################################################
########################## IMPUTACION MULTIPLE CON LOS SUBCONJUNTOS ##############################################################
##################################################################################################################################


Ensayocovid2$intubation <- as.numeric(Ensayocovid2$intubation)
Ensayocovid2$estado_geral<-as.numeric(Ensayocovid2$estado_geral)
Ensayocovid2$sex<-as.numeric(Ensayocovid2$sex)
str(Ensayocovid2)


imputation.data1<-BBPMM(Ensayocovid2,nIter=4)
imputation1<-imputation.data1$impdata
summary(imputation1)


### Now from the imputation (1) I want to use the subset 1###

Data1<- complete(imputation1[[1]])
summary(Data1)
dim(Data1)

## Now from the second imputation I want to use subset 2####
Data2<- complete(imputation1[[2]])
summary(Data2)
dim(Data2)

## Now from the third imputation I want to use subset 3 ####

Data3<- complete(imputation1[[3]])
summary(Data3)
dim(Data3)

### Now from the four imputation I want to use subset 4######

Data4<- complete(imputation1[[4]])
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

NewDataset$intubation <- factor(NewDataset$intubation)
NewDataset$estado_geral <- factor(NewDataset$estado_geral)
NewDataset$sex <- factor(NewDataset$sex)
str(NewDataset)

levels(NewDataset$intubation)
levels(NewDataset$estado_geral)
levels(NewDataset$sex)

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



#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid6)

str(NewDataset)
str(trainingCovid6)
View(trainingCovid6)
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

#write.csv(CombinedData1, "combineddata1.csv", row.names = FALSE)
## AUC: 0.80 Variance:0.00122769



######################### BOOSTRAP EXPECTATION MAXIMIZATION ###################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################




#############################################################################################
########### CODIGO DEL PASADO SIN CONSIDERAR LOS SUBCONJUTNOS ################################
#############################################################################################

############ create subsets of the data ##################################
library(Amelia)
library(missForest)

categorical_vars <- c("death", "sex", "estado_geral", "intubation")

amelia_fit <- amelia(Ensayocovid2, m=5, parallel = "multicore", noms = categorical_vars, p2s=0)
amelia_fit
summary(amelia_fit)

amelia_fit$imputations[[1]]


imputed_data <- amelia_fit$imputations[[1]]
summary(imputed_data)
dim(imputed_data)


###### Training the Data ########
table(imputed_data$death)
library(caret)
set.seed(123)
trainCovid7 <- createDataPartition(imputed_data$death, p=.80, list=FALSE,times=1)
head(trainCovid7)
trainingCovid7 <- imputed_data[trainCovid7, ]
testingCovid7 <- imputed_data[-trainCovid7, ]

table(imputed_data$death)

#########################################
##Logistic Regression 
#########################################

##Model Fitting Uisng Training Data

xtabs(~death+intubation+estado_geral+sex, data = trainingCovid7)

str(imputed_data)
set.seed(321)
mylogitcovid7 <- glm(death ~., data=trainingCovid7, family = "binomial")
summary(mylogitcovid7)


library(pROC)

test_prob7 = predict(mylogitcovid7, newdata = testingCovid7, type = "response")
test_roc7 = roc(response = testingCovid7$death, predictor = test_prob7)

# Dibujar la curva ROC manualmente y ajustar el eje X para mostrar 1 - especificidad
p7<- plot(1 - test_roc7$specificities, test_roc7$sensitivities, type = "l", col = "#d35967",
          xlab = "1 - Especificidad", ylab = "Sensibilidad",
          main = "",
          cex.axis = 1.3, 
          cex.lab = 1.3,
          lwd=2)

# Agregar el área bajo la curva (AUC) en la gráfica
auc(test_roc7)
legend("bottomright", legend = paste("AUC =", round(auc(test_roc7), 2)), col = "#d35967", lwd = 2)
abline(0, 1, col = "black", lwd = 0.5)
var(test_roc7)

## AUC: 0.80 Variance:0.001243056

##################################################################################################################################
########################## IMPUTACION MULTIPLE CON LOS SUBCONJUNTOS ##############################################################
##################################################################################################################################

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

### AUC: 0.79 Variance:0.001266514



################################################## GRAPHS TOGUETER OF THE ROC CURVE ##############################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

library(pROC)

# Assuming you have the test_roc objects for p3, p5, p7 already calculated
# Plot the first ROC curve (p3)
plot(1 - test_roc3$specificities, test_roc3$sensitivities, type = "l", col = "#d35967",
     xlab = "1 - Especificidad", ylab = "Sensibilidad",
     main = "",
     cex.axis = 1.3, cex.lab = 1.3, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add the second ROC curve (p5)
lines(1 - test_roc5$specificities, test_roc5$sensitivities, type = "l", col = "#0072B2", lwd = 2)

# Add the third ROC curve (p7)
lines(1 - test_roc7$specificities, test_roc7$sensitivities, type = "l", col = "#009E73", lwd = 2)

# Add legends and AUC values
legend("bottomright", legend = c(paste("AUC PMM   =", round(auc(test_roc3), 2)),
                                 paste("AUC BPMM.=", round(auc(test_roc5), 2)),
                                 paste("AUC EM      =", round(auc(test_roc7), 2))),
       col = c("#d35967", "#0072B2", "#009E73"), lwd = 2)

# Adding diagonal line for reference
abline(0, 1, col = "black", lwd = 0.5)


library(pROC)

# Assuming you have the test_roc objects for p4, p6, p8 already calculated
# Plot the first ROC curve (p4)
plot(1 - test_roc4$specificities, test_roc4$sensitivities, type = "l", col = "#d35967",
     xlab = "1 - Especificidad", ylab = "Sensibilidad",
     main = "",
     cex.axis = 1.3, cex.lab = 1.3, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add the second ROC curve (p6)
lines(1 - test_roc6$specificities, test_roc6$sensitivities, type = "l", col = "#0072B2", lwd = 2)

# Add the third ROC curve (p8)
lines(1 - test_roc8$specificities, test_roc8$sensitivities, type = "l", col = "#009E73", lwd = 2)

# Add legends and AUC values
legend("bottomright", legend = c(paste("AUC PMM   =", round(auc(test_roc4), 2)),
                                 paste("AUC BPMM.=", round(auc(test_roc6), 2)),
                                 paste("AUC EM      =", round(auc(test_roc8), 2))),
       col = c("#d35967", "#0072B2", "#009E73"), lwd = 2)

# Adding diagonal line for reference
abline(0, 1, col = "black", lwd = 0.5)











