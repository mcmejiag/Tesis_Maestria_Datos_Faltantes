# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Codes Final/Simulaciones")


## Distribucions de Variables Continuas##
library(gamlss)

# Age Normal com media 60.61, desviación standar 2.725
set.seed(321)
Age<-rnorm(800, 60.61, 2.725)
histDist(Age, family = NO, freq = NULL, density = TRUE, main="Distribucion de Edad")
dev.off()


## saps3 Gamma, 
## Mu Coefficients:4.129, Sigma Coefficients: -1.327
#This are in log scale. Then log(mu)= 4.129, log(sigma)=-1.327
#https://search.r-project.org/CRAN/refmans/gamlss.dist/html/GA.html
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/gamlss.dist/gamlss.dist.pdf
set.seed(123)
s_saps3<-rGA(800, mu = 62.13, sigma =0.265)
hist(s_saps3)

histDist(s_saps3, family = GA, freq = NULL, density = TRUE, main="Distribucion de Saps3")


#sao2_admission (Gamma)
#Mu Coefficients:  4.532 = mu, Sigma=-2.69
## log(mu)=4.532 y log(Sigma)=-2.69
set.seed(123)
sao2_admission_a<-rGA(800, mu = 93.06 , sigma =0.068)

histDist(sao2_admission_a, family = GA, freq = NULL, density = TRUE, main="Distribucion de Sao2")


#Icu_los_2 Duration time in UCI ##
# Log(MU)= 2.649, Log(Sigma)=-0.1975
set.seed(123)
Icu_los<-rGA(800, mu = 14.13989168, sigma = 0.8207801406)
histDist(Icu_los, family = GA, freq = NULL, density = TRUE,  main="Distribucion de ICU")

## Systolib_bp_admission 
## Normal com media 123.5, desviación standar 3.201
set.seed(123)
systolic<-rnorm(800, 123.5, 3.201)
histDist(systolic, family = NO, freq = NULL, density = TRUE, main="Distribucion de Systolic")

# diastolic_bp_admission 
#Normal com media 75.01, desviación standar 2.749
set.seed(1)
diastolic<-rnorm(800, 75.01, 2.749)
histDist(diastolic, family = NO, freq = NULL, density = TRUE,  main="Distribucion de Diastolic")


###### Simulacion de Variables Categoricas #######
set.seed(123)
n<-800
intubation <- sample(c("Sí", "No"), size = n, replace = TRUE, prob = c(0.78, 0.22))
intubation<- factor(intubation)
table(intubation)
## Si =0, no=1

set.seed(123)
n<-800
sexo <- sample(c("Masculino", "Femenino"), size = n, replace = TRUE, prob = c(0.59, 0.41))
sexo<- factor(sexo)
table(sexo)
##0 male, 1 female

set.seed(123)
n<-800
Estado_general <- sample(c("Bueno", "Regular","Malo"), size = n, replace = TRUE, prob = c(0.39, 0.49, 0.12))
Estado_general<- factor(Estado_general)
table(Estado_general)


#### Dataset Combinando todas las variables ###

Data <- data.frame(
  Age = Age,
  Saps3 = s_saps3,
  Sao2 = sao2_admission_a,
  Icu_los = Icu_los,
  Systolic = systolic,
  Diastolic = diastolic,
  Intubation = intubation,
  Sexo = sexo,
  Estado_general = Estado_general
)
View(Data)

##########################
### Cheking Levels########
levels(Data$Intubation)
is.factor(Data$Intubation)
contrasts(Data$Intubation)

levels(Data$Sexo)
is.factor(Data$Sexo)
contrasts(Data$Sexo)

levels(Data$Estado_general)
is.factor(Data$Estado_general)
contrasts(Data$Estado_general)

#############################

# Binary factors to 0/1
Data$Intubation_bin <- ifelse(Data$Intubation == "Sí", 1, 0)
Data$Sexo_bin <- ifelse(Data$Sexo == "Masculino", 1, 0)

# Dummy variables for Estado_general (reference: "Bueno")
Data$Estado_Regular <- ifelse(Data$Estado_general == "Regular", 1, 0)
Data$Estado_Malo <- ifelse(Data$Estado_general == "Malo", 1, 0)

View(Data)


####

target_deaths <- 290
n <- nrow(Data)

find_intercept <- function(beta_0_try) {
  xb <- beta_0_try +
    beta_age * Data$Age +
    beta_saps3 * Data$Saps3 +
    beta_sao2 * Data$Sao2 +
    beta_icu_los * Data$Icu_los +
    beta_intub * Data$Intubation_bin +
    beta_sexo * Data$Sexo_bin +
    beta_estado_regular * Data$Estado_Regular +
    beta_estado_malo * Data$Estado_Malo
  p <- 1 / (1 + exp(-xb))
  mean(p)
}

# Try a sequence of intercepts
intercepts <- seq(-8, -5, by=0.1)
mean_probs <- sapply(intercepts, find_intercept)

# Find the intercept closest to the target death rate
closest <- which.min(abs(mean_probs - (target_deaths/n)))
best_intercept <- intercepts[closest]
best_intercept





############################
## Definimos el modelo de
## Regresion logistica

# Coefficients
beta_0 <- -7.9       # intercept
beta_age <- 0.0576953      # older age, higher risk
beta_saps3 <- 0.0098103
beta_sao2 <- -0.0063243     # higher oxygen, lower risk
beta_icu_los <- -0.0010378
beta_intub <- 2.9699452
beta_sexo <- 0.4968814
beta_estado_regular <- 1.3071543
beta_estado_malo <- 1.4208588



## El mdelo de regression
xb <- beta_0 +
  beta_age * Data$Age +
  beta_saps3 * Data$Saps3 +
  beta_sao2 * Data$Sao2 +
  beta_icu_los * Data$Icu_los +
  beta_intub * Data$Intubation_bin +
  beta_sexo * Data$Sexo_bin +
  beta_estado_regular * Data$Estado_Regular +
  beta_estado_malo * Data$Estado_Malo

# Probability of death
p<- 1 / (1 + exp(-xb))
summary(p)

# Simulate death: 1 = fallecido, 0 = vivo

Data$y<- rbinom(n, 1, p)

table(Data$y)
prop.table(table(Data$y))

View(Data)

Data1= subset(Data,select=-c(Estado_Malo,Estado_Regular,Sexo_bin,Intubation_bin,Systolic,Diastolic))
View(Data1)

######### sex masculino=1 femenino=0 ######
levels(Data1$Sexo)
is.factor(Data1$Sexo)
contrasts(Data1$Sexo)
######### Intubation ######
levels(Data1$Intubation)
is.factor(Data1$Intubation)
contrasts(Data1$Intubation)

######## estado_general BEG,MEG,REG ###
levels(Data1$Estado_general)
is.factor(Data1$Estado_general)
contrasts(Data1$Estado_general)

###### Training the Data ########
table(Data1$y)
library(caret)
set.seed(123)
trainCovid <- createDataPartition(Data1$y, p=.80, list=FALSE,times=1)
head(trainCovid)
trainingCovid1 <- Data1[trainCovid, ]
testingCovid1 <- Data1[-trainCovid, ]

table(trainingCovid1$y)

##Model Fitting Uisng Training Data

xtabs(~y+Intubation+Estado_general+Sexo, data = trainingCovid1)

str(Data1)
set.seed(123)
mylogitcovid1 <- glm(y ~., data=trainingCovid1, family = "binomial")
summary(mylogitcovid1)

library(pROC)

test_prob1 = predict(mylogitcovid1, newdata = testingCovid1, type = "response")
test_roc1 = roc(response = testingCovid1$y, predictor = test_prob1)

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


## > table(Data$y)
# 0   1 
#510 290 
## Estado_generalRegular +Age+ Intubation
## ROC CURVE 0.75


mylogitcovid1 <- glm(y ~., data=trainingCovid1, family = "binomial")
summary(mylogitcovid1)

library(pROC)

test_prob1 = predict(mylogitcovid1, newdata = testingCovid1, type = "response")
test_roc1 = roc(response = testingCovid1$y, predictor = test_prob1)

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
