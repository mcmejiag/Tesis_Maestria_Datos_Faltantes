# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Simulaciones ejemplos")

getwd()


####### Some package could be useful##########

library(naniar)
library(readr)
library(ISLR)
library(formattable)
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



###############################################################
##################### Histograms ##############################
###############################################################

##################### Histogram of Age ########################
Age <- Ensayocovid2$age

install.packages("gamlss")
library(gamlss)

dis_age<-fitDist(Age, type=c("realplus"))

par(mfrow=c(2,2))

histDist(Age, family = BCPE, freq = NULL, density = TRUE, main = "Distribución BCPE ")

histDist(Age, family = GA, freq = NULL, density = TRUE, main = "Distribución GA ")

histDist(Age, family = NO, freq = NULL, density = TRUE, main = "Distribución NO ")


dev.off()

#Age Normal
#Mu Coefficients:
#[1]  60.61
#Sigma Coefficients:
#[1]  2.725



##################### Histogram of saps3 ########################
Saps3 <- Ensayocovid2$saps3
hist(Saps3,main = "Histograma Signos Vitales", col="lightblue", border="black")
#####---------------------------------------------------
dis_Saps3<-fitDist(Saps3, type=c("realplus"))

par(mfrow=c(2,2))

histDist(Saps3, family = BCPEo, freq = NULL, density = TRUE, main = "Distribución BCPEo ")

histDist(Saps3, family = GA, freq = NULL, density = TRUE, main = "Distribución GA ")

histDist(Saps3, family = NO, freq = NULL, density = TRUE, main = "Distribución NO")

#Gamma
#Call:  gamlssML(formula = Saps3, family = "GA") 
#Mu Coefficients:
#[1]  4.129
#Sigma Coefficients:
#[1]  -1.327

dev.off()
##################### Histogram of sao2_admission ########################
Sao<-na.omit(Ensayocovid2$sao2_admission)
hist(Sao,main = "Histograma Saturación y oxigeno", col="lightblue", border="black")
#####---------------------------------------------------
dis_Sao<-fitDist(Sao, type=c("realplus"))

par(mfrow=c(2,2))
histDist(Sao, family = BCCG, freq = NULL, density = TRUE, ylim=c(0, 0.1), main = "Distribución BCCG")

histDist(Sao, family = GA, freq = NULL, density = TRUE, ylim=c(0, 0.1),, main = "Distribución GA")

histDist(Sao, family = NO, freq = NULL, density = TRUE, ylim=c(0, 0.1), main = "Distribución NO")

histDist(Sao, family = WEI, freq = NULL, density = TRUE, ylim=c(0, 0.1), main = "Distribución WEI")

dev.off()
#Gamma
#Call:  gamlssML(formula = Sao, family = "GA") 
#Mu Coefficients:
# 4.532
#Sigma Coefficients:
# -2.69

##################### Histogram of icu_los_2 ########################
Icu<-na.omit(Ensayocovid2$icu_los_2)
hist(Icu,main = "Histograma Duración en UCI", col="lightblue", border="black")

#####---------------------------------------------------
dis_sao<-fitDist(Icu, type=c("realplus"))

par(mfrow=c(2,2))
histDist(Icu, family = BCCG, freq = NULL, density = TRUE, ylim=c(0, 0.06), main = "Distribución BCCG")

histDist(Icu, family = GA, freq = NULL, density = TRUE, ylim=c(0, 0.06), main = "Distribución GA")

histDist(Icu, family = EXP, freq = NULL, density = TRUE, ylim=c(0, 0.06), main = "Distribución EXP")

#histDist(Icu, family = WEI2, freq = NULL, density = TRUE, ylim=c(0, 0.06))

dev.off()
#Gama
#Mu Coefficients:
#[1]  2.649
#Sigma Coefficients:
#[1]  -0.1975

##################### Histogram of systolic_bp_admission ########################
systolic<-na.omit(Ensayocovid2$systolic_bp_admission)
hist(systolic,main = "Histograma PA Systolic", col="lightblue", border="black")

#####---------------------------------------------------
dis_Sys<-fitDist(systolic, type=c("realplus"))

par(mfrow=c(2,2))
histDist(systolic, family = exGAUS, freq = NULL, density = TRUE, main = "Distribución exGAUS")

histDist(systolic, family = GA, freq = NULL, density = TRUE, main = "Distribución GA")

histDist(systolic, family = NO, freq = NULL, density = TRUE, main = "Distribución NO")

#Normal
#Mu Coefficients:
#[1]  123.5
#Sigma Coefficients:
#[1]  3.201


##################### Histogram of diastolic_bp_admission ########################
diastolic<-na.omit(Ensayocovid2$diastolic_bp_admission)
hist(diastolic,main = "Histograma PA diagnostic", col="lightblue", border="black")
#####---------------------------------------------------
dis_dias<-fitDist(diastolic, type=c("realplus"))

dev.off()
par(mfrow=c(2,2))
histDist(diastolic, family = BCTo, freq = NULL, density = TRUE, main = "Distribución BCTo")

histDist(diastolic, family = GA, freq = NULL, density = TRUE, main = "Distribución GA")

histDist(diastolic, family = NO, freq = NULL, density = TRUE, main = "Distribución NO")

#Normal
#Mu Coefficients:
#[1]  75.01
#Sigma Coefficients:
#[1]  2.749



