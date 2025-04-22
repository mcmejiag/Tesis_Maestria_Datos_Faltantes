# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Codes Final")

getwd()

#Read#Reading data into R

library(readr)
Ensayocovid <- read_delim("Ensayo 5.csv", delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE)

View(Ensayocovid)
dim(Ensayocovid)
summary(Ensayocovid)

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
head(Ensayocovid1)


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


# Pacientes muertos
muertos <- subset(Ensayocovid2, death == "sim")

# Pacientes sobrevivientes
sobrevivientes <- subset(Ensayocovid2, death == "nao")


# Summary para pacientes muertos
summary(muertos)

# Summary para pacientes sobrevivientes
summary(sobrevivientes)

# Create a function to extract p-values from Mann-Whitney tests
get_pvalues <- function(data, variables) {
  pvalues <- numeric(length(variables))
  names(pvalues) <- variables
  
  for (i in seq_along(variables)) {
    var <- variables[i]
    # Skip if the variable doesn't exist or contains non-numeric data
    if (!var %in% names(data) || !is.numeric(data[[var]])) {
      pvalues[i] <- NA
      next
    }
    
    # Perform Mann-Whitney U test and extract p-value
    test_result <- wilcox.test(data[[var]] ~ data$death, exact = FALSE)
    pvalues[i] <- test_result$p.value
  }
  
  return(pvalues)
}

# List of variables to test (adjust as needed based on your dataset)
variables_to_test <- c("age", "saps3", "sao2_admission", "icu_los_2", "systolic_bp_admission", "diastolic_bp_admission")

# Get p-values
p_values <- get_pvalues(Ensayocovid2, variables_to_test)

# Round to 2 decimal places for display
p_values_rounded <- round(p_values, 2)

# Replace very small values with "<0.01"
p_values_rounded <- ifelse(p_values_rounded < 0.01, "<0.01", p_values_rounded)

# Display p-values
p_values_rounded


#############################################################################################
#######################
#############################################################################################



# Pacientes muertos
intubados <- subset(Ensayocovid2, intubation == "sim")

# Pacientes sobrevivientes
nointubados <- subset(Ensayocovid2, intubation == "nao")

summary(intubados)
summary(nointubados)



# Create a function to extract p-values from Mann-Whitney tests
get_pvalues <- function(data, variables) {
  pvalues <- numeric(length(variables))
  names(pvalues) <- variables
  
  for (i in seq_along(variables)) {
    var <- variables[i]
    # Skip if the variable doesn't exist or contains non-numeric data
    if (!var %in% names(data) || !is.numeric(data[[var]])) {
      pvalues[i] <- NA
      next
    }
    
    # Perform Mann-Whitney U test and extract p-value
    test_result <- wilcox.test(data[[var]] ~ data$intubation, exact = FALSE)
    pvalues[i] <- test_result$p.value
  }
  
  return(pvalues)
}

# List of variables to test (adjust as needed based on your dataset)
variables_to_test <- c("age", "saps3", "sao2_admission", "icu_los_2", "systolic_bp_admission", "diastolic_bp_admission")

# Get p-values
p_values <- get_pvalues(Ensayocovid2, variables_to_test)

# Round to 2 decimal places for display
p_values_rounded <- round(p_values, 2)

# Replace very small values with "<0.01"
p_values_rounded <- ifelse(p_values_rounded < 0.01, "<0.01", p_values_rounded)

# Display p-values
p_values_rounded
