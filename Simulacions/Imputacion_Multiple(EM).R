Age<-rnorm(800, 60.61, 2.725)
s_saps3<-rGA(800, mu = 62.13, sigma =0.265)
sao2_admission_a<-rGA(800, mu = 93.06 , sigma =0.068)
Icu_los<-rGA(800, mu = 14.13989168, sigma = 0.8207801406)
systolic<-rnorm(800, 123.5, 3.201)
diastolic<-rnorm(800, 75.01, 2.749)
n<-800
intubation <- sample(c("Sí", "No"), size = n, replace = TRUE, prob = c(0.78, 0.22))
sexo <- sample(c("Masculino", "Femenino"), size = n, replace = TRUE, prob = c(0.59, 0.41))
Estado_general <- sample(c("Bueno", "Regular","Malo"), size = n, replace = TRUE, prob = c(0.39, 0.49, 0.12))

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())

##Setting up the working directory
setwd("/Users/ycd455/Desktop/University Camila 2025/Tesis Maestria 2025/Codes Final/Simulaciones")

library(GenSA) # Si usas funciones como rGA de otro paquete, asegúrate de cargarlo

# Creamos una función que genera una sola muestra
generar_muestra <- function(n = 800) {
  data.frame(
    Age = rnorm(n, 60.61, 2.725),
    s_saps3 = rGA(n, mu = 62.13, sigma = 0.265),
    sao2_admission_a = rGA(n, mu = 93.06 , sigma = 0.068),
    Icu_los = rGA(n, mu = 14.13989168, sigma = 0.8207801406),
    systolic = rnorm(n, 123.5, 3.201),
    diastolic = rnorm(n, 75.01, 2.749),
    intubation = sample(c("Sí", "No"), size = n, replace = TRUE, prob = c(0.78, 0.22)),
    sexo = sample(c("Masculino", "Femenino"), size = n, replace = TRUE, prob = c(0.59, 0.41)),
    Estado_general = sample(c("Bueno", "Regular","Malo"), size = n, replace = TRUE, prob = c(0.39, 0.49, 0.12))
  )
}

# Generamos 100 muestras
set.seed(123)  # Para reproducibilidad
muestras <- lapply(1:100, function(i) generar_muestra())

# Puedes acceder a cada muestra así:
head(muestras[[1]])  # Primera muestra
head(muestras[[2]])  # Segunda muestra
summary(muestras[[1]])
summary(muestras[[80]])

####### Redefinir las variables categoricas ###
## Las variables dummy y los factores

muestras_procesadas <- lapply(muestras, function(Data) {
  
  # Verificación de niveles (puedes imprimir si lo necesitas, pero omitimos aquí para procesamiento en lote)
  # levels(Data$Intubation); is.factor(Data$Intubation); contrasts(Data$Intubation)
  # levels(Data$Sexo); is.factor(Data$Sexo); contrasts(Data$Sexo)
  # levels(Data$Estado_general); is.factor(Data$Estado_general); contrasts(Data$Estado_general)
  
  # Crear variables binarias y dummies
  Data$Intubation_bin <- ifelse(Data$intubation == "Sí", 1, 0)
  Data$Sexo_bin <- ifelse(Data$sexo == "Masculino", 1, 0)
  Data$Estado_Regular <- ifelse(Data$Estado_general == "Regular", 1, 0)
  Data$Estado_Malo <- ifelse(Data$Estado_general == "Malo", 1, 0)
  
  # Convertimos a factores (por si no lo están)
  Data$intubation <- factor(Data$intubation)  # Usa el nombre exacto de tu columna
  Data$sexo <- factor(Data$sexo)
  Data$Estado_general <- factor(Data$Estado_general, levels = c("Bueno", "Regular", "Malo")) # Definimos orden
  
  return(Data)
})

str(muestras_procesadas[[1]])  # Muestra 1 con nuevas columnas
table(muestras_procesadas[[1]]$Estado_general)
head(muestras_procesadas[[1]][, c("intubation", "Intubation_bin", "sexo", "Sexo_bin", "Estado_general", "Estado_Regular", "Estado_Malo")])


####### Modelo de Regresion Logistica 
## con coeficientes fijos para modelar
## la variable mortalidad para cada una
## de las 100 muestras
########################

# Coeficientes del modelo
beta_0 <- -7.9
beta_age <- 0.0576953
beta_saps3 <- 0.0098103
beta_sao2 <- -0.0063243
beta_icu_los <- -0.0010378
beta_intub <- 2.9699452
beta_sexo <- 0.4968814
beta_estado_regular <- 1.3071543
beta_estado_malo <- 1.4208588

# Aplicamos el modelo a cada muestra
muestras_finales <- lapply(muestras_procesadas, function(Data) {
  # Renombrar si es necesario
  
  # Cálculo del valor lineal del modelo logístico
  xb <- beta_0 +
    beta_age * Data$Age +
    beta_saps3 * Data$s_saps3 +
    beta_sao2 * Data$sao2_admission_a +
    beta_icu_los * Data$Icu_los +
    beta_intub * Data$Intubation_bin +
    beta_sexo * Data$Sexo_bin +
    beta_estado_regular * Data$Estado_Regular +
    beta_estado_malo * Data$Estado_Malo
  
  # Calcular probabilidad de fallecimiento
  p <- 1 / (1 + exp(-xb))
  
  # Simular variable respuesta (mortalidad)
  Data$y <- rbinom(n = length(p), size = 1, prob = p)
  
  return(Data)
})

table(muestras_finales[[1]]$y)
prop.table(table(muestras_finales[[1]]$y))
summary(muestras_finales[[1]])
str(muestras_finales[[1]])

########## Crear nuevo conjunto de datos
## sin las variables que no usaremos en el modelo

# Eliminar columnas específicas en todas las muestras
muestras_sin_vars <- lapply(muestras_finales, function(Data) {
  subset(Data, select = -c(Estado_Malo, Estado_Regular, Sexo_bin, Intubation_bin, systolic, diastolic))
})

names(muestras_sin_vars[[1]])
summary(muestras_sin_vars[[1]])
str(muestras_sin_vars[[1]])

###############################################
####### Generar Valores Faltantes #############
##############################################

library(dplyr)

# Definir las proporciones de valores faltantes
prop_missingintubation <- 0.064  # 6.04% missing values
prop_missingsaps3 <- 0.3843  # 38.43% missing values
prop_missingsao <- 0.0023  # 0.23% missing values
prop_missingicu <- 0.3047  # 30.47% missing values
prop_missingestado <- 0.3133  # 31.33% missing values


# Insertar valores faltantes en cada conjunto de datos de "muestras_sin_vars"
muestras_con_faltantes <- lapply(muestras_sin_vars, function(Data) {
  # Insertar valores faltantes en las variables seleccionadas
  Data$intubation[sample(nrow(Data), prop_missingintubation * nrow(Data))] <- NA
  Data$s_saps3[sample(nrow(Data), prop_missingsaps3 * nrow(Data))] <- NA
  Data$sao2_admission_a[sample(nrow(Data), prop_missingsao * nrow(Data))] <- NA
  Data$Icu_los[sample(nrow(Data), prop_missingicu * nrow(Data))] <- NA
  Data$Estado_general[sample(nrow(Data), prop_missingestado * nrow(Data))] <- NA
  
  return(Data)
})

summary(muestras_con_faltantes[[1]])
colSums(is.na(muestras_con_faltantes[[1]]))

######## IMPUTACION EM #######################

library(mice)
library(Amelia)
library(missForest)
# Realizar imputación múltiple para cada conjunto de datos en muestras_con_faltantes
muestras_imputadas <- lapply(muestras_con_faltantes, function(Data) {
  
  # Verificar si Data es un data.frame, si no, convertirlo
  if (!is.data.frame(Data)) {
    Data <- as.data.frame(Data)
  }
  
  categorical_vars <- c( "sexo", "Estado_general", "intubation")
  # Realizar imputación múltiple usando el método de regresión predictiva (bbpmm)
  amelia_fit <- amelia(Data, m=4, parallel = "multicore", noms = categorical_vars, p2s=0)
  
  
  # Crear las versiones completas de los datos imputados
  Data1<- amelia_fit$imputations[[1]]
  Data2<- amelia_fit$imputations[[2]]
  Data3<- amelia_fit$imputations[[3]]
  Data4<- amelia_fit$imputations[[4]]
  
  # Crear una nueva base de datos donde reemplazamos los valores imputados
  NewDataset <- Data  # Usamos Data como base para NewDataset
  
  # Reemplazar filas con las bases de datos imputadas
  NewDataset[1:200, ] <- Data1[1:200, ]
  NewDataset[201:400, ] <- Data2[201:400, ]
  NewDataset[401:600, ] <- Data3[401:600, ]
  NewDataset[601:800, ] <- Data4[601:800, ]
  
  # Devolver el conjunto de datos imputado
  return(NewDataset)
})

# Verifica la estructura de las muestras imputadas
str(muestras_imputadas[[1]])

# Si deseas acceder a una muestra imputada específica, por ejemplo, la primera muestra:
summary(muestras_imputadas[[1]])
View(muestras_imputadas[[1]])



############ Organizar dataset
## Dividir mi conjunto de datos
## en entrenamiento y prueba
#############################

library(caret)

set.seed(123)

# Paso 1 y 2: convertir factores y hacer split para las 100 muestras
splits_list <- lapply(muestras_imputadas, function(Data1) {
  
  # Crear partición de entrenamiento (80%)
  train_idx <- createDataPartition(Data1$y, p = 0.80, list = FALSE)
  training <- Data1[train_idx, ]
  testing <- Data1[-train_idx, ]
  
  # Devolver ambos conjuntos
  list(train = training, test = testing)
})

table(splits_list[[1]]$train$y)
prop.table(table(splits_list[[1]]$train$y))

table(splits_list[[1]]$test$y)
prop.table(table(splits_list[[1]]$test$y))


library(pROC)
set.seed(123)
# Inicializamos vectores para guardar resultados
auc_values <- numeric(100)
auc_vars <- numeric(100)
modelos_logit <- vector("list", 100)
# Loop sobre cada muestra
for (i in 1:100) {
  train_data <- splits_list[[i]]$train
  test_data <- splits_list[[i]]$test
  
  # Ajuste del modelo
  model <- glm(y ~ ., data = train_data, family = "binomial")
  modelos_logit[[i]] <- model
  # Predicciones de probabilidad
  test_prob <- predict(model, newdata = test_data, type = "response")
  
  # ROC y AUC
  test_roc <- roc(response = test_data$y, predictor = test_prob)
  auc_values[i] <- auc(test_roc)
  auc_vars[i] <- var(test_roc)
}


# Ver resultados
summary(auc_values)
summary(auc_vars)

# Crear un data frame con los resultados
resultados_auc <- data.frame(
  Muestra = 1:100,
  AUC = auc_values,
  Varianza_AUC = auc_vars
)

# Guardar como CSV
write.csv(resultados_auc, "resultados_auc_EM.csv", row.names = FALSE)

summary(modelos_logit[[1]])
summary(modelos_logit[[10]])
summary(modelos_logit[[24]])
summary(modelos_logit[[56]])
summary(modelos_logit[[72]])
summary(modelos_logit[[49]])


##### Validacion Cruzada ##################


# Realizar imputación múltiple para cada conjunto de datos en muestras_con_faltantes
muestras_imputadas_1 <- lapply(muestras_imputadas, function(NewDataset) {
  NewDataset <- NewDataset %>%
    mutate(y = recode(y, `1` = "Si", `0` = "No"))
  NewDataset$y <- factor(NewDataset$y)
  
  # Devolver el conjunto de datos imputado
  return(NewDataset)
})
summary(muestras_imputadas[[1]])
summary(muestras_imputadas_1[[1]])
str(muestras_imputadas_1[[1]])

library(caret)
library(pROC)


# Inicializamos vectores para guardar resultados
auc_values <- numeric(100)
auc_vars <- numeric(100)
modelos_logit <- vector("list", 100)

# Configuración de la validación cruzada
ctrl <- trainControl(method = "cv", 
                     number = 4, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

# Loop sobre cada muestra imputada
for (i in 1:100) {
  
  # Obtener los datos imputados para la muestra i
  train_data <- muestras_imputadas_1[[i]] # Ajustar esto según el formato de tus datos imputados
  test_data <- train_data  # En este ejemplo no se hace separación entre entrenamiento y prueba; ajusta según sea necesario.
  
  # Entrenamiento del modelo de regresión logística con validación cruzada
  model <- train(y ~ ., 
                 data = train_data, 
                 method = "glm", 
                 family = "binomial", 
                 trControl = ctrl, 
                 metric = "ROC")
  
  # Guardamos el modelo en la lista
  modelos_logit[[i]] <- model
  
  # Obtener las probabilidades predichas
  predictions <- predict.train(model, newdata = test_data, type = "prob")
  
  # Crear el objeto ROC
  roc_obj <- roc(response = test_data$y, predictor = predictions$Si)  # Cambiar "Yes" según el nombre de la columna
  
  # Calcular AUC y la varianza del AUC
  auc_values[i] <- auc(roc_obj)
  auc_vars[i] <- var(roc_obj)
}


# Ver resumen de los resultados
summary(auc_values)
summary(auc_vars)

# Crear un data frame con los resultados
resultados_auc <- data.frame(
  Muestra = 1:100,
  AUC = auc_values,
  Varianza_AUC = auc_vars
)

# Guardar los resultados en un archivo CSV
write.csv(resultados_auc, "resultados_auc_EM_CV.csv", row.names = FALSE)

# Mostrar los resúmenes de algunos modelos
summary(modelos_logit[[1]])
summary(modelos_logit[[10]])
summary(modelos_logit[[24]])
summary(modelos_logit[[56]])
summary(modelos_logit[[72]])
summary(modelos_logit[[49]])
