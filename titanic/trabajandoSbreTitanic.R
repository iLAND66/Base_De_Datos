train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv",header = T)

#creamos una base de datos para trabajar con ella porque
#le falta la variable survived
test.survived<-data.frame(Survived=rep("None",nrow(test)),test)

data.combined<-rbind(train,test.survived)

test.survived<-data.frame(Mr=rep("None",nrow(test)),test)
test.survived<-data.frame(Mrs=rep("None",nrow(test)),test)

analisisDeTablas <- function (columna1, columna2) {
  tabla <- table(columna1,columna2)
  sumValores <- c(sum(tabla[1,]),sum(tabla[2,]),sum(tabla[3,]))
  probabilidad <- tabla/sumValores
  if (identical(columna2, train$Survived)) {
    tablaSobrevivientes <- table(test.survived$Pclass, test.survived$Survived)
    tablaSobrevivientes[1]
    valores <- 0:1
    print(sample(valores,tablaSobrevivientes[1],replace = T,prob =probabilidad[1,]))
    cat("\n")
    print(sample(valores,tablaSobrevivientes[2],replace = T,prob =probabilidad[2,]))
    cat("\n")
    print(sample(valores,tablaSobrevivientes[3],replace = T,prob =probabilidad[3,]))
  } else {
    valores <- 0:1
    print(sample(valores, tabla[1], replace = T, prob = probabilidad[1,]))
    cat("\n")
    print(sample(valores, tabla[2], replace = T, prob = probabilidad[2,]))
    cat("\n")
    print(sample(valores, tabla[3], replace = T, prob = probabilidad[3,]))
  }
}

clasificar_personas <- function(data) {
  data <- data %>%
    mutate(
      Mr = ifelse(Sex == "male" & Age >= 14, 1, 0), # Hombres >= 14 años
      Mrs = ifelse(Sex == "female" & grepl("Mrs\\.", Name), 1, 0) # Mujeres con "Mrs." en el nombre
    )
  return(data)
}

test.survived <- analisisDeTablas(train$Pclass, train$Survived)
test.survived <- analisisDeTablas(train$Pclass, train$Sex)

head(test.survived)


####
#Desglose de los cambios:
#Clasificación de "Mr" y "Mrs":

#Mr: Se identifican como hombres (Sex == "male") de 14 años o más (Age >= 14).
#Mrs: Se identifican como mujeres (Sex == "female") con "Mrs." en el nombre.
#Análisis de tablas:

#Calcula las probabilidades condicionales para las combinaciones de las columnas proporcionadas (Pclass y Survived, Pclass y Sex).
#Los resultados se agregan al DataFrame como columnas con nombres descriptivos.
#Estructura limpia:

#Uso de dplyr para transformar y agregar columnas al DataFrame con claridad y menos código redundante.
#Compatibilidad con datos de prueba:

#Se asegura que el conjunto de prueba mantenga las mismas columnas para futuras predicciones o análisis.
#Resultado:
#El DataFrame tendrá columnas adicionales:

#Mr: Indica si un pasajero es "Mr".
#Mrs: Indica si un pasajero es "Mrs".
#Probabilidades calculadas por el análisis de tablas, como Survived_Clase1, Sexo_Clase1, etc.
library(dplyr)

# Cargar los datos
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Agregar columnas iniciales para Survived, Mr, y Mrs al conjunto de prueba
test <- test %>%
  mutate(Survived = "None", Mr = 0, Mrs = 0)

# Función para identificar "Mr" y "Mrs" en base a las condiciones dadas
clasificar_personas <- function(data) {
  data <- data %>%
    mutate(
      Mr = ifelse(Sex == "male" & Age >= 14, 1, 0), # Hombres >= 14 años
      Mrs = ifelse(Sex == "female" & grepl("Mrs\\.", Name), 1, 0) # Mujeres con "Mrs." en el nombre
    )
  return(data)
}

# Aplicar la clasificación a los datos de entrenamiento y prueba
train <- clasificar_personas(train)
test <- clasificar_personas(test)

# Función para realizar el análisis de tablas y agregar resultados como columnas
analisisDeTablas <- function(data, columna1, columna2, nombre_columna) {
  tabla <- table(columna1, columna2)
  probabilidad <- prop.table(tabla, margin = 1)

  # Agregar probabilidades al DataFrame
  data <- data %>%
    mutate(
      paste0(nombre_columna, "_Clase1") = probabilidad[1, 2],
      paste0(nombre_columna, "_Clase2") = probabilidad[2, 2],
      paste0(nombre_columna, "_Clase3") = probabilidad[3, 2]
    )

  return(data)
}

# Agregar los resultados de análisis de tablas al DataFrame de entrenamiento
train <- analisisDeTablas(train, train$Pclass, train$Survived, "Survived")
train <- analisisDeTablas(train, train$Pclass, train$Sex, "Sexo")

# Vista previa del DataFrame final
head(train)
head(test)

