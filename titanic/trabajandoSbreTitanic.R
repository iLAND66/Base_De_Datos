train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv",header = T)

#creamos una base de datos para trabajar con ella porque
#le falta la variable survived
test.survived<-data.frame(Survived=rep("None",nrow(test)),test)

#data.combined<-rbind(train,test.survived)

analisisDeTablas <- function (columna1, columna2, df) {
  tabla <- table(columna1, columna2)
  sumValores <- c(sum(tabla[1,]), sum(tabla[2,]), sum(tabla[3,]))
  probabilidad <- tabla / sumValores
  resultados <- numeric(nrow(df))  # Inicializar un vector para los resultados

  if (identical(columna2, train$Survived)) {
    tablaSobrevivientes <- table(df$Pclass, df$Survived)
    valores <- 0:1
    for (i in 1:3) {
      # Asegurarse de que la longitud coincida con el número de filas que cumplen la condición
      num_elementos <- sum(df$Pclass == i)
      if (num_elementos > 0) {
        resultados[df$Pclass == i] <- sample(valores, num_elementos, replace = TRUE, prob = probabilidad[i,])
      }
    }
  } else {
    valores <- 0:1
    for (i in 1:3) {
      num_elementos <- sum(df$Pclass == i)
      if (num_elementos > 0) {
        resultados[df$Pclass == i] <- sample(valores, num_elementos, replace = TRUE, prob = probabilidad[i,])
      }
    }
  }
  return(resultados)
}

analizarMrPorClase <- function(data) {
  # Verificar si el nombre contiene "Mr." y si la edad es mayor a 14 años
  cumpleCondicion <- grepl("Mr\\.", data$Name) & data$Age > 14

  # Crear una tabla con las clases existentes y la cantidad total de personas por clase
  tabla <- table(data$Pclass)

  # Calcular la probabilidad de cumplir la condición para cada clase
  probabilidadCumple <- tapply(cumpleCondicion, data$Pclass, sum) / tabla

  # Reemplazar NA en probabilidades por 0
  probabilidadCumple[is.na(probabilidadCumple)] <- 0

  # Generar resultados para cada clase
  resultados <- rep(NA, nrow(data))  # Inicializar vector de resultados
  for (clase in names(tabla)) {
    indicesClase <- which(data$Pclass == as.numeric(clase))
    resultados[indicesClase] <- sample(c(0, 1), length(indicesClase), replace = TRUE,
                                       prob = c(1 - probabilidadCumple[clase], probabilidadCumple[clase]))
  }

  return(resultados)
}

analizarMrsPorClase <- function(data) {
  # Verificar si el nombre contiene "Mr." y si la edad es mayor a 14 años
  cumpleCondicion <- grepl("Mrs\\.", data$Name) & data$Age > 14

  # Crear una tabla con las clases existentes y la cantidad total de personas por clase
  tabla <- table(data$Pclass)

  # Calcular la probabilidad de cumplir la condición para cada clase
  probabilidadCumple <- tapply(cumpleCondicion, data$Pclass, sum) / tabla

  # Reemplazar NA en probabilidades por 0
  probabilidadCumple[is.na(probabilidadCumple)] <- 0

  # Generar resultados para cada clase
  resultados <- rep(NA, nrow(data))  # Inicializar vector de resultados
  for (clase in names(tabla)) {
    indicesClase <- which(data$Pclass == as.numeric(clase))
    resultados[indicesClase] <- sample(c(0, 1), length(indicesClase), replace = TRUE,
                                       prob = c(1 - probabilidadCumple[clase], probabilidadCumple[clase]))
  }

  return(resultados)
}


test.survived$Sobrevivientes <- analisisDeTablas(train$Pclass, train$Survived, test.survived)
test.survived$Genero <- analisisDeTablas(train$Pclass, train$Sex, test.survived)
resultadosMr <- analizarMrPorClase(test.survived)
test.survived$Mr <- resultadosMr
resultadosMrs <- analizarMrsPorClase(test.survived)
test.survived$Mrs <- resultadosMrs

head(test.survived)

