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

analisisDeTablas(train$Pclass, train$Survived)
analisisDeTablas(train$Pclass, train$Sex)



#definimos una tabla usando las columnas pclas y survived
tabla <- table(train$Pclass,train$Survived)
#creamos un vector que va a sumar las filas de los valores de nuestra tabla
sumValores <- c(sum(tabla[1,]),sum(tabla[2,]),sum(tabla[3,]))
#definimos nuestra probabilidad dividiendo nuestra tabla sobre suma de las filas
probabilidad <- tabla/sumValores

#definimos otra tabla de nuestro df test.survived
tablaSobrevivientes <-table(test.survived$Pclass,test.survived$Survived)
#convertimos la tabla a matriz usando []
tablaSobrevivientes[1]
#cambiamos los valores a ceros y unos
valores <- 0:1

sample(valores,tablaSobrevivientes[1],replace = T,prob =probabilidad[1,])
sample(valores,tablaSobrevivientes[2],replace = T,prob =probabilidad[2,])
sample(valores,tablaSobrevivientes[3],replace = T,prob =probabilidad[3,])

#hacer lo mismo usando pclass y sex
tablaClasSex <- table(train$Pclass, train$Sex)
sumValores <- c(sum(tablaClasSex[1,]),sum(tablaClasSex[2,]),sum(tablaClasSex[3,]))
proba <- tablaClasSex/sumValores

tablaClasSex2 <- tablaClasSex
tablaClasSex2[1]
valoresClSe <- 0:1
sample(valoresClSe, tablaClasSex2[1], replace = T, prob = proba[1,])
sample(valoresClSe, tablaClasSex2[2], replace = T, prob = proba[2,])
sample(valoresClSe, tablaClasSex2[3], replace = T, prob = proba[3,])
