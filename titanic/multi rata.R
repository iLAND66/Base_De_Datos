laberinto<-function(X,Y,P){
  Z<-dim(P)[1]
  while ( tail (X ,1) !=Y){ # Mientras la rata no encuentre la comida del cuarto Y
    X <-c(X, sample (c (1:Z) ,1, prob =P[ tail (X ,1) ,])) # Escogemos un cuarto al azar a
  }
  return(X)
}

generar_matriz_transicion <- function(n, k) {
  # Tamaño total del laberinto
  tamano_laberinto <- n * k
  # Inicializa una matriz de transición con ceros
  matriz_transicion <- matrix(0, nrow = tamano_laberinto, ncol = tamano_laberinto)
  
  # Función para obtener la posición en la matriz 1D
  get_posicion <- function(fila, columna) {
    return(fila + (columna - 1) * n)
  }
  
  # Rellenar la matriz de transición con probabilidades de movimiento
  for (fila in 1:n) {
    for (columna in 1:k) {
      posicion_actual <- get_posicion(fila, columna)
      probabilidades_validas <- 0
      
      # Movimiento hacia arriba
      if (fila > 1) {
        posicion_arriba <- get_posicion(fila - 1, columna)
        matriz_transicion[posicion_actual, posicion_arriba] <- 0.25
        probabilidades_validas <- probabilidades_validas + 0.25
      }
      
      # Movimiento hacia abajo
      if (fila < n) {
        posicion_abajo <- get_posicion(fila + 1, columna)
        matriz_transicion[posicion_actual, posicion_abajo] <- 0.25
        probabilidades_validas <- probabilidades_validas + 0.25
      }
      
      # Movimiento hacia la izquierda
      if (columna > 1) {
        posicion_izquierda <- get_posicion(fila, columna - 1)
        matriz_transicion[posicion_actual, posicion_izquierda] <- 0.25
        probabilidades_validas <- probabilidades_validas + 0.25
      }
      
      # Movimiento hacia la derecha
      if (columna < k) {
        posicion_derecha <- get_posicion(fila, columna + 1)
        matriz_transicion[posicion_actual, posicion_derecha] <- 0.25
        probabilidades_validas <- probabilidades_validas + 0.25
      }
      # Ajustar las probabilidades para que sumen 1
      matriz_transicion[posicion_actual, ] <- matriz_transicion[posicion_actual, ] / probabilidades_validas
    }
  }
  
  return(matriz_transicion)
}

rata.montecarlo<-function(X,Y,P,n){
  l<-list()
  for(z in 1:n){
    mucha.rata<- paste("Rata Infinita #",z,sep = "")
    l[[mucha.rata]]<-laberinto(X,Y,P)
  }
  return(l)
}

corre.rata<-rata.montecarlo(3,16,P1,1000)

par(mar=c(1,1,1,1))
datos<-data.frame(x,y)
ggplot(datos,aes(x=x,y=y))+
  geom_line()



rata.promedio<-function(X,Y,P,n){v<-c();
  lista<-rata.montecarlo(X,Y,P,n);
  for (i in 1:n) {
    v[i]<-length(lista[[i]])
  }
  return(round(sum(v)/n))
}






