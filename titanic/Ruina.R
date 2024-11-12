#basura ----
if (length(grep("Miss.",n))>0){
  return("Miss.")
} else if (length(grep("Master.",n))>0){
  return("Master.")
  
  h<-"Miss. Juanita"
  k<-"Mr. Jack"
  grep("Miss.",k)
  
  
  
AA<-c("Reyes","Gutiérrez","H. Figueroa","H. Mtz","Mtz. Castañeda","Carvajal","Machucho","Lópes^2","Medina","Camacho","Palma")

sample(AA,4)

sample(AA,4)






  

#ruina

#Ejemplo 1.1 (Caminatas aleatorias simples y el problema de la ruina). Imaginemos
#la siguiente situacion: tengo un capital de 20 pesos al tiempo cero y cada
#instante de tiempo apuesto un peso en un volado, ganando si cae aguila. ¿como
#puedo estudiar matematicamente a la evolucion de mi capital en el tiempo? De particular
#interes es la variable aleatoria que nos indica el instante en que me arruino
#por primera vez, misma que a priori podria ser infinita si jamas me arruino.



C<-20 #C es un vector cuya entrada i sera ́ mi capital al tiempo i 
aux<-C #Esta variable me dice cu ́al es el  ́ultimo valor de mi capital
while (aux>0) { #Mientras no me haya arruinado
  aux<-aux+2*(runif(1)<1/2)-1 #actualizo mi capital al sumarle una variable que toma valores -1 y 1 con probabilidad 1/2
  C<-c(C,aux) #Agrego el  ́ultimo valor de mi fortuna al vector C
} 
#par(mar=c(1,1,1,1)) #sirve para arreglar los márgenes
plot(C)


#Ejemplo 1.2 (Apostando con prisa). Modificaremos el ejemplo anterior como
#sigue: tengo un capital de 20 pesos al tiempo cero y cada instante de tiempo
#apuesto en un volado ya sea la mitad de mi fortuna si tengo mas de 6 pesos o 2
#pesos si mi fortuna es menor o igual a 6, ganando si cae aguila.




C<-20 #C es un vector cuya entrada i ser ́a mi capital al tiempo 
fortuna<-C #Esta variable me dice cu ́al es el u ́ltimo valor de mi
while (fortuna>0){ #Mientras no me haya arruinado
  monto<-2*(fortuna <=6)+floor(fortuna/2)*(fortuna >6)
  fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1) 
  C<-c(C,fortuna) 
} 

plot(C)


C<-20 
fortuna<-C 
while (fortuna>0){ 
  monto<-2*(fortuna <=6)+floor(fortuna/2)*(fortuna >6)
  fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1) 
  C<-c(C,fortuna) 
} 

almacena<-function(n){l=list(); for(i in 1:n)
  C<-20 
fortuna<-C 
while (fortuna>0){ 
  monto<-2*(fortuna <=6)+floor(fortuna/2)*(fortuna >6)
  fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1) 
  C<-c(C,fortuna)} l[[i]]=C;return(l)}




data <- data.frame(x = c(1:5, 8),y = c(1:6),z=rep(5,6))
data 
medias<-apply(data, 2, mean)    





C<-20 #C es un vector cuya entrada i ser ́a mi capital al tiempo 
fortuna<-C #Esta variable me dice cu ́al es el u ́ltimo valor de mi
while (fortuna>0){ #Mientras no me haya arruinado
  monto<-2*(fortuna <=6)+floor(fortuna/2)*(fortuna >6)
  fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1) 
  C<-c(C,fortuna) 
} 

fort<-function(v){aux<-v;while (v>0){aux<-aux+2*(runif(1)<1/2)-1 
v<-c(v,aux)}
return(v)
} 

sims.demo <- function(a) { x <- a ; while(x < 100) x <- x + 1;return(x) }
 b <- sims.demo(10)
 b

 
 
 
 
 factorialR <- function(x) {
   if (x == 0) {
     res <- 1
   } else {
      res <- x
     while(x > 1){
       res <- (x - 1) * res
       x <- x - 1
     }
   }
   return(res)
 } 
 
 
 
#simulación tipo montecarlo de las apuestas ----
 
 
 fort<-function(v){aux<-v
 while (aux>0) { 
   aux<-aux+2*(runif(1)<1/2)-1 
   v<-c(v,aux) 
 }
 return(v)
 }
 
 montecarlo_fort<-function(v,n){l=list();for(i in 1:n)
   l[[i]]=fort(v);return(l)
   }
 
 #ejemplo
 montecarlo_fort(30,5)
 
 long<-function(l){v=c();for(i in 1:length(l))
   v[i]=length(l[[i]]);return(v)}
 
 tiempo_medio<-function(v,n){
   t<-floor(mean(long(montecarlo_fort(v,n))));return(t)}
 
 #ejemplo
 tiempo_medio(15,4)
 
 #para apuestas fuertes
 
 fort_fuer<-function(v){fortuna<-v
 while (fortuna>0){ 
   monto<-2*(fortuna <=6)+floor(fortuna/2)*(fortuna >6)
   fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1) 
   v<-c(v,fortuna) 
 }
 return(v)
 }
 
 montecarlo_fuer<-function(v,n){l=list();for(i in 1:n)
   l[[i]]=fort_fuer(v);return(l)
 }
 
 #tiempo medio apuestas fuertes
 
 tiempo_medio_f<-function(v,n){
   t<-floor(mean(long(montecarlo_fuer(v,n))));return(t)}
 
 #ejemplo
 
 tiempo_medio_f(20,10)
 
# Tiempo montecarlo
 
 l_tiem<-function(k,r){v=c();for (i in 1:r) {v[i]= tiempo_medio_f(k,r)
 };return(v)}
 
med_mon<-function(k,r){v= l_tiem(k,r);m<-mean(v);
ifelse(m-floor(m)<1/2,floor(m),floor(m)+1)
} 
med_mon(10,50) 
 