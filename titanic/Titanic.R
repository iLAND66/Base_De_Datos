#Damos de alta nuestra sesión con el botón Session > Set working
#directory

#test <- read.csv("~/Desktop/Clase R-Studio/titanic/test.csv")
#train <- read.csv("~/Desktop/Clase R-Studio/titanic/train.csv")

train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv",header = T)

#creamos una base de datos para trabajar con ella porque
#le falta la variable survived
test.survived<-data.frame(Survived=rep("None",nrow(test)),test)

#combinemos las dos bases de datos
data.combined<-rbind(train,test.survived)

data.combined[444,]$Name<-"Reynaldo, Mrs. 
Encarnacion"

data.combined[980,]$Name<-"O'Donoghue, Mrs. Bridget"
#Estructura de nuestros datos
str(data.combined)

data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)

#veamos las tasas de supervivencia

table(data.combined$Survived)

#veamos la distribución de las clases sociales
table(data.combined$Pclass)

#para hacer algunas gráficas
#install.packages("ggplot2")
library(ggplot2)

#Nuestra hipótesis de investigación es que sobrevivieron
#más personas ricas que pobres

train$Pclass<-as.factor(train$Pclass)

ggplot(train,aes(x=Pclass,fill=factor(Survived))) +
  stat_count(width = .5)+
  xlab("Clase social")+
  ylab("Total")+
  labs(fill="Supervivencia")
  
#La base de datos tiene la variable nombre como 
#factor. Para visualizar bien esta variable hacemos:
head(as.character(train$Name))
#Por ejemplo
"Cumings, Mrs. John Bradley (Florence Briggs Thayer)"
#Apellido #Título #Marido #Nombre de soltera

#Veamos el número de nombres únicos

length(unique(as.character(data.combined$Name)))

length(as.character(data.combined$Name))

#Para obtener los nombres duplicados
dup.names<-as.character(
  data.combined[which(duplicated(
    as.character(data.combined$Name))),"Name"])
#train[c(23,126,456),c("Name","Age")]
#Analicemos tales duplicados en nuestra base de datos

dups<-data.combined[which(data.combined$Name 
                          %in% dup.names),]

#Qué información me da Mr., Miss, Master
#install.packages("stringr")
library(stringr)

#str_detect() es una función que detecta un patrón en 
#una cadena

misses<-data.combined[which(str_detect(data.combined$Name,
                                       "Miss.")), ]

View(head(misses,15))
#función que asigna edades
asigna.edad<-function(datos,titulo){
  estado<-which(str_detect(datos$Name,titulo))
  tit<-datos[estado,]
  m1<-as.factor(as.character(tit$Age))
  coor.m1<-which(m1!="NA")
  m2<-tit[coor.m1,]$Age
  media.tit<-mean(m2)
  sd.tit<-sd(m2)
  coor.m1.NA<-which(is.na(m1)==T)
  m3<-rnorm(length(coor.m1.NA),media.tit,sd.tit)
  m3<-round(ifelse(m3<0,0,m3))
  tit[coor.m1.NA,"Age"]<-m3
  d1<-data.frame(Coordenadas=coor.m1.NA,Edad_asignada=m3)
  return(d1)
}

asigna.edad(data.combined,"Miss.")
asigna.edad(data.combined,"Mr.")
asigna.edad(data.combined,"Mrs.")

males<-data.combined[which(data.combined$Sex=="male"),]

juniors<-which(males$Pclass==1 & males$Age<15)

j1<-males[juniors,]

cucharita<-which(males$Pclass==2 & males$Age<15)

j2<-males[cucharita,]

pancito<-which(males$Pclass==3 & males$Age<15)

j3<-males[pancito,]

#Lo anterior nos indica que el título de la persona
#parece ser una variable de interés

#función que extrae el título
extraeTit<-function(n){
  n<-as.character(n)
  if (length(grep("Miss.",n))>0){
    return("Miss.")
  } else if (length(grep("Master.",n))>0){
    return("Master.")
  } else if (length(grep("Mrs.",n))>0){
    return("Mrs.")
  } else if (length(grep("Mr.",n))>0){
    return("Mr.")
  } else {
    return("Otro")
  }
}

titulo<-function(n){v=c();for(i in 1:length(n))
v[i]=extraeTit(n[i]);return(as.factor(v))
}

#extraeTit(data.combined$Name[6])
titulo(data.combined$Name)

#Otra manera de hacerlo
#Titulo<-NULL
#for(i in 1:nrow(data.combined)){
 # Titulo<-c(Titulo,extraeTit(data.combined[i,"Name"]))
#}

data.combined$Titulo<-titulo(data.combined$Name)

otros<-which(data.combined$Titulo=="Otro")
otros2<-data.combined[otros,]

revs<-grep("Rev.",data.combined$Name)
dr<-grep("Dr.",data.combined$Name)
dones<-grep("Don.",data.combined$Name)

#Graficamos los datos conocidos (del 1 al 891)

#Note que facet_wrap(~Pclass) parte al conjunto de 
#datos en tres categorías

ggplot(data.combined[1:891, ], aes(x = Titulo, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Clase social") +
  xlab("Título") +
  ylab("Total") +
  labs(fill = "Sobrevivientes")


#¿Cuál es la distribución entre hombres y mujeres en nuestra base de datos?

table(data.combined$Sex)


ggplot(data.combined[1:891, ], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Clase social") +
  xlab("Sexo") +
  ylab("Total") +
  labs(fill = "Sobrevivientes")

#Parece ser que el sexo y la edad juegan un papel importante en
#nuestro análisis

summary(data.combined$Age)

ggplot(data.combined[1:891, ],aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10)+
  ggtitle("Clase social") +
  xlab("Edad") +
  ylab("Total") +  
  labs(fill = "Sobrevivientes")

#validemos que master es un título adecuado para niño rico

juniors<-data.combined[which(data.combined$Titulo=="Master."),]
summary(juniors$Age)
View(juniors)

#La variable miss es más complicada, analicemos más a detalle

misses<-data.combined[which(data.combined$Titulo=="Miss."),]
summary(misses$Age)
View(misses)

ggplot(misses[misses$Survived!="None", ], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5)+
  ggtitle("Edad de Miss. por clase social") +
  xlab("Edad") +
  ylab("Total") +  
  labs(fill = "Sobrevivientes")

#Nuestra gráfica arroja diferencias con respecto a la 
#supervivencia por edades de las Misses

#Analicemos a las solteras

solteras<-misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(solteras$Age)
length(which(solteras$Age<=14.5))

#Sólo 4 de ellas eran niñas solteras :(

#recordemos sibling = hermana/hermano 

#pensamos que Título es una variable predictiva
#visualicemos las tasas de supervivencia por título 
#sbisp y #pclass


ggplot(data.combined[1:891, ], aes(x = SibSp, fill = Survived)) +
  facet_wrap(~Pclass+Titulo) +
  geom_histogram(binwidth = 2)+
  ggtitle("Clase Soc. y Título") +
  xlab("Sbisp") +
  ylab("Total") +  
  labs(fill = "Sobrevivientes")

#Tratemos a la variable parch como factor
data.combined$Parch<-as.factor(data.combined$Parch)

ggplot(data.combined[1:891, ], aes(x = Parch, fill = Survived)) +
  geom_histogram(stat="count")+
  facet_wrap(~Pclass+Titulo) +
  ggtitle("Clase Soc. y Título") +
  xlab("Parch") +
  ylab("Total") +  
  ylim(0,300)+
  labs(fill = "Sobrevivientes")
  
  #¿Podremos analizar si el tamaño de la familia es significativo?
temp.sibsp<-c(train$SibSp,test$SibSp)
temp.parch<-c(train$Parch,test$Parch)  
data.combined$tam.fam<-as.factor(temp.parch+temp.sibsp+1)  

#visualicemos para ver si es predictiva


ggplot(data.combined[1:891, ], aes(x = tam.fam, fill = Survived)) +
  facet_wrap(~Pclass+Titulo) +
  geom_histogram(stat="count")+
  ggtitle("Clase Soc. y Título") +
  xlab("Tamaño de la familia") +
  ylab("Total") +  
  ylim(0,300)+
  labs(fill = "Sobrevivientes")