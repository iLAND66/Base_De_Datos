#instalamos los paquetes DBI y RSQLite
install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr") #paquete necesaria pra el 2do ejercicio

#llamamos a los paquetes DBI y RSQLite
library(DBI)
library(RSQLite)
library(dplyr) #llamamos el paquete

nombres_hombres <- c(
  "Aaron", "Abel", "Abraham", "Adam", "Adrian", "Aiden", "Alan", "Albert", "Alec", "Alexander",
  "Alfred", "Andre", "Andres", "Angelo", "Anthony", "Antonio", "Arthur", "Asher", "Austin", "Benjamin",
  "Bernard", "Blake", "Boris", "Brad", "Brandon")

nombres_mujeres <- c(
  "Ada", "Adele", "Adriana", "Aileen", "Aisha", "Alicia", "Amara", "Amelia", "Ana", "Andrea",
  "Angela", "Anna", "Ariana", "Astrid", "Aurora", "Beatrice", "Bella", "Bianca", "Blanca", "Brianna",
  "Camila", "Carla", "Caroline", "Carmen", "Cecilia")

apellidos <- c(
  "Adams", "Allen", "Anderson", "Armstrong", "Baker", "Bates", "Bell", "Bennett", "Benson", "Berg",
  "Black", "Blake", "Bond", "Booth", "Bowers", "Boyd", "Bradley", "Brooks", "Brown", "Bryant",
  "Burke", "Burns", "Butler", "Campbell", "Carter", "Chapman", "Clark", "Collins", "Cook", "Cooper",
  "Cox", "Craig", "Cross", "Curtis", "Davis", "Day", "Dean", "Diaz", "Dixon", "Douglas",
  "Duncan", "Edwards", "Ellis", "Evans", "Fisher", "Fitzgerald", "Foster", "Fox", "Franklin", "Garcia")

actividades_academicas <- c(
  "Taller de Escritura Creativa",
  "Clase Magistral de Matematicas Avanzadas",
  "Seminario de Fisica Cuantica",
  "Curso de Programacion en Python",
  "Conferencia de Inteligencia Artificial",
  "Mesa Redonda sobre Cambio Climatico",
  "Taller de Resolucion de Problemas",
  "Curso de Estadistica Aplicada",
  "Charla sobre Etica Profesional",
  "Laboratorio de Quimica Experimental",
  "Estudio de Caso en Economia",
  "Clinica de Diseno Grafico",
  "Workshop de Emprendimiento",
  "Taller de Oratoria y Debate",
  "Seminario sobre Literatura Clasica",
  "Practicas de Biologia Molecular",
  "Simposio de Ingenieria Civil",
  "Curso de Historia del Arte",
  "Taller de Redaccion Cientifica",
  "Conferencia sobre Energias Renovables"
)

actividades_recreativas <- c(
  "Caminata en la naturaleza",
  "Taller de pintura al oleo",
  "Clase de yoga",
  "Sesion de meditacion guiada",
  "Torneo de videojuegos",
  "Taller de cocina internacional",
  "Cine al aire libre",
  "Maraton de series",
  "Jornada de fotografia",
  "Clase de baile salsa",
  "Taller de ceramica",
  "Busqueda del tesoro",
  "Concurso de karaoke",
  "Clase de defensa personal",
  "Torneo de ajedrez",
  "Sesion de lectura al aire libre",
  "Clases de jardineria",
  "Taller de escritura creativa",
  "Escalada en muro",
  "Torneo de voleibol",
  "Excursion en bicicleta",
  "Dia de juegos de mesa",
  "Taller de origami",
  "Clase de musica con ukelele",
  "Carrera de obstaculos",
  "Dia de picnic comunitario",
  "Taller de maquillaje artistico",
  "Maraton de peliculas",
  "Cata de chocolates",
  "Taller de costura y bordado",
  "Juegos acuaticos",
  "Festival de talentos",
  "Taller de teatro improvisado",
  "Noche de astronomia",
  "Clase de natacion",
  "Taller de bricolaje",
  "Yoga en pareja",
  "Sesion de dibujo al natural",
  "Campeonato de ping pong",
  "Competencia de rompecabezas"
)

actividades_sociales <- c(
  "Cena comunitaria",
  "Reunion de bienvenida",
  "Evento de intercambio cultural",
  "Fiesta tematica de disfraces",
  "Reunion de networking",
  "Cafecito literario",
  "Barbacoa al aire libre",
  "Evento de intercambio de libros",
  "Tarde de bingo",
  "Celebracion de cumpleanos grupal",
  "Taller de voluntariado",
  "Reunion de planificacion de eventos",
  "Fiesta de ano nuevo",
  "Reunion de recaudacion de fondos",
  "Encuentro de antiguos alumnos",
  "Taller de inclusion social",
  "Charla motivacional grupal",
  "Convivencia con juegos de integracion",
  "Fiesta sorpresa de agradecimiento",
  "Mesa redonda sobre liderazgo",
  "Encuentro intergeneracional",
  "Cena de gala",
  "Cafe filosofico",
  "Tarde de cine y conversacion",
  "Mercado de trueque",
  "Encuentro tematico cultural",
  "Jornada de limpieza comunitaria",
  "Campana de donacion de ropa",
  "Fiesta de despedida",
  "Rally de integracion",
  "Festival de danza comunitaria",
  "Feria de emprendimiento social",
  "Encuentro deportivo amistoso",
  "Taller de liderazgo juvenil",
  "Evento de adopcion de mascotas",
  "Fiesta en la playa",
  "Celebracion del dia de la amistad",
  "Jornada de reflexion grupal",
  "Dia de agradecimiento comunitario",
  "Noche de talentos"
)

generadorNombre <- function (nombres, apellidos, tamanoDelVector) {
  n <<- tamanoDelVector
  segundoNombre <- sample(nombres, n, replace = T)
  primerNombre <- sample(nombres, n, replace = T)
  segundoApellido <- sample(apellidos, n, replace = T)
  primerApellidos <- sample(apellidos, n, replace = T)
  nombresVacios <- rep("", round(length(nombres)/6))
  fullName <- gsub(" +", " ", paste(primerNombre, segundoNombre, primerApellidos, segundoApellido))
  return(fullName)
}

generadorCorreos <- function(fullName) {
  partes <- strsplit(fullName, " ")[[1]]
  primer_nombre <- partes[1]
  primer_apellido <- partes[length(partes)-1]
  segundo_apellido <- partes[length(partes)]
  correo <- paste0(substr(primer_nombre, 1, 1),
                   primer_apellido,
                   substr(segundo_apellido, 1, 1),
                   "@ipn.mx")
  correo <- tolower(correo)
  return(correo)
}

nombresHombres <- generadorNombre(nombres_hombres, apellidos, 5)
correosMasc <- sapply(nombresHombres, generadorCorreos)
generoMasculino <- rep("Masculino", length(correosMasc))

nombresMujeres <- generadorNombre(nombres_mujeres, apellidos, 5)
correosFem <- sapply(nombresMujeres, generadorCorreos)
generoFemenino <- rep("Femenino", length(correosFem))

dfStudents <- data.frame(
    Nombre = c(nombresHombres, nombresMujeres),
    Genero = c(generoMasculino, generoFemenino),
    Correo = c(correosMasc, correosFem)
  )

dfStudents$Edad <- sample(18:40, size = nrow(dfStudents), replace = T)

actividades <- c(actividades_academicas, actividades_recreativas, actividades_sociales)
dfStudents$Titulo <- sample(actividades, size = nrow(dfStudents), replace = T)

start_date <- as.Date("2024-01-01")
fechas <- seq(from = start_date, by = "year", length.out = nrow(dfStudents))
dfStudents$Fecha <- sample(fechas, size = nrow(dfStudents), replace = T)

dfStudents$Duracion <- sample(actividades, size = nrow(dfStudents), replace = T)

generarDuracion <- function (fecha){
  anio <- format(fecha, "%y")
  meses <- sample(0:5, 1)
  dias <- sample(0:30, 1)
  fechaInicio <- as.Date(paste(anio, sample(1:12, 1), 1, sep = "-"))
  fechaTermino <- fechaInicio + months(meses) + days(dias)
  return(c(fechaInicio, fechaTermino))
}
