#instalamos los paquetes
install.packages("DBI")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")

#llamamos a los paquetes
library(DBI)
library(dplyr)
library(ggplot2)
library(caret)

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
  n <- tamanoDelVector
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

#dfStudents$Tipo <- sample(actividades, size = nrow(dfStudents), replace = T)

generarDuracion <- function (fecha){
  anio <- format(fecha, "%y")
  meses <- sample(0:5, 1)
  dias <- sample(0:30, 1)
  fechaInicio <- fecha
  fechaTermino <- fechaInicio + (meses * 30) + dias
  return(paste(fechaInicio, "a", fechaTermino))
}

dfStudents$Duracion <- sapply(dfStudents$Fecha, generarDuracion)

dfStudents$id_estudiantes <- 1:nrow(dfStudents)

dfActividades <- data.frame(
  id_actividad = 1:length(actividades),
  Titulo = actividades,
  Tipo = c(rep("Academica", length(actividades_academicas)),
           rep("Recreativa", length(actividades_recreativas)),
           rep("Social", length(actividades_sociales)))
)

num_interacciones <- 50
dfInteracciones <- data.frame(
  id_estudiantes = sample(dfStudents$id_estudiantes, num_interacciones, replace = T),
  id_actividad = sample(dfActividades$id_actividad, num_interacciones, replace = T),
  Fecha = sample(seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), num_interacciones, replace = T)
)

#Manipulacion
#Mas de 5 Actividades
estudiantes_actividades <- dfInteracciones %>%
  group_by(id_estudiantes) %>%
  summarise(total_actividades = n()) %>%
  filter(total_actividades > 5)

estudiantes_con_mas_de_5 <- estudiantes_actividades %>%
  inner_join(dfStudents, by = "id_estudiantes") %>%
  select(Nombre, Genero, Correo, total_actividades)

#Actividades populares
dfActividades <- dfActividades %>%
  mutate(Tipo = case_when(
    Titulo %in% actividades_academicas ~ "Academica",
    Titulo %in% actividades_recreativas ~ "Recreativa",
    Titulo %in% actividades_sociales ~ "Social",
    TRUE ~ "Otro"
  ))

actividades_populares <- dfInteracciones %>%
  inner_join(dfActividades, by = "id_actividad") %>%
  group_by(Tipo, Titulo) %>%
  summarise(total_participantes = n(), .groups = "drop") %>%
  arrange(Tipo, desc(total_participantes))

#porcentaje de estudiantes por genero en cada tipo de actividad
datos_completos <- dfInteracciones %>%
  inner_join(dfStudents, by = "id_estudiantes") %>%
  inner_join(dfActividades, by = "id_actividad") %>%
  select(id_estudiantes, Nombre, Genero, Edad, Titulo, Tipo)

porcentaje_genero <- datos_completos %>%
  group_by(Tipo, Genero) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(Tipo) %>%
  mutate(porcentaje = (total / sum(total)) * 100) %>%
  arrange(Tipo, desc(porcentaje))


#agregar columnas calculadas, como la duracion promedio de actividades por estudiante
#esta calculada la duracion en dias
datos_completos <- datos_completos %>%
  mutate(
    Inicio = as.Date(sub(" a .*", "", Duracion)),
    Termino = as.Date(sub(".* a ", "", Duracion)),
    Duracion_dias = as.numeric(Termino - Inicio) 
  )

duracion_promedio_estudiantes <- datos_completos %>%
  group_by(id_estudiantes) %>%
  summarise(
    Nombre = first(Nombre),
    Duracion_promedio = mean(Duracion_dias, na.rm = TRUE),
    total_actividades = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Duracion_promedio))

#Graficos
ggplot(duracion_promedio_estudiantes, aes(x = reorder(Nombre, total_actividades), y = total_actividades)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Total de Actividades por Estudiante",
    x = "Estudiante",
    y = "Número de Actividades"
  ) +
  theme_minimal()

#tabla resumen

head(dfInteracciones %>%
       inner_join(dfStudents, by = "id_estudiantes") %>%
       inner_join(dfActividades, by = "id_actividad"))
head(dfActividades)

tabla_resumen <- dfInteracciones %>%
  inner_join(dfStudents, by = "id_estudiantes") %>%
  inner_join(dfActividades, by = "id_actividad") %>%
  rename(Tipo = Tipo.y) %>% # Asegura que usamos el Tipo de actividades
  group_by(id_estudiantes) %>%
  summarise(
    Nombre = first(Nombre),
    Genero = first(Genero),
    Edad = first(Edad),
    total_actividades = n(),
    recreativas = sum(Tipo == "Recreativa", na.rm = TRUE),
    sociales = sum(Tipo == "Social", na.rm = TRUE),
    academicas = sum(Tipo == "Academica", na.rm = TRUE)
  )

#variable objetivo
tabla_resumen <- tabla_resumen %>%
  mutate(
    participa_recreativa = ifelse(recreativas > 0, 1, 0)
  )

table(tabla_resumen$participa_recreativa)
