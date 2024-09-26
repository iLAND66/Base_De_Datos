#instalamos los paquetes DBI y RSQLite
install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr") #paquete necesaria pra el 2do ejercicio

#llamamos a los paquetes DBI y RSQLite
library(DBI)
library(RSQLite)
library(dplyr) #llamamos el paquete

#creamos la conexion con la Base de datos para eso usamos el comando 
#conexion <- dbConnect(RSQLite::SQLite(), dbname = 
#"aqui va la ruta del archivo si tienes los guiones invertidos ( \ ) cambialos ( / )")
#conexion es el nobre que le estamos asignando a la conexion
conexion <- dbConnect(RSQLite::SQLite(),
                      dbname = "E:/iLAND66/Programacion/DataBase/ejemplo.db")

#esta parte de codigo es para listar todas las tablas que existan en tu data base
#para eso utilizamos dbListTables()
#tablas es el nombre que le estamos asignando
#lo que ponemos en los parentesis es el nombre que le asignamos a la conexion
#si cambias el nomre de la conexion tambien cambia lo que esta en este prentesis
tablas <- dbListTables(conexion)
print(tablas) #imprimimos las tablas que existan en tu base de datos

#Cargamos los datos de la tabla empleados en un dataframe usando dplyr
empleados_df <- tbl(conexion, "empleados") %>%
  collect() # para traer los datos al dataframe

print(head(empleados_df))# imprimimos las primeras filas del dataframe

### ejercicio 2 ###

#punto 2
perez <- tbl(conexion, "empleados") %>%
  filter(apellido == "Perez") %>% #filtra los registros donde la columna apellido sea igual a Perez
  collect()
print(perez)

# Crear la tabla 'clientes'
dbExecute(conexion, "
  CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INTEGER PRIMARY KEY,
    nombre TEXT,
    apellido TEXT,
    email TEXT
  );
")

# Insertar algunos registros de ejemplo en la tabla 'clientes'
dbExecute(conexion, "
  INSERT INTO clientes (nombre, apellido, email) VALUES
  ('Juan', 'Pérez', 'juan.perez@mail.com'),
  ('Ana', 'García', 'ana.garcia@mail.com'),
  ('Luis', 'Pérez', 'luis.perez@mail.com');
")

# Verificar si los datos fueron agregados correctamente
clientes_df <- dbGetQuery(conexion, "SELECT * FROM clientes;")
print(clientes_df)

# Crear la tabla 'empleados'
dbExecute(conexion, "
  CREATE TABLE IF NOT EXISTS empleados (
    id_empleado INTEGER PRIMARY KEY,
    nombre TEXT,
    apellido TEXT,
    puesto TEXT,
    salario REAL
  );
")

# Insertar algunos registros de ejemplo en la tabla 'empleados'
dbExecute(conexion, "
  INSERT INTO empleados (nombre, apellido, puesto, salario) VALUES
  ('Carlos', 'López', 'Gerente', 50000),
  ('María', 'Fernández', 'Asistente', 25000),
  ('Pedro', 'Gómez', 'Analista', 30000);
")

# Verificar si los datos fueron agregados correctamente
empleados_df <- dbGetQuery(conexion, "SELECT * FROM empleados;")
print(empleados_df)

# Crear la tabla 'compras'
dbExecute(conexion, "
  CREATE TABLE IF NOT EXISTS compras (
    id_compra INTEGER PRIMARY KEY,
    id_cliente INTEGER,
    fecha DATE,
    monto REAL
  );
")

# Insertar algunos registros de ejemplo en la tabla 'compras'
dbExecute(conexion, "
  INSERT INTO compras (id_cliente, fecha, monto) VALUES
  (1, '2024-01-15', 100.50),
  (1, '2024-02-20', 250.75),
  (2, '2024-03-05', 320.00),
  (3, '2024-04-12', 180.40),
  (1, '2024-05-22', 500.00);
")

# Verificar si los datos fueron agregados correctamente
compras_df <- dbGetQuery(conexion, "SELECT * FROM compras;")
print(compras_df)


#punto 3
whales <- tbl(conexion, "compras") %>% 
  group_by(id_cliente) %>% #agrupamos por id_cliente
  summarise(total_compras = n()) %>% #contamos el numero de compras por cliente
  filter(total_compras > 15) #filtramos los clientes que han echo mas de 15 compras
print(whales)

#unimos la tabla clientes para obtener el nombre y apellido de los clientes
mas_compran <- whales %>%
  inner_join(tbl(conexion, "clientes"), by = "id_cliente") %>% #unimos la tablas compras y clientes
  select(nombre, apellido) %>% #Seleccionar: Usamos select() para quedarnos solo 
                               #con las columnas nombre y apellido.
  collect()

print(mas_compran)
# suponemos que tenemos una tabla llamada compras que tiene una columna id_cliente
# y una tabla clientes que tiene columnas nombre, apellido e id_cliente

dbDisconnect(conexion)# desconectamos de la ase de datos
