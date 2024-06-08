library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
# Funcion de conexion
conexion_BD<-function(host_db,name_db,user_db,pass_db,port_db)
{
  con <- dbConnect(RPostgres::Postgres(),
                   host = host_db, 
                   dbname = name_db,
                   user = user_db,
                   password = pass_db,
                   port = port_db)
  return(con)
}

# RECOPILAR CONJUNTO DE DATOS
conex=conexion_BD("127.0.0.1","Urgencias","postgres","rootpostgres","5432")

dbListTables(conex)
atenUr=dbGetQuery(conex,"Select * from atencionesurgencia2023")


#ORGANIZAR
dim(atenUr)
head(atenUr)
str(atenUr)
glimpse(atenUr)
#Revison datos faltantes
is.na(atenUr)
summary(atenUr)
na <- is.na(atenUr)

#obtener variable id establecimiento

comPmont<- filter(atenUr, nombrecomuna == "Puerto Montt" & !is.na(idestablecimiento))

View(comPmont)
glimpse(comPmont)
hist(comPmont$total)
summary(comPmont$total)
