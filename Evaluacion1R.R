library(RPostgreSQL)
library(readr)
library(dplyr)
library(ggplot2)
###############################################################################################
#establecerconexion a la base de datos
recolecion_Datos<-function(host,port,dbname,user, passd)
{
  dvr <- PostgreSQL()
  
  db <- dbname  ##Nombre de la BBDD
  #host_db <- '10.6.43.108'
  host_db<-host
  db_port <- port 
  db_user <- user  ##Tu usuario
  db_password <- passd ##Tu contraseña 
  
  con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port,
                   user=db_user, password=db_password)  
  return(con)
}


#obtener list de nombres dataframe
listaNombres<-function(df)
{
  nombres=names(df) 
  return(nombres)
}

#verificar nulos
verificaNulos<-function(df)
{
  cond <- c(names(df))
  cond
  dnulos=df %>% filter(any(is.na()))
  return(dnulos)
}
#verificar na
verificaNA<-function(df)
{
  dna=anyNA(df)
   
  return(dna)
}
verificaCaracteres<-function(varia)
{
  if (is.numeric(varia))
  {
    respuesta=TRUE
   
  }
  else
  {
    respuesta=FALSE
  }
  return(respuesta)
}
##############################################################################################
repositorio="local"
atenUrgencia
if(repositorio == "DB")
  {
   conex=recolecion_Datos("localhost","5432","Urgencias","postgres","rootpostgres")
   #dbListTables(conex)
   atenUrgencia=dbGetQuery(conex, 'select * from AtencionesUrgencia2023')
  }
else
  {
   
   setwd("D:/Magister Ciencias Datos UDLA/Clases R/Practicos R/MagisterCsDatos/")
   AtenUrgencia <- read_delim("AtencionesUrgencia2023.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
   
 }
######################## DESCRIPCION DEL CONJUNTO DE DATOS#################################
###########################################################################################
# Se obtiene el nombre de la variables
names(AtenUrgencia)

# Se visualiza el data set que contien los datos de las atenciones
View(AtenUrgencia)

# Se obtiene un resumen de las variables que tiene el data set
glimpse(AtenUrgencia)

#se obtiene los tipos de datos que contiene el dataset de atenciones 
str(AtenUrgencia)



######################## PREPROCESAMIENTO DE DATOS########################################
###########################################################################################

#se tranforma el data set en tipo de datos dataframe
dataAten<-data.frame(AtenUrgencia)

#veirificar datos nulos
dnulos=verificaNulos(dataAten)
dnulos
if (dnulos == FALSE)
{
  #busca registros nulos y remplaza con cero en variable numericas
  dataAten$Total<-replace(dataAten$Total,is.nul(dataAten$Total),0)
  dataAten$Menores_1<-replace(dataAten$Menores_1,is.null(dataAten$Menores_1),0)
  dataAten$De_1_a_4-replace(dataAten$De_1_a_4,is.null(dataAten$De_1_a_4),0)
  dataAten$De_5_a_14<-replace(dataAten$De_5_a_14,is.null(dataAten$De_5_a_14),0)
  dataAten$De_15_a_64<-replace(dataAten$De_15_a_64,is.null(dataAten$De_15_a_64),0)
  dataAten$De_65_y_mas<-replace(dataAten$De_65_y_mas,is.null(dataAten$De_65_y_mas),0)
}
   
#verifica na
dna = verificaNA(dataAten)
dna
if(dna == FALSE)
{
   #busca registros na
   which(is.na(dataAten))
  #busca registros nulos y remplaza con cero en variable numericas
  dataAten$Total<-replace(dataAten$Total,is.nul(dataAten$Total),0)
  dataAten$Menores_1<-replace(dataAten$Menores_1,is.null(dataAten$Menores_1),0)
  dataAten$De_1_a_4-replace(dataAten$De_1_a_4,is.null(dataAten$De_1_a_4),0)
  dataAten$De_5_a_14<-replace(dataAten$De_5_a_14,is.null(dataAten$De_5_a_14),0)
  dataAten$De_15_a_64<-replace(dataAten$De_15_a_64,is.null(dataAten$De_15_a_64),0)
  dataAten$De_65_y_mas<-replace(dataAten$De_65_y_mas,is.null(dataAten$De_65_y_mas),0)
}

#verificar caracter 0 en variables numericas
total=dataAten$Total
total
class(total)
vtotal=verificaCaracteres(dataAten$Total)
vtotal
if(vtotal == FALSE)
{
  dataAten$Total<-replace(dataAten$Total,is.nul(dataAten$Total),0)
}

vm1=verificaCaracteres(dataAten$Menores_1)
vm1
if(vm1 == FALSE)
{
  dataAten$Menores_1<-replace(dataAten$Menores_1,is.null(dataAten$Menores_1),0)
}

vm1y4=verificaCaracteres(dataAten$De_1_a_4)
vm1y4
if(vm1y4 == FALSE)
{
  dataAten$De_1_a_4-replace(dataAten$De_1_a_4,is.null(dataAten$De_1_a_4),0)
}

vm5y14=verificaCaracteres(dataAten$De_5_a_14)
vm5y14
if(vm5y14 == FALSE)
{
  dataAten$De_5_a_14<-replace(dataAten$De_5_a_14,is.null(dataAten$De_5_a_14),0)
}
vm15y64=verificaCaracteres(dataAten$De_15_a_64)
vm15y64
if(vm15y64 == FALSE)
{
  dataAten$De_15_a_64<-replace(dataAten$De_15_a_64,is.null(dataAten$De_15_a_64),0)
}
vm65ymas=verificaCaracteres(dataAten$De_65_y_mas)
vm65ymas
if(vm65ymas== FALSE)
{
  dataAten$De_65_y_mas<-replace(dataAten$De_65_y_mas,is.null(dataAten$De_65_y_mas),0)
}

# Quitar caracteres especiales de la variable nombre de communa

dataAten$NombreComuna<-replace(dataAten$NombreComuna,dataAten$NombreComuna == "Maull\xedn","Maullin")
dataAten$NombreComuna<-replace(dataAten$NombreComuna,dataAten$NombreComuna == "Chait\xe9n","Chaiten") 
dataAten$NombreComuna<-replace(dataAten$NombreComuna,dataAten$NombreComuna == "Futaleuf\xfa","Futaleufu")
dataAten$NombreComuna<-replace(dataAten$NombreComuna,dataAten$NombreComuna == "Cocham\xf3","Cochamo")
dataAten$NombreComuna<-replace(dataAten$NombreComuna,dataAten$NombreComuna == "Hualaihu\xe9","Hualaihue")
 
# Quitar caracteres especiales de la variable nestablecimiento

dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "Hospital de Maull\xedn","Hospital de Maullin")
dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "Hospital de Futaleuf\xfa","Hospital de Futaleufu")
dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "Hospital de Chait\xe9n","Hospital de Chaiten")
dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "Centro de Salud Familiar R\xedo Negro Hornopir\xe9n","Centro de Salud Familiar Rio Negro Hornopiren")
dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "SUR Cocham\xf3 - R\xedo Puelo","Sur Cochamo - Rio Puelo")
dataAten$NEstablecimiento<-replace(dataAten$NEstablecimiento,dataAten$NEstablecimiento == "SAPU Angelm\xf3","Sapu Angelmo")
###########################
#obnter el conjunto de observacionde de las comunas con dependencia  del servico salud del reloncavi
dataRegion=dataAten%>%filter(CodigoDependencia == 24)
dataRegion
tregion<-table(dataRegion$NombreComuna)
tregion
glimpse(dataRegion)
ggplot(dataRegion, aes(x = NombreComuna)) +                      
  geom_bar(width=0.5, colour="red", fill="skyblue")+
  ggtitle("Diagrama de barras")  


#obtener el conjunto de datos de la dependencia del reloncavi
testablecmiento<-table(dataRegion$NEstablecimiento)
testablecmiento
glimpse(testablecmiento)

ggplot(dataRegion, aes(x = NEstablecimiento)) +                      
  geom_bar(width=0.5, colour="red", fill="skyblue")+
  ggtitle("Diagrama de barras")  

