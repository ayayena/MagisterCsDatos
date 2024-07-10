library(RPostgreSQL)
library(readr)
library(dplyr)
library(ggplot2)
##############DIRECTORIO #############################################################################
setwd("D:/Magister Ciencias Datos UDLA/Clases R/Practicos R/MagisterCsDatos/")
source("D:/Magister Ciencias Datos UDLA/Clases R/Practicos R/MagisterCsDatos/funcionesEvaluacion.R")
#############COMIENZO SCRIPT #########################################################################
repositorio="local"
atenUrgencia=data.frame()
if(repositorio == "DB")
{
   conex=recolecion_Datos("localhost","5432","Urgencias","postgres","rootpostgres")
   #dbListTables(conex)
   atenUrgencia=dbGetQuery(conex, 'select * from AtencionesUrgencia2023')
}else
{
   
   
   AtenUrgencia <- read_delim("AtencionesUrgencia2023.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
   
}
######################## DESCRIPCION DEL CONJUNTO DE DATOS#################################

# Se obtiene el nombre de la variables
names(AtenUrgencia)

# Se visualiza el data set que contien los datos de las atenciones
View(AtenUrgencia)

# Se obtiene un resumen de las variables que tiene el data set
glimpse(AtenUrgencia)

#se obtiene los tipos de datos que contiene el dataset de atenciones 
str(AtenUrgencia)



######################## PREPROCESAMIENTO DE DATOS########################################


#se tranforma el data set en tipo de datos dataframe
dataAten<-data.frame(AtenUrgencia)

#veirificar datos nulos
dnulos=verificaNulos(dataAten)
dnulos
if (dnulos == FALSE)
{
  #busca registros nulos y remplaza con cero en variable numericas
  dataAten$Total<-replace(dataAten$Total,is.null(dataAten$Total),0)
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
  dataAten$Total<-replace(dataAten$Total,is.null(dataAten$Total),0)
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

#################VARIABLES CATEGORICAS###########################

#Definicion Varibales Categoricas
#obnter el conjunto de observacionde de las comunas con dependencia  del servico salud del reloncavi
dataRegion=dataAten%>%filter(CodigoDependencia == 24)
dataRegion

dataRegion$CodigoComuna=factor(dataRegion$CodigoComuna)

tregion<-table(dataRegion$CodigoComuna)
tregion
dfregion=data.frame(tregion)
dfregion

glimpse(dataRegion)
ggplot(dataRegion, aes(x = CodigoComuna)) +                      
  geom_bar(width=0.5, colour="#eb3c46", fill="#eb3c46")+
  ggtitle("Diagrama de Atenciones Urgencias 2023 por Comuna")  +
  labs(x = NULL, y = "Número de observaciones") 

#obtener variable categorica pr fecha
dataRegion$fecha=factor(dataRegion$fecha)
tfecha=table(dataRegion$fecha)
tfecha
dfecha=data.frame(tfecha)
dfecha
ggplot(dfecha, aes(x = Var1, y=Freq)) +       
  geom_line()+
  geom_point(colour="#eb3c46")+
  labs(x = "Fechas", y = "Frecuencia de Atenciones", 
     title = "Diagrama de Atenciones Urgencias 2023 por Fecha")
  

#geom_line(width=0.5, colour="#e94860", fill="#e94860")+
  #ggtitle("Diagrama de Atenciones Urgencias 2023 por Comuna")  

#obtener el conjunto de datos de la establecmientos de la dependencia del reloncavi
dataRegion$IdEstablecimiento=factor(dataRegion$IdEstablecimiento)
testablecmiento<-table(dataRegion$IdEstablecimiento)
testablecmiento

ggplot(dataRegion, aes(x = IdEstablecimiento)) +                      
  geom_bar(width=0.5, colour="#e94860", fill="#e94860")+
  ggtitle("Diagrama de barras")  +
  labs(x = "Establecimiento", y = "Frecuencia de Atenciones", 
       title = "Diagrama de Atenciones Urgencias 2023 por Establecimiento")

#################VARIABLES CUANTITATIVASN###########################

#Definicion Variables Cuantitativas
#variable de total
ttotal=table(dataRegion$Total)
ttotal
ggplot(dataRegion, aes(x=Total)) + 
  geom_histogram(color="#eb3c46", fill="#d3def2")+
  labs(x = NULL, y = "Número de observaciones",title = "Diagrama Total de Atenciones") 

#variable de menores de   1 año
ggplot(dataRegion, aes(x=Menores_1)) + 
  geom_histogram(color="#eb3c46", fill="#d3def2")+
labs(x = NULL, y = "Número de observaciones",title = "Diagrama Total de Atenciones Menores de 1 año") 

#variable de menores de   1 año
ggplot(dataRegion, aes(x=De_1_a_4)) + 
  geom_histogram(color="#eb3c46", fill="#d3def2")
  
#################MEDIDAS DE TENDENCIA Y POSICION###########################
#medidas de tendencia central media, moda, mediana
glimpse(dataRegion$Total)
#variable total
summary(dataRegion$Total)
modatotal<-Moda(dataRegion$Total)
modatotal
desvtotal=sd(dataRegion$Total) 
desvtotal
#variable menores de 1 año
summary(dataRegion$Menores_1)
modam1<-Moda(dataRegion$Menores_1)
modam1
desvm1=sd(dataRegion$Menores_1) 
desvm1

######################################################################3
# Algoritmo de cluster de K-means
#crear dataset para algoritmo
class(dataRegion)
dataRegion


gruposcomunast=aggregate(Total ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunast)
gruposcomunasm1=aggregate(Menores_1 ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunasm1)
gruposcomunasm1a4=aggregate(De_1_a_4 ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunasm1a4)
gruposcomunasm5a14=aggregate(De_5_a_14 ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunasm5a14)
gruposcomunasm15a64=aggregate(De_15_a_64 ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunasm15a64)
gruposcomunasm65ymas=aggregate(De_65_y_mas ~ NombreComuna, data=dataRegion, FUN = sum)
class(gruposcomunasm65ymas)

grupo1=merge(x = gruposcomunas, y = gruposcomunasm1)
grupo1
grupo2=merge(x = grupo1, y = gruposcomunasm1a4)
grupo2
grupo3=merge(x = grupo2, y = gruposcomunasm5a14)
grupo3
grupo4=merge(x = grupo3, y = gruposcomunasm15a64)
grupo4
grupokm=merge(x = grupo4, y = gruposcomunasm65ymas)
grupokm

ciudades=factor(grupokm$NombreComuna)
ciudades
rownames(grupokm)=ciudades

dfkm=grupokm %>% select(Total, Menores_1, De_65_y_mas)

totalidad = scale(dfkm, center = TRUE, scale = TRUE)
summary(inseguridad)

totalidad= as.data.frame(totalidad)



#creamos 4 cluster en funcion a su grado de inseguridad

kmcluster = kmeans(totalidad,centers=4,nstart = 50)
kmcluster

#graficamos los cluster en funcion del %total  y %menores de 1 año

totalidad = totalidad %>% mutate(cluster = kmcluster$cluster)

(g1=ggplot(totalidad, aes(x = Total, y = Menores_1)) +
    geom_point(aes(color=as.factor(cluster)), size=10)+
    geom_text(aes(label = cluster), size = 5) +
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "Kmenas con k=4") 
)


(g1=ggplot(totalidad, aes(x = Total, y = De_65_y_mas)) +
    geom_point(aes(color=as.factor(cluster)), size=10)+
    geom_text(aes(label = cluster), size = 5) +
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "Kmenas con k=4") 
)
