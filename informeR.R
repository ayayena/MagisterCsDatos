#--------------PAQUETES A USAR
#--Directorio de trabajo
setwd("C:/Users/Lenovo/Desktop/Magister Ciencia Datos/Informe R/")
##-------------IMPORTAR DATOS
#lectura de datos Atenciones Urgencias 2023
#mars=read.csv(file="AtencionesUrgencia2023.csv",sep=";",header=TRUE,quote="")
#nrow(mars)


mars2=read.csv(file="AtencionesUrgencia2023.csv",sep=";",header=TRUE)
#nrow(mars2)

##análisis exploratorio de datos
##--------------EXPLORACION DE DATOS
view(mars2)
str(mars2)#informacion del datframe

class(mars2) #VER DATOS
dim(mars2) #vER DIMENSION DATOS
head(mars2)#ver primeras filas
tail(mars2)#ver ultimas filas

summary(mars2)

#Buscar datos vacios
is.na(mars2)
##TRANSFORMAR DATOS



##vISUALIZAR
hist(x=mars2$Total,main = "Histograma Totales Atenciones Urgencia 2024", xlab = "Total", ylab = "Frecuencia")

plot(mars2$Total)
##MODELAR

##COMUNICAR