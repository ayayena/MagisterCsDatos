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
#listaNombres<-function(df)
#{
#  nombres=names(df) 
#  return(nombres)
#}

#verificar nulos
verificaNulos<-function(df)
{
  #cond <- c(names(df))
  #cond
  dnulos=any(is.na(df))
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
Moda = function(x) {  #G) Otra función para hallar la moda
  q <- unique(x)
  q[which.max(tabulate(match(x, q)))]
}
