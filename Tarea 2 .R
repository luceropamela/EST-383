#---------- TAREA 2 EST-383 -----------------#
#Usando el enlace adjunto, explore las alternativas respecto los formatos disponibles y cargue la base de datos en R
getwd()
setwd("C:\\Users\\JULIA\\Documents\\PAMELA\\R Segundo semestre\\BASES DE DATOS")
#install.packages("jsonlite")
library(jsonlite)

#install.packages("curl")
library(curl)

url<-"https://datosagt2020.carto.com:443/api/v2/sql?q=select * from public.mun_covid_se36"
covid <- fromJSON(url)
q <- curl_escape("select * from public.mun_covid_se36")
url2 <- paste("https://datosagt2020.carto.com:443/api/v2/sql?q=",q, sep="")
covid <- fromJSON(url2)
bd_covid<-covid[["rows"]]
names(bd_covid)
head(bd_covid)
# La base de datos es:
head(bd_covid)
