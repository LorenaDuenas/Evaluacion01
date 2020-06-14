#### Datos generales ####

#Estudiante: Lorena Dueñas
#Años de Precesamiento: Agoato 2018 y 2019

#### Configuraciones iniciales ####
getwd()
setwd("D:/Disco_LorenaD_2020/003_Cursos/017_CursoR_Uni/Evaluacion01/Codigo")
dir()
help(help.search)
sessionInfo()
installed.packages()
ls()
rm()
options()
history()
q()
rm(list = ls())

#### Notas importantes ####
# Vector

xnum <- c(1,2,3,4)
xchart <- c(TRUE, FALSE,TRUE)
xchart <- c("lorena","duenas","Soto","Huanuco")

suma.promedio <- sum(xnum)/length(xnum)
mean(xnum)
var(xnum)
lista <- list(fila="lorena","duenas","Soto","Huanuco", columna="a","b","c","d","e","f","g","h")

#Objetos en R
#numeric> enteros con precision 2
#complex
#character
#logical
#NA: not available, 
#por defecto en x funcion na.rm=FALSE no elimina los NA
#x funcion na.rm=TRUE la operacion se efectua con datos validos

#Matrices,se crea por columnas, pero si byrow=TRUE lo hace x filas
matrix(xnum,5)
m <- matrix(data= xnum, nrow = 4, ncol = 8, byrow = FALSE, dimnames = list(xchart,c("a","b","c","d","e","f","g","h")))
m[1,2]
ncol(m)
nrow(m)
t(m)


#### Descarga de informacion de datos ####
#Verificamos que hay en el directorio
dir()
#Cargamos la data
OsAgosto2018 <- read.table("201808_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                           col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
                           colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))
#Verificamos el tipo de data
class(OsAgosto2019)
str(OsAgosto2019)
dim(OsAgosto2019)

#asiganamos su variable y le cmbiamos los nombres de las variables
OsAgosto2019 <- read.table("201908_TABLA4.txt", header = TRUE,sep = "\t",
                           col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
                           colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))


####Conversion de la variable fecha, agregamos un obejto que tiene informacion de fecha y tiempo ####
library(lubridate)

OsAgosto2018$FechaDate <- ymd_hm(OsAgosto2018$Fecha)
View(OsAgosto2018$FechaDate)
View(OsAgosto2018)
class(OsAgosto2018$FechaDate)

OsAgosto2019$FechaDate <- ymd_hm(OsAgosto2019$Fecha)
View(OsAgosto2019$FechaDate)
class(OsAgosto2019$FechaDate)

#Usamos el paquete de libreria base
OsAgosto2018$Year <- format(OsAgosto2018$FechaDate,"%Y")
OsAgosto2018$month <- format(OsAgosto2018$FechaDate,"%m")
OsAgosto2018$day <- format(OsAgosto2018$FechaDate,"%d")
OsAgosto2018$hour <- format(OsAgosto2018$FechaDate,"%H")
OsAgosto2018$minute <- format(OsAgosto2018$FechaDate,"%M")

OsAgosto2019$Year <- format(OsAgosto2019$FechaDate,"%Y")
OsAgosto2019$month <- format(OsAgosto2019$FechaDate,"%m")
OsAgosto2019$day <- format(OsAgosto2019$FechaDate,"%d")
OsAgosto2019$hour <- format(OsAgosto2019$FechaDate,"%H")
OsAgosto2019$minute <- format(OsAgosto2019$FechaDate,"%M")

#Usamos funciones de lubridate
year(OsAgosto2018$FechaDate)
day(OsAgosto2018$FechaDate)
month(OsAgosto2018$FechaDate)

year(OsAgosto2019$FechaDate)
day(OsAgosto2019$FechaDate)
month(OsAgosto2019$FechaDate)

#Funciones de analisis procesar por anio, comportamiento 
#por mes, podemos agrupar , calcular media, maxima , minimos

#### Procesamiento ####

#Plotear la serie
plot(OsAgosto2018)
plot(OsAgosto2019)

# Recordar que las variables son :
# Código de la Empresa Eléctrica Suministradora	
# Código del Suministro del Usuario Libre	
# Código del Punto de Suministro	
# Fecha (AAAAMMDDHHMM)	
# Registro de Energía Activa en kW.h	
# Registro de Energía Reactiva en kVarh	
# Periodo

str(OsAgosto2018)

#Limpiamos la data, vemos existencia NA en la columna Suministro
sum(is.na(OsAgosto2018$Suministro))

OsAgosto2018 <- na.omit(OsAgosto2018)

str(OsAgosto2019)

#Limpiamos la data, vemos existencia NA en la columna Suministro
sum(is.na(OsAgosto2019$Suministro))

OsAgosto2019 <- na.omit(OsAgosto2019)

####Graficos de dispersion ####

#Primer y ultimo dia de data a;o 2018
head(OsAgosto2018, n=1)$FechaDate
tail(OsAgosto2018, n=1)$FechaDate

#EPrimer y ultimo dia de data a;o 2019
head(OsAgosto2019, n=1)$FechaDate
tail(OsAgosto2019, n=1)$FechaDate

#### Usamos libreria graphics ####
plot(x= OsAgosto2018$day, y=OsAgosto2018$Suministro,
     xlab = paste ("Fecha[" , head(OsAgosto2018,n=1)$day, "-",tail(OsAgosto2018, n=1)$day,"]"),
     ylab = "Suministro",
     main = "Suministro de Energia - Osinerming Agosto 2018")

plot(x= OsAgosto2019$day, y=OsAgosto2019$Suministro,
     xlab = paste ("Dia mes de agosto[" , head(OsAgosto2019,n=1)$day, "-",tail(OsAgosto2019, n=1)$day,"]"),
     ylab = "Suministro",
     main = "Suministro de Energia - Osinerming Agosto 2019",
     Sub = "Fuente: https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria ")
dev.off()

#### habgamos el grafico en ggplot ####
library(ggplot2)
ggplot(data = OsAgosto2018, mapping = aes(x= day, y = Suministro )+
         geom_lines()+
         xlab(paste("Dia [", head(OsAgosto2018, n=1)$Day, "-", tail(OsAgosto2018, n=1)$day, "]"))+
         ylab("Suministro")+
         ggtitle("Suministro del mes de agosto del 2018")+
         labs(subtitle="Fuente: https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
              caption ="Evaluacion 01" )
       
ggplot(data = OsAgosto2019, mapping = aes(x= day, y = Suministro )+
         geom_lines()+
         xlab(paste("Dia [", head(OsAgosto2019, n=1)$Day, "-", tail(OsAgosto2018, n=1)$day, "]"))+
         ylab("Suministro")+
         ggtitle("Suministro del mes de agosto del 2019")+
         labs(subtitle="Fuente: https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria",
               caption ="Evaluacion 01" )
              














