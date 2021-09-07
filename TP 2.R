# Trabajo Practico Nº2 - Scrapeo


# Primero vamos a cargar las librerias necesarias para realizar el trabajo. En primer lugar vamos a cargars tidyverse y luego para este caso puntual sera necesario que instalaemos y carguemos rvest.
# Rvest puntualmente nos va a permitir extraer datos de las paginas webs.

#install.packages("tidyverse")
library(tidyverse) # "No used functions found"
#install.packages(rvest)
library(rvest) # read_html
library(stringr)

# Para este trabajo vamos a buscar datos relacionados a los casos de Covid19 para la ciudad de Buenos Aires

url <- "https://es.wikipedia.org/wiki/Pandemia_de_COVID-19_en_la_Ciudad_Aut%C3%B3noma_de_Buenos_Aires"

covidcaba <- read_html(url)

covidcaba

#Ahora comenzaremos a traes datos de la pagina web

# Primero vamos a traer mediante html_elements() el total de casos registrados para cada uno de los barrios de CABA

casosXbarrio <- covidcaba %>% 
  html_elements(".mw-parser-output div td:nth-child(1) div") %>% 
  html_table()

casosXbarrio

# Ahora con Xpath vamos a traer el total de casos registrados para cada villa de la ciudad

casosXvilla <- covidcaba %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[2]/table/tbody/tr/td[2]') %>% 
  html_table()

casosXvilla

#Convertimos los datos obtenidos en dos tibble

tabla_casosxbarrio <- casosXbarrio[[1]]

tabla_casosxbarrio

tabla_casosXvilla <- casosXvilla[[1]]

tabla_casosXvilla

# Como vemos en este ultimo caso hay dos columnas vacias. Las eliminamos

tabla_casosXvilla <- tabla_casosXvilla[1:3]

tabla_casosXvilla

# La ultima fila de cada columna refiere que el Ministerio de Salud actualiza los datos una vez por semana, que si bien es una aclaracion valiosa, en este caso la eliminaremos para centrarnos en los datos obtenidos hasta el momento y seguir trabajando con ellos

tabla_casosXvilla <-tabla_casosXvilla %>% 
  slice(1:18)

tabla_casosXvilla

# Ahora queremos estudiar cual es la relacion entre el numero de habitatnes que contrajo covid que vive en la ciudad formal y los que no.

# Primero trabajaremos con los datos de casos por villas. Vamos a transformar a factor los datos de la Zona y el Barrio y a numero el de Casos confirmados


tabla_casosXvilla <-tabla_casosXvilla %>% 
  mutate(Zona=as.factor(tabla_casosXvilla$Zona)) %>% 
  mutate(Barrio=as.factor(tabla_casosXvilla$Barrio)) %>% 
  mutate(Casosconfirmados=as.numeric(tabla_casosXvilla$Casosconfirmados))

tabla_casosXvilla

#Lo que nos interesa es agrupar las villas o asentamientos por barrio para saber cual es el total de poblacion por barrio que vive en villas o asentamiento y tuvo Covid

Casos_villasXbarrio <- tabla_casosXvilla %>% 
  group_by(Barrio) %>% 
  summarise(CasosAsentamientos=sum(Casosconfirmados))

Casos_villasXbarrio

#Ahora nos interesa saber como es esta relacion con el total de la poblacion del barrio. Para esto vamos a utilizar la otra tabla que obtuvimos de wikipedia

tabla_casosxbarrio

# Acomodamos los datos, primero vamos a transformar a factor los datos de Barrio y a numero el de Poblacion y el de Casos confirmados

tabla_casosxbarrio <-tabla_casosxbarrio %>% 
  mutate(Barrio=as.factor(tabla_casosxbarrio$Barrio)) %>% 
  mutate(Población=as.factor(tabla_casosxbarrio$Población)) %>% 
  mutate(Casosconfirmados=as.numeric(tabla_casosxbarrio$Casosconfirmados))

tabla_casosxbarrio


# Ahora para poder analizar los datos en conjunto, vamos a unir las dos tablas y veremos cual es la relacion entre numeros de contagiados en la ciudad formal y los habitantes de las villas y asentamientos


CiudadformalVsCiudadinformal <- left_join(tabla_casosxbarrio, Casos_villasXbarrio, by="Barrio")

# Como los barrios que no tiene villas tienen la variable "CasosAsentamientos" vacia y para este estudio puntual no nos interesan, los filtramos

CiudadformalVsCiudadinformal <- CiudadformalVsCiudadinformal %>% 
  filter(!is.na(CasosAsentamientos))

# Para finalizar este analisis queremos comparar los porcentajes de la poblacion que tuvo covid.

CiudadformalVsCiudadinformal <-CiudadformalVsCiudadinformal %>% 
  mutate(Casosconfirmados=as.numeric(CiudadformalVsCiudadinformal$Casosconfirmados)) %>% 
  mutate(CasosAsentamientos=as.numeric(CiudadformalVsCiudadinformal$CasosAsentamientos))

str(CiudadformalVsCiudadinformal)

CiudadformalVsCiudadinformal



PRUEBA <-CiudadformalVsCiudadinformal %>% 
  as.numeric(CiudadformalVsCiudadinformal$Población)


PRUEBA

PRUEBA <- CiudadformalVsCiudadinformal %>% 
  mutate(PCT_CiudadFormal= Casosconfirmados * 100 / Población) %>% 
  mutate(PCT_CiudadInformal = CasosAsentamientos * 100 / Población)


rm(PRUEBA)


CiudadformalVsCiudadinformal <- CiudadformalVsCiudadinformal %>% 
  mutate(PCT_CiudadFormal= Casosconfirmados * 100) %>% 
  mutate(PCT_CiudadInformal = CasosAsentamientos * 100)




rm(CiudadformalVsCiudadinformal)

