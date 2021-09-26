# Trabajo Práctico Nº2 - Scrapeo


# Primero vamos a cargar las librerias necesarias para realizar el trabajo. En primer lugar vamos a cargars tidyverse y luego para este caso puntual sera necesario que instalaemos y carguemos rvest.
# Rvest puntualmente nos va a permitir extraer datos de las paginas webs.

#install.packages("tidyverse")
library(tidyverse) # "No used functions found"
#install.packages(rvest)
library(rvest) # read_html
library(stringr)

# Para este trabajo vamos a buscar datos relacionados a los casos de Covid19 para la ciudad de Buenos Aires.
# Encontramos en link de wikipedia que tiene infromacion detallada de como fue la evolucion de la pandemia en la ciudad, y tiene un par de tablas con informacion acerca de los casos confirmados
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
# Para esto, primero trabajaremos con los datos de casos por villas. Vamos a transformar a factor los datos de la Zona y el Barrio y a numero el de Casos confirmados
tabla_casosXvilla <-tabla_casosXvilla %>% 
  mutate(Zona=as.factor(Zona)) %>% 
  mutate(Barrio=as.factor(Barrio)) %>% 
  mutate(Casosconfirmados=as.numeric(Casosconfirmados))
tabla_casosXvilla

#Nos interesa agrupar las villas y asentamientos por barrio para saber cual es el total de poblacion por barrio que vive en villas o asentamiento que tuvo Covid
Casos_villasXbarrio <- tabla_casosXvilla %>% 
  group_by(Barrio) %>% 
  summarise(CasosAsentamientos=sum(Casosconfirmados))
Casos_villasXbarrio

#Ahora nos interesa saber como es esta relacion con el total de la poblacion del barrio. Para esto vamos a utilizar la otra tabla que obtuvimos de wikipedia
tabla_casosxbarrio

#Acomodamos los datos. Primero vamos a transformar a factor los datos de Barrio y a numero el de Poblacion y el de Casos confirmados
tabla_casosxbarrio <- tabla_casosxbarrio %>% 
  mutate(Población = str_remove_all(string = Población, pattern = " ")) 
tabla_casosxbarrio <- tabla_casosxbarrio %>%  
  mutate(Barrio=as.factor(Barrio)) %>% 
  mutate(across(.cols = c(Población, Casosconfirmados), .fns = ~as.numeric(.)))
tabla_casosxbarrio


# Ahora para poder analizar los datos en conjunto, vamos a unir las dos tablas y veremos cual es la relacion entre numeros de contagiados en la ciudad formal y en la ciudad informal
CiudadformalVsCiudadinformal <- left_join(tabla_casosxbarrio, Casos_villasXbarrio, by="Barrio")

# Como en los barrios que no tiene villas,la variable "CasosAsentamientos" queda vacia y para este estudio puntual no son neceesarios, los filtramos
CiudadformalVsCiudadinformal <- CiudadformalVsCiudadinformal %>% 
  filter(!is.na(CasosAsentamientos))

# Para finalizar este analisis queremos comparar los porcentajes de la poblacion que tuvo covid.
# Para esto calcularemos los porcentajes para cada barrio de población con Covid, pero antes le restaremos al total de los casos confirmados el valor de los casos confirmados en Asentamientos para tener los datos separados, ya que entendemos que los casos confirmados por barrio hace referencia al total 
CiudadformalVsCiudadinformal <- CiudadformalVsCiudadinformal %>% 
  mutate(CasosCFormal= Casosconfirmados - CasosAsentamientos) %>% 
  rename(CasosCInformal = CasosAsentamientos)

#Vemos que en el Barrio de Pompeya hay un error, porque los casos de confirmados en villas y asentamientos es mayor al total, por ende los casos para la ciudad formal quedan en negativo. Para poder continuar con el analisis filtraremos al barrio, y trabajaremos con el resto. 
CiudadformalVsCiudadinformal <- CiudadformalVsCiudadinformal %>% 
  filter(Barrio != "Nueva Pompeya")
CiudadformalVsCiudadinformal

#Ahora si, una vez desagregado este dato, calculamos los porcentajes
PCT_CiudadFvsCInformal <- CiudadformalVsCiudadinformal %>% 
  mutate(PCT_CiudadFormal= CasosCFormal * 100 / Población) %>% 
  mutate(PCT_CiudadInformal = CasosCInformal * 100 / Población)
PCT_CiudadFvsCInformal

#De esta manera vemos el porcentaje de la poblacion que tuvo covid en la ciudad formal y en la informal, pero concideramos que para el analisis, lo mas rico es ver como se distribuye en porcentaje de contagiados
PCT2_CiudadFvsCInformal <- CiudadformalVsCiudadinformal %>% 
  mutate(PCT_CiudadFormal= CasosCFormal * 100 / Casosconfirmados) %>% 
  mutate(PCT_CiudadInformal = CasosCInformal * 100 / Casosconfirmados)
PCT2_CiudadFvsCInformal

# De esta manera podemos ver que el Barrio con mayor diferencia entre los contagiados que viven en los barrios informales y los que no, es el barrio de Retiro en donde casi el 80% de las personas que tuvieron covid viven en la Villa 31.
# Vamos a guardar la informacion obtenida para poder trabajar con la misma y visualizarla en Trabajo Practico Nº3
write.csv(PCT2_CiudadFvsCInformal, "PCT2_CiudadFvsCInformal")
