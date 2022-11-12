##### Instrumentos de Análisis Urbano II, UTDT #####
##### Trabajo Final #####
### Alumna: Lara Forlino

#Librerías
library(tidyverse)
library(janitor)
library (geoAr)
library(sf)
library(geoAr)
library(eph)
library(skimr)
library(stringi)
library(mapview)
library(kableExtra)
library(leafpop)
library(viridis)

#¿Dónde están y qué características reúnen los barrios populares de Argentina?
#Este trabajo explora la distribución geográfica y algunas características de los barrios populares del país
#Para ello se utiliza el Registro Nacional de Barrios Populares (RENABAP)

#Descargo las bases de datos con información relevante del RENABAP
#Si bien alguna información se repite entre bases, algunas variables solo aparecen en algunas de ellas

base <- read.csv("https://datosabiertos.desarrollosocial.gob.ar/dataset/0d022767-9390-486a-bff4-ba53b85d730e/resource/9a951270-60dd-4f21-aa19-4ef1205620bd/download/2022-07-13_info_publica.csv")

base_geo <- st_read("https://datosabiertos.desarrollosocial.gob.ar/dataset/0d022767-9390-486a-bff4-ba53b85d730e/resource/97cc7d10-ad4c-46cb-9ee4-becb402adf9f/download/2022-07-13_info_publica.geojson")


#Analizo la información de la base
skim(base) %>%
  kbl(caption = "Varibales RENABAP") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Analizo la distribución porcentual de los barrios populares por provincia
base %>% 
  calculate_tabulates("provincia", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(provincia, Freq), y = Freq)) +
  geom_col(fill = "blue", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "Distribución porcentual de los barrios populares de Argentina por provincia",
       caption= "Fuente: https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares",
       fill = "", x = "Provincia", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold")) 

#Como es de esperar, Buenos Aires concentra la mayor parte de los barrios populares del país

#Descargo mapas de Argentina y la provincia de Buenos Aires (PBA) para localizar geográficamente los datos
argentina_mapa <- get_geo(geo = "ARGENTINA",level = "provincia")

buenos_aires_mapa<- get_geo(geo = "BUENOS AIRES",level = "departamento") %>% 
  add_geo_codes() %>% 
  rename(departamento=nomdepto_censo) 

#Procedo a localizar geograficamente los barrios populares en el mapa de Argentina
ggplot() +  
  geom_sf(data=argentina_mapa,color="black")+
  geom_sf(data=base_geo,color="blue")+
  labs(title = "Distribución geográfica de los barrios populares",
       caption= "Fuente: https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares",
       fill = "", x = "Provincia", y = "Cantidad") +
  theme_void() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=8, face = "bold"))


#Dado que la mayoría de los barrios populares se encuentran en Buenos Aires, me quedo con los datos correspondientes a esa provincia
base_ba <- base %>% 
  filter(provincia == "Buenos Aires") 

base_geo_ba <- base_geo %>% 
  filter(provincia == "Buenos Aires") 

#Ahora localizo los barrios populares de la provincia en el mapa
ggplot() +  
  geom_sf(data=buenos_aires_mapa,color="black")+
  geom_sf(data=base_geo_ba,color="red")+
  labs(title = "Distribución geográfica de los barrios populares",
       caption= "Fuente: https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares",
       fill = "", x = "Provincia", y = "Cantidad") +
  theme_void() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=8, face = "bold"))

#Analizo que departamentos de la provincia concentran más barrios populares
base_geo_ba %>%
  calculate_tabulates("departamento", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  filter(Freq>="1%") %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(departamento, Freq), y = Freq)) +
  geom_col(fill = "red", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "Distribución porcentual de los barrios populares por departamento de PBA",
       caption= "https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares",
       fill = "", x = "Provincia", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold")) 

#Dado que el grueso de los barrios populares de PBA se concentran en el AMBA, recortamos el mapa de la provincia de Buenos Aires para observar con más precisión esa zona
AMBA <-  st_crop(buenos_aires_mapa,xmin=-59,xmax=-56,
                 ymin=-35,ymax=-34.2)

AMBA_geo<- st_crop(base_geo_ba,xmin=-59,xmax=-56,
                   ymin=-35,ymax=-34.2)

#Localizo los barrios populares en el mapa del AMBA
ggplot()+
  geom_sf(data=AMBA,color="black")+
  geom_sf(data=AMBA_geo,color="red")+
  labs(title = "Distribución geográfica de los barrios populares en el AMBA",
       caption= "Fuente: https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares",
       fill = "", x = "Provincia", y = "Cantidad") +
  theme_void() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=9, face = "bold"))

#Procedo a armar un mapa de coropletas segun cantidad de barrios populares por departamento del AMBA

#Paso el nombre de los departamentos del mapa del AMBA a minúsculas
AMBA <- AMBA %>% mutate(departamento=tolower(departamento),
                        departamento = 
                          stri_trans_general(departamento, "Latin-ASCII"))

#Agrupo la cantidad de barrios populares por departamento del AMBA
departamentos <- base_geo_ba  %>% group_by(departamento) %>% 
  summarise(cantidad=n()) %>% 
  st_set_geometry(NULL)

#Unifico el formato de la nueva base a minúsculas y sin acentos para hacer un join
departamentos <- departamentos %>% 
  mutate(departamento=tolower(departamento)) %>% 
  mutate (departamento = stri_trans_general (departamento, "Latin-ASCII"))

AMBA <- AMBA %>%
  mutate(departamento = recode(departamento, ezeiza = 'jose m. ezeiza'))

#Junto la información del mapa del AMBA con la cantidad de barrios por departamento
AMBA <- AMBA %>%
  left_join(departamentos,by = "departamento")

#Mapa de coropletas
ggplot() +
  geom_sf(data = AMBA, aes(fill=cantidad), color = NA) + 
  geom_sf_text(data = AMBA, aes(label = departamento), size=2.5, colour = "black") +
  labs(title = "Barrios Populares del AMBA",
       subtitle = "Densidad de asentamientos",
       fill = "Cantidad",
       caption= "Fuente: https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares") +
  theme_void() +
  scale_fill_distiller(palette = "Spectral")


#Realizo un mapa interactivo que permite visualizar la cantidad de barrios populares por departamento
#Además, el mapa muestra los poligonos correspondientes a los barrios, sobre los cuales al hacer click se abre un popup con información relevante de cada uno
#Las variables informativas elegidas para cada barrio son su nombre, la decada de creacion, el tipo de barrio, la cantidad de familias y cantidad de viviendas aproximadas

pal <- cividis(n = 32, direction = -1)

mapview(AMBA,zcol="cantidad",col.regions = pal,
        popup=popupTable(
          AMBA,
          zcol=c("departamento")))+
  mapview(AMBA_geo,legend=FALSE,color="black",col.regions="black",
          popup=popupTable(
            AMBA_geo,
            zcol=c("nombre_barrio",
                   "clasificacion_barrio",
                   "decada_de_creacion",
                   "cantidad_familias_aproximada",
                   "cantidad_viviendas_aproximadas")))

#Ahora analizo el acceso a servicios básicos de los barrios populares del AMBA

#Electricidad: el 62% de las viviendas tiene una conexión irregular a la red
AMBA_geo %>%
  calculate_tabulates("energia_electrica", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>%  
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(energia_electrica, Freq), y = Freq)) +
  geom_col(fill = "orange", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "servicio de electricidad",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Acceso", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold")) 

#Cocina: solo el  4.3% de las viviendas tiene conexión formal a la red de gas
AMBA_geo %>%
  calculate_tabulates("cocina", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(cocina, Freq), y = Freq)) +
  geom_col(fill = "green", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "Cocina",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Acceso", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"))

#Efluentes Cloacales: más de la mitad de las viviendas cuentan con desagüe solo a pozo negro/ciego u hoyo
AMBA_geo %>%
calculate_tabulates("efluentes_cloacales", add.percentage = "col") %>% 
  mutate(Freq = as.numeric(Freq)) %>% 
  arrange(-Freq) %>% 
  ggplot(aes(x = reorder(efluentes_cloacales, Freq), y = Freq)) +
  geom_col(fill = "grey", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = 0) + 
  geom_text(aes(label = paste0(Freq, "%"), y = Freq + 0.5), hjust = -0.1, size = 3) +
  labs(title = "Efluentes Cloacales",
       caption= "Fuente: https://datos.gob.ar/",
       fill = "", x = "Tipo", y = "Porcentaje") +
  coord_flip() +
  theme_minimal() +
  theme(plot.caption=element_text(face = "italic", colour = "gray35",size=6),
        title=element_text(size=10, face = "bold"))

#Analizo que porcentaje de familias viviendo en estos barrios recibe AUH
base_ba %>%   
  summarise("Recibe AUH"=round(sum(recibe_asignacion_universal_por_hijo)/454448,digits = 4)*100,
            "NO recibe AUH"=round(sum(no_recibe_asignacion_universal_por_hijo)/454448,digits = 4)*100,
            "No sabe"=round(sum(no_sabe_si_recibe_asignacion_universal_por_hijo)/454448,digits = 4)*100,
            "Total de familias"=sum(recibe_asignacion_universal_por_hijo)+
              sum(no_recibe_asignacion_universal_por_hijo)+
              sum(no_sabe_si_recibe_asignacion_universal_por_hijo))%>% 
  kbl(caption = "AUH en barrios populares de PBA") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Conclusión: La mayor parte de los barrios populares de Argentina se concentran en la provincia de Buenos Aires (34%)
#Dentro de PBA, los alrededores de CABA, el AMBA, concentra el grueso de barrios populares.
#El departamento con más barrios populares de AMBA es La Plata, seguido de La Matanza y Moreno.

#Al analizar el acceso a servicios básicos tales como electricidad, gas y servicios de cloaca, podemos notar que una significativa mayoría no accede a servicios básicos de calidad
#La carencia de acceso a servicios básicos tiene consecuencias nocivas para la población, especialmente para los niños/as y su desarrollo

#Si observamos el porcentaje de familias que recibe la AUH en PBA, el principal programa social que combate la pobreza infantil de nuestro país, notamos que más de la mitad de los hogares son beneficiarios

#Explorar las características de los integrantes de barrios populares y su acceso a servicios básicos nos brinda información valiosa para el diseño de políticas públicas de calidad
#Una propuesta de política pública que he desarrollado en otros trabajos consiste en segmentar la AUH según las características de sus beneficiarios
#Al segmentar esta transferencia de ingresos según parametros preestablecidos  como el acceso a servicios básicos, el programa podría brindar más recursos a quienes más lo necesitan
#De esta manera, si bien la AUH continuaría siendo un programa universal, la distribución de recursos se ajusta al nivel de necesidad de cada familia, optimizando los esfuerzos estatales para combatir la pobreza
