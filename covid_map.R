library(fst)
library(data.table)
library(leaflet)
library(stringr)
library(scales)
mexico <- read_sf("geo_data/00mun.shp") %>%
  st_transform(crs = 4326)
pal <- colorNumeric("RdYlBu", NULL)

states <- read_sf("geo_data/00ent.shp") %>%
  st_transform(crs = 4326)

covid_mex<-fread("Covid_data/201114COVID19MEXICO.csv")
covid_mex[,`:=`(MUNICIPIO_RES = str_pad(MUNICIPIO_RES,width = 3,
                                        side = "left",pad = '0'),
                ENTIDAD_RES = str_pad(ENTIDAD_RES,width = 2,
                                      side = "left",pad = '0'))]

covid_mex_group<-merge(mexico, covid_mex[RESULTADO_LAB==1,
                                          .N,by=.(MUNICIPIO_RES, ENTIDAD_RES)]
                       ,by.x=c("CVE_MUN","CVE_ENT"),
by.y=c("MUNICIPIO_RES", "ENTIDAD_RES" ), all.x=T)  

covid_mex_group$NOMGEO<-iconv(covid_mex_group$NOMGEO,from="latin1"
                       ,to="ASCII//TRANSLIT")
mun_pop <- paste0("<strong>Municipio: </strong>", 
                  covid_mex_group$NOMGEO, 
                  "<br><strong>Pacientes Totales: </strong>", 
                  comma(covid_mex_group$N))  
covid_mex_group %>% 
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data = states, color = "black", opacity = 1, weight = 1) %>% 
  addPolygons(fillColor = ~pal(log10(N)),
              fillOpacity = 0.5, 
              color = "#BDBDC3", 
              weight = .6,
              popup = mun_pop ) %>% 
  addLegend("bottomright", pal = pal, values = ~log10(N),
            title = "Number of positive cases (log10)"
            ) %>% 
  setView(-19.4326, 99.1332, zoom = 10)
  
