library(tmap)
tmap_mode("view")

city_names = c("London","Birmingham","Manchester",
               "Liverpool","Leeds","Sheffield","Middlesbrough",
               "Bristol","Bournemouth","Stoke-on-Trent",
               "Leicester","Wirral","Coventry",
               "Nottingham","Bradford","Newcastle",
               "Brighton","Plymouth","Hull",
               "Glasgow","Southampton","Edinburgh",
               "Cardiff","Aberdeen","Inverness",
               "Perth","York","Peterborough","Cambridge",
               "Norwich","Milton Keynes","Oxford","Gloucester",
               "Canterbury","Gloucester","Exeter",
               "Swansea","Carlisle","Lancaster",
               "Bangor","Colchester","Portsmouth","Lincoln",
               "Southend-on-Sea",
               "Hereford")


x = tmaptools::geocode_OSM(paste0(city_names,", United Kingdom"), as.sf = TRUE)
qtm(x)

x = x[,c("query")]

st_write(x,"data/example_cities.geojson", delete_dsn = TRUE)
