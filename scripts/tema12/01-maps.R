#R Google Maps
library(RgoogleMaps)

lat <- 41.652099 
lon <- -0.910399

uib.map <- GetMap(center = c(lat, lon), zoom = 17,
                  destfile = "tema12/uib.pdf",
                  format = "pdf",
                  maptype = "terrain")
PlotOnStaticMap(uib.map)

#satellite, roadmap, terrain