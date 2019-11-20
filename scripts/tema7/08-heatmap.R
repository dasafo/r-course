#Heat Map
install.packages(c("ggmap", "maps"))
library(ggmap)
tartu.data <- read.csv("../data/tema7/tartu_housing.csv",
                       sep = ";") 
head(tartu.data)

if(!requireNamespace("devtools")) install.packages("devtools")

devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library(ggmap)

register_google(key = "AIzaSyC8g6rMYjL7MV0niMjT-QSy84gqoce13_g")
has_google_key()

tartu_map <- get_map(location = "tartu", maptype = "satellite", zoom = 12,
                     
                     api_key = "AIzaSyC8g6rMYjL7MV0niMjT-QSy84gqoce13_g")

ggmap(tartu.map, extent = "device")+
  geom_point(data = tartu.data, aes(x=lon, y = lat), 
             colour = "red", alpha = 0.12, size = 2)

tartu.map2 <- get_map(location="tartu", zoom = 13)
ggmap(tartu.map2, extent = "device")+
  geom_density2d(data = tartu.data, aes(x=lon, y = lat),
                 size = .3)+
  stat_density2d(data = tartu.data, aes(x=lon, y = lat,
                                        fill = ..level..,
                                        alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon")+
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range=c(0,0.25), guide = F)
