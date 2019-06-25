
x <- c("ggmap", "rgdal", "rgeos", "maptools", "tidyverse", "tmap")
lapply(x, library, character.only = T)

beat <- readOGR(dsn = "source_data/lmpd_beat", layer = "lmpd_beat")

str(beat@data)
head(beat@data)
unique(beat$LMPD_WEB)

qtm(shp = beat, fill = )