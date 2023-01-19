require(sf)
require(dplyr)

fonds_communal <- st_read("commune_francemetro_2021.shp",
                          options="ENCODING=WINDOWS-1252")

# options:        ENCODING=WINDOWS-1252 
# Reading layer `commune_francemetro_2021' from data source 
  # `/home/onyxia/work/TP_stat_spatiale/fonds/commune_francemetro_2021.shp' 
# using driver `ESRI Shapefile'
# Simple feature collection with 34836 features and 16 fields
# Geometry type: MULTIPOLYGON
# # Dimension:     XY
# Bounding box:  xmin: 99225.97 ymin: 6049647 xmax: 1242375 ymax: 7110480
# Projected CRS: RGF93 / Lambert-93

summary(fonds_communal)
View(fonds_communal)
head(fonds_communal)

st_crs(fonds_communal)
str(fonds_communal)

commune_bretagne <- fonds_communal %>% 
                      filter(reg == 53) %>%
                      select(code, libelle, epc, dep, surf)

st_crs(commune_bretagne)

plot(commune_bretagne, lwd=0.1, )


plot(st_geometry(commune_bretagne), lwd=0.1)

commune_bretagne$surf2 <- st_area(commune_bretagne$geometry)
str(commune_bretagne)
commune_bretagne$surf2<-commune_bretagne$surf2 * 1000000

# require(units)

commune_bretagne$surf3 <- units::set_units(st_area(commune_bretagne), "km^2")

str(commune_bretagne)


dept_bretagne <- 
