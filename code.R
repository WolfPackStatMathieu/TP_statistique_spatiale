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

plot(commune_bretagne,
     lwd=0.1 )


plot(st_geometry(commune_bretagne),
     lwd=0.1)
require(ggplot2)
ggplot() + 
  geom_sf(data= commune_bretagne,
          fill = "steelblue", col = "grey45")+
  theme_void()

commune_bretagne <- commune_bretagne %>%
  mutate(surf2 = st_area(geometry))


str(commune_bretagne)

# 11
all.equal(
  target = commune_bretagne$surf,
  round(as.double(commune_bretagne$surf2), 2))


commune_bretagne$surf2 <- st_area(commune_bretagne$geometry)
str(commune_bretagne)
commune_bretagne$surf2<-commune_bretagne$surf2 * 1000000
#pour supprimer la géometry : 
# commune_bretagne %>% st_drop_geometry()
# require(units)




commune_bretagne$surf3 <- units::set_units(st_area(commune_bretagne), "km^2")

str(commune_bretagne)

# 12

depts_bretagne <- commune_bretagne %>%
  group_by(dep) %>%
  summarise(
    surf = sum(surf)
  )
str(depts_bretagne)# tjrs objet sf
plot(depts_bretagne)

# 13 par union des géométries

commune_bretagne %>% st_union() %>% st_geometry() %>% plot()

depts_bretagne_geo <- commune_bretagne %>%
  group_by(dep) %>%
  summarise(
    surf = sum(surf)
  )
plot(depts_bretagne_geo)
str(depts_bretagne_geo)


# 14 centroïdes
centr_depts_bretagne<- depts_bretagne_geo %>%
  st_centroid()  #warning mais c'est pas une erreur

str(centr_depts_bretagne)
st_crs(centr_depts_bretagne)


plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)


ggplot() +
  geom_sf(data = depts_bretagne_geo)+
  geom_sf(data= centr_depts_bretagne)+
  theme_void()



lib_depts <- data.frame(
  code = as.character(c(22,29,35,56)),
  lib=c("Cotes-d'Armor", "Finistère", "Ille-et-Vilaine", "Morbihan")
)
centr_depts_bretagne <- centr_depts_bretagne %>%
  left_join(
    lib_depts,
    by = c("dep" = "code")
  )

coords_centr <- st_coordinates(centr_depts_bretagne) %>%
  bind_cols(
    centr_depts_bretagne %>%
      select(dep, lib) %>%
      st_drop_geometry()
  )
str(coords_centr)


plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)
text(coords_centr, labels = coords_centr$lib, adj = 0)

ggplot() +
  geom_sf(data = depts_bretagne_geo) +
  geom_sf(data = centr_depts_bretagne)+
  geom_sf_label(
    data = centr_depts_bretagne,
    aes(label = lib), size = 4
  )  +
  theme_void()

# 15 - intersection


commun




