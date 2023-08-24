provincias <- "data/provincia.json"

caba <- st_read(provincias) %>% 
  filter(fna == "Ciudad Autónoma de Buenos Aires") %>% 
  select(geometry) %>% 
  mutate(provincia = "CABA")

caba <- st_transform(caba, 3857)

caba <- caba %>% 
  mutate(link = NA,
         codpcia = "02", #Le asigno un valor para futuros procesamientos
         departamen = NA,
         mujeres = NA,
         varones = NA,
         personas = NA,
         hogares = NA, 
         viv_part_h = NA,
         viv_part = NA, 
         codigo_departamento_indec = "2000") #Le asigno un valor para futuros procesamientos

mapa_dptos <- read_sf ("data/departamentos_arg.geojson")
st_crs(mapa_dptos)
mapa_dptos_mercator <- st_transform(mapa_dptos, 3857)

mapa_dptos_mercator <- mapa_dptos_mercator %>% 
  filter(provincia != "Ciudad Autónoma de Buenos Aires")

mapa_con_caba <- rbind(mapa_dptos_mercator, caba)

mapa_con_caba <- mapa_con_caba %>% 
  rename(codprov_censo = codpcia)

rm(caba, mapa_dptos, mapa_dptos_mercator)

ggplot (mapa_con_caba)+
  geom_sf ()

####Segunda etapa:
mapa_dptos_mercator <- mapa_con_caba %>%
  mutate(codigo_departamento_indec = as.numeric(codigo_departamento_indec))

#Achicamos la base para tener menos columnas, decidimos trabajar sólo con los datos de marzo 2023
base_dptos_3_23<- base_dptos_total %>%
  select(codigo_departamento_indec, m202303, provincia, departamento)

#Unificamos ambos archivos.
base_unificada <- mapa_dptos_mercator %>%
  left_join(base_dptos_3_23, by = "codigo_departamento_indec")

base_unificada <- base_unificada %>%
  mutate(cuartil4 = case_when(is.na(m202303) ~ "Sin datos",
                              m202303 >= quantile(m202303, probs = 0.75, na.rm = TRUE) ~ "Si",
                              TRUE ~ "No")) %>%
  select(m202303, cuartil4, codprov_censo)

mapa <- ggplot(base_unificada)+
  geom_sf(aes(fill= cuartil4))+
  scale_fill_viridis_d()+
  labs (title = "Departamentos del país con salarios promedios más elevados** (cuarto cuartil)",
        subtitle = "Argentina, marzo 2023.",
        fill= "Departamentos del cuarto cuartil de salarios",
        caption= "Elaboración propia en base a datos del Ministerio de desarrollo productivo")+
  theme_void()

polArViz::inset_caba(mapa)





