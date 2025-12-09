########################################################
# Script práctica clase 19/11                          #
# Análisis de datos del Censo argentino                #
# Alejandra Rojas.                                     #
########################################################

# Limpio el entorno de trabajo
rm(list = ls())

# Obtengo la ruta de trabajo
getwd()

# =====================================================================
# INSTALACIÓN Y CARGA DE PAQUETES
# =====================================================================

# Lista de paquetes realmente necesarios
required_packages <- c(
  "dplyr",
  "ggplot2",
  "ggiraph",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "maps"
)

# Verifico qué paquetes no están instalados
packages_to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Instalo sólo los paquetes que faltan (desde fuente, compatible con MacPorts)
if(length(packages_to_install) > 0) {
  cat("Instalando paquetes faltantes:", paste(packages_to_install, collapse = ", "), "\n")
  install.packages(packages_to_install, dependencies = TRUE)
} else {
  cat("Todos los paquetes necesarios ya están instalados.\n")
}

# Cargo las librerías necesarias
library(dplyr)
library(ggplot2)
library(ggiraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

cat("Todas las librerías cargadas correctamente.\n")

# =====================================================================
# CARGA Y PREPARACIÓN DE DATOS
# =====================================================================

# Listo archivos disponibles
list.files()

# Cargo el archivo de trabajo
# destino <- read.csv("/Users/alejandra/Downloads/Humanidades Digitales/Herramientas-Maestria/Programacion_R/censo.csv")

# Cargo el archivo de trabajo desde la carpeta del script
destino <- read.csv("censo.csv")

# Visualizo la estructura
View(destino)
colnames(destino)

# Renombro el dataframe y sus columnas para mejor legibilidad
df_provincias <- destino %>%
  rename(
    viviendas_2022_colectivas = 'Viviendas.colectivas..2022.',
    poblacion_2022 = `Población..2022.`,
    personas_2022_viv_cole = 'Personas.en.viviendas.colectivas..2022.',
    personas_calle = `Personas.en.situación.de.calle..vía.pública.`,
    Viviendas_2010_Colectivas = `Viviendas.colectivas..2010.`,
    Poblacion_2010 = `Población..2010.`,
    Personas_2010_Colectivas = `Personas.en.viviendas.colectivas..2010.`,
    Nombre_Provincia = `Nombre.de.provincia`,
    Poblacion_Total = `Población.total`,
    Total_Hogares = `Total.de.hogares`,
    Latitud_Centroide = `Latitud.del.centroide`,
    Longitud_Centroide = `Longitud.del.centroide`
  )

# Selecciono las columnas relevantes para el análisis
df_provincias <- df_provincias %>%
  select(
    'viviendas_2022_colectivas',
    'poblacion_2022',
    'personas_2022_viv_cole',
    'personas_calle',
    'Viviendas_2010_Colectivas',
    'Poblacion_2010',
    'Personas_2010_Colectivas',
    'Nombre_Provincia',
    'Poblacion_Total',
    'Total_Hogares'
  )

# Calculo las diferencias entre períodos censales
df_provincias <- df_provincias %>% 
  mutate(
    dif_Pers_viviendas_colectivas = personas_2022_viv_cole - Personas_2010_Colectivas,
    dif_poblacion = poblacion_2022 - Poblacion_2010
  )

# =====================================================================
# VISUALIZACIONES
# =====================================================================

## Gráfico 1: Personas en situación de calle por provincia
a <- ggplot(df_provincias, 
            aes(x = reorder(Nombre_Provincia, personas_calle), 
                y = personas_calle,
                tooltip = paste0(
                  Nombre_Provincia, "<br>",
                  "Personas en calle: ", personas_calle
                ),
                data_id = Nombre_Provincia)) +
  geom_bar_interactive(stat = "identity", na.rm = TRUE) +
  coord_flip() +
  labs(
    title = "Personas en situación de calle por provincias argentinas",
    x = "Provincias",
    y = "Cantidad de personas en situación de calle"
  ) +
  theme_light()

girafe(
  ggobj = a,
  width_svg = 12,
  height_svg = 6,
  options = list(
    opts_sizing(rescale = TRUE)
  )
)

## Gráfico 2: Diferencia de población entre censos
b <- ggplot(df_provincias, 
            aes(x = Nombre_Provincia, 
                y = dif_poblacion,
                tooltip = paste0(
                  Nombre_Provincia, "<br>",
                  "Diferencia población: ", format(dif_poblacion, big.mark = " ")
                ),
                data_id = Nombre_Provincia
            )) +
  geom_bar_interactive(stat = "identity", na.rm = TRUE, fill = "violet") +
  labs(
    title = "Diferencia de población por provincias argentinas (2022 vs 2010)",
    x = "Provincias",
    y = "Diferencia de población"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

girafe(
  ggobj = b,
  width_svg = 12,
  height_svg = 6,
  options = list(
    opts_sizing(rescale = TRUE)
  )
)

## Gráfico 3: Comparación población 2022 y personas en situación de calle
c <- ggplot(destino, 
            aes(x = Nombre.de.provincia, 
                y = Población..2022., 
                fill = Personas.en.situación.de.calle..vía.pública.,
                tooltip = paste0(
                  Nombre.de.provincia, "<br>",
                  "Población 2022: ", format(Población..2022., big.mark = " "), "<br>",
                  "Personas en calle: ", Personas.en.situación.de.calle..vía.pública.
                ),
                data_id = Nombre.de.provincia)) +
  geom_bar_interactive(stat = "identity", na.rm = TRUE, position = "dodge", width = 0.7) +
  scale_fill_viridis_c() +
  labs(
    title = "Comparación de población 2022 y personas en situación de calle por provincia",
    x = "Provincia",
    y = "Población 2022",
    fill = "Personas en situación de calle"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

girafe(
  ggobj = c,
  width_svg = 12,
  height_svg = 6,
  options = list(
    opts_sizing(rescale = TRUE)
  )
)

# =====================================================================
# VISUALIZACIONES GEOESPACIALES
# =====================================================================

## Convierto el dataframe a formato espacial (sf)
df_sf <- st_as_sf(
  destino, 
  coords = c("Longitud.del.centroide", "Latitud.del.centroide"), 
  crs = 4326
)

## Obtengo el mapa base de Argentina
mapa_argentina <- ne_countries(
  scale = "medium", 
  country = "Argentina",
  returnclass = "sf"
)

## Gráfico 4: Mapa estático con personas en situación de calle
ggplot(data = mapa_argentina) +
  geom_sf(fill = "lightgrey") +
  geom_sf(
    data = df_sf, 
    aes(
      size = `Personas.en.situación.de.calle..vía.pública.`,
      color = `Personas.en.situación.de.calle..vía.pública.`
    ), 
    alpha = 0.7
  ) +
  scale_size_continuous(
    range = c(1, 10), 
    name = "Personas en Situación de Calle"
  ) +
  scale_color_gradient(
    low = "violet", 
    high = "lightgreen", 
    name = "Personas en Situación de Calle"
  ) +
  guides(
    size = guide_legend(),
    color = guide_legend()
  ) +
  labs(
    title = "Provincias de Argentina con personas en situación de calle",
    x = "Longitud", 
    y = "Latitud"
  ) +
  theme_minimal()

## Gráfico 5: Mapa interactivo con personas en situación de calle
f <- ggplot() +
  geom_sf(
    data = mapa_argentina,
    fill = "lightgrey",
    color = "white"
  ) +
  geom_sf_interactive(
    data = df_sf,
    aes(
      geometry = geometry,
      color = Personas.en.situación.de.calle..vía.pública.,
      tooltip = paste0(
        "<b>", Nombre.de.provincia, "</b><br>",
        "Población 2022: ", format(Población..2022., big.mark = " "), "<br>",
        "Personas en calle: ", Personas.en.situación.de.calle..vía.pública.
      ),
      data_id = Nombre.de.provincia
    ),
    size = 3,
    alpha = 0.9
  ) +
  scale_color_gradient(
    low = "violet",
    high = "lightgreen",
    name = "Personas en Situación de Calle"
  ) +
  labs(
    title = "Mapa interactivo: Personas en situación de calle por provincia",
    x = "Longitud", 
    y = "Latitud"
  ) +
  theme_minimal()

girafe(
  ggobj = f,
  width_svg = 12,
  height_svg = 6,
  options = list(
    opts_sizing(rescale = TRUE),
    opts_hover(css = "stroke:black;stroke-width:2;")
  )
)

cat("\n¡Análisis completado exitosamente!\n")

