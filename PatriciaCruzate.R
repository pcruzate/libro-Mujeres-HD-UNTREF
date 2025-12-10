# Script: PatriciaCruzate.R
# Ejercicio - Primeros análisis en RStudio

# Paso 1: cargar tidyverse
library(tidyverse)

# Paso 2: leer el CSV desde la web (el de la profe)
url <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"

provincias <- read_csv(url)

names(provincias)

# Paso 3: preparar los datos y calcular una media por provincia

provincias <- provincias |>
  rename(
    situacion_calle = `Personas en situación de calle (vía pública)`
  )

media_situacion_calle <- provincias |>
  group_by(`Nombre de provincia`) |>
  summarise(
    media_situacion_calle = mean(situacion_calle, na.rm = TRUE)
  )

media_situacion_calle

# Paso 4: gráfico con ggplot

grafico <- ggplot(
  subset(media_situacion_calle, media_situacion_calle > 0),
  aes(x = reorder(`Nombre de provincia`, media_situacion_calle),
      y = media_situacion_calle,
      fill = media_situacion_calle)
) +
  geom_col() +
  labs(
    title = "Valor medio de personas en situación de calle por provincia",
    x = "Provincia",
    y = "Valor medio de personas en situación de calle"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(grafico)

# Paso 5: gráfico comparativo 

grafico_comparativo <- ggplot(
  provincias,
  aes(
    x = `Nombre de provincia`,
    y = `Población (2022)`,
    fill = situacion_calle
  )
) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_c() +  # paleta de colores linda para la escala de fill
  labs(
    title = "Comparación de Población 2022 y personas en situación de calle por provincia",
    x = "Provincia",
    y = "Población 2022",
    fill = "Personas en situación de calle"
  ) +
  theme_minimal() +
 theme(
  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
)


print(grafico_comparativo)

# Paso 6: Calcular el porcentaje de personas en situación de calle
# sobre la población total por provincia

porcentaje_calle <- provincias |>
  mutate(
    porcentaje_calle = situacion_calle / `Población (2022)` * 100
  ) |>
  filter(!is.na(porcentaje_calle), porcentaje_calle > 0) |>
  select(
    `Nombre de provincia`,
    situacion_calle,
    `Población (2022)`,
    porcentaje_calle
  )

# Mostramos la tabla de porcentajes
porcentaje_calle

# Paso 7: Gráfico del porcentaje de personas en situación de calle (colores pastel)

grafico_porcentaje <- ggplot(
  porcentaje_calle,
  aes(
    x = reorder(`Nombre de provincia`, porcentaje_calle),
    y = porcentaje_calle,
    fill = porcentaje_calle
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "#F7A8C7",   # rosa pastel
    high = "#B39DDB"   # violeta pastel
  ) +
  labs(
    title = "Porcentaje de personas en situación de calle por provincia",
    x = "Provincia",
    y = "Porcentaje sobre población 2022 (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(grafico_porcentaje)
