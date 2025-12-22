# En primer lugar, cargamos las librerías 
library("tidytext", "tidyverse")
library(dplyr)
library(ggplot2)

# Cargamos los datos
provincias <- read.csv("https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv")
View(provincias)

# Simplificamos el nombre de las columnas 
provincias <- provincias %>%
  rename(situacionCalle = Personas.en.situación.de.calle..vía.pública.)

# Comienzo del análisis
# La media de personas en situacion de calle agrupadas según provincia.
media_situacionCalle <- provincias %>%
  group_by(Nombre.de.provincia) %>%
  summarise(
    media_situacionCalle = mean(situacionCalle, na.rm = TRUE)
  )

media_situacionCalle

# Gráfico
ggplot(media_situacionCalle %>% 
       filter(!is.na(media_situacionCalle)), # Elimino los que tienen valor NaN
       aes(x = reorder(Nombre.de.provincia, media_situacionCalle),
           y = media_situacionCalle)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Media de situación en calle por provincia",
    x = "Provincia",
    y = "Media situación en calle"
  ) +
  theme_minimal()

# Acerca de la población total
porcentaje_poblacion <- provincias %>%
  select(Nombre.de.provincia, Población.total) %>%
  filter(!is.na(Población.total)) %>%
  mutate(
    porcentaje = Población.total / sum(Población.total) * 100
  ) %>%
  select(Nombre.de.provincia, porcentaje) # Solo veo las columnas que quiero analizar

print(porcentaje_poblacion)

# Realizo el gráfico
ggplot(porcentaje_poblacion,
       aes(x = reorder(Nombre.de.provincia, porcentaje),
           y = porcentaje)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Porcentaje de población por provincia",
    x = "Provincia",
    y = "Porcentaje (%)"
  ) +
  theme_minimal()



