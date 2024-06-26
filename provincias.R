# Instalar y cargar los paquetes necesarios
install.packages("sf")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("dplyr")

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Obtener el shapefile de Argentina
argentina <- ne_states(country = "argentina", returnclass = "sf")

# Datos de ejemplo (reemplaza esto con tus datos)
provincia_data <- data %>%
  count(provincia, sort = TRUE)

# Convertir nombres de provincias a minúsculas para emparejamiento
provincia_data <- provincia_data %>%
  mutate(provincia = tolower(provincia))

argentina <- argentina %>%
  mutate(name = tolower(name))

# Unir los datos de provincias con el shapefile de Argentina
argentina_data <- argentina %>%
  left_join(provincia_data, by = c("name" = "provincia"))

# Crear el mapa de Argentina con los datos de provincias
# Crear el mapa de Argentina con los datos de provincias, incluyendo los números de respuestas dentro de cada provincia
ggplot(data = argentina_data) +
  geom_sf(aes(fill = n)) +
  geom_text(data = argentina_data, aes(label = n, geometry = geometry), size = 4, family = "amatic", face = "bold", stat = "sf_coordinates", color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 20, family = "amatic", face = "bold"),
        legend.title = element_text(size = 22, family = "amatic", face = "bold")) 




argentina_data$name
argentina_data$n

sum(argentina_data$n, na.rm = TRUE)

# Alternativa 1: Viridis
ggplot(data = argentina_data) +
  geom_sf(aes(fill = n)) +
  geom_text(data = argentina_data, aes(label = n, geometry = geometry), size = 4, family = "amatic", face = "bold", stat = "sf_coordinates", color = "black") +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 20, family = "amatic", face = "bold"),
        legend.title = element_text(size = 22, family = "amatic", face = "bold"))

# Alternativa 2: Plasma
ggplot(data = argentina_data) +
  geom_sf(aes(fill = n)) +
  geom_text(data = argentina_data, aes(label = n, geometry = geometry), size = 4, family = "amatic", face = "bold", stat = "sf_coordinates", color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 20, family = "amatic", face = "bold"),
        legend.title = element_text(size = 22, family = "amatic", face = "bold"))

# Alternativa 3: Inferno
ggplot(data = argentina_data) +
  geom_sf(aes(fill = n)) +
  geom_text(data = argentina_data, aes(label = n, geometry = geometry), size = 4, family = "amatic", face = "bold", stat = "sf_coordinates", color = "black") +
  scale_fill_viridis_c(option = "inferno", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 20, family = "amatic", face = "bold"),
        legend.title = element_text(size = 22, family = "amatic", face = "bold"))

# Alternativa 4: Cividis
ggplot(data = argentina_data) +
  geom_sf(aes(fill = n)) +
  geom_text(data = argentina_data, aes(label = n, geometry = geometry), size = 4, family = "amatic", face = "bold", stat = "sf_coordinates", color = "black") +
  scale_fill_viridis_c(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 20, family = "amatic", face = "bold"),
        legend.title = element_text(size = 22, family = "amatic", face = "bold"))
