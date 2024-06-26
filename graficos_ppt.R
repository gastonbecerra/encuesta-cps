library(tidyverse)
library(googlesheets4)
library(skimr)
library(DataExplorer)
library("ggalluvial")
library(showtext)
# font_add_google("Amatic SC", "amatic")

showtext_auto()
theme_set(theme_minimal())

googlesheets4::gs4_deauth()

data <- googlesheets4::read_sheet('https://docs.google.comspreadsheets/d/19xjd3petoUF-xbrF-d_0sTyGzuhSCreImtxEsh7dQRc/edit?usp=sharing')

# 2 universidad y residencia -------------------- 

data %>%
  count(universidad) %>%
  filter(!is.na(universidad)) %>%
  ggplot(aes(x = reorder(universidad, desc(n)), y = n, fill = universidad)) +
  geom_col() +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  # coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")

argentina <- ne_states(country = "argentina", returnclass = "sf")
provincia_data <- data %>%
  count(provincia, sort = TRUE)
provincia_data <- provincia_data %>%
  mutate(provincia = tolower(provincia))
argentina <- argentina %>%
  mutate(name = tolower(name))
argentina_data <- argentina %>%
  left_join(provincia_data, by = c("name" = "provincia"))

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

# 3 edad e ingreso -------------------- 

data %>%
  select(marca, ingreso, egreso) %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), right = FALSE, labels = paste(seq(1965, 2020, by = 5), seq(1970, 2025, by = 5), sep = "-"))) %>%
  count(quinquenio, sort = TRUE) %>%
  filter(!is.na(quinquenio)) %>%
  ggplot(aes(x = quinquenio, y = n, fill = quinquenio)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")

data %>%
  filter(edad>19) %>%
  mutate(grupo_edad = cut(edad, 
                          breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90), 
                          right = FALSE, 
                          labels = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"))) %>%
  count(grupo_edad, sort = TRUE) %>%
  filter(!is.na(grupo_edad)) %>%
  ggplot(aes(x = grupo_edad, y = n, fill = grupo_edad)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")


# 4 areas de trabajo -------------------- 

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(n > 1, area_trabajo2 != "") %>%
  ggplot(aes(x = reorder(area_trabajo2, n), y = n, fill = area_trabajo2)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.position = "none")

# 5 motivos texto -------------------- 

# 6 motivos -------------------- 

data %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), 
                          right = FALSE, 
                          labels = paste(seq(1965, 2020, by = 5), 
                                         seq(1970, 2025, by = 5), sep = "-"))) %>%
  mutate(descripcion = case_when(
    motivos_CH == "BCDC" ~ "Búsqueda de Herramientas Analíticas",
    motivos_CH == "ICS" ~ "Interés, Gusto y Vocación",
    motivos_CH == "IPF" ~ "Influencias Personales y Familiares",
    motivos_CH == "MPH" ~ "Motivaciones Contextuales e Históricas",
    motivos_CH == "OAP" ~ "Oportunidades Académicas y Profesionales",
    motivos_CH == "CSS" ~ "Militancia y Transformación Social",
    motivos_CH == "RS" ~ "Responsabilidad Social",
    TRUE ~ "Otro" #vacio
  )) %>%
  count(quinquenio, descripcion) %>%
  filter(!is.na(descripcion), !is.na(quinquenio)) %>%
  group_by(descripcion) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(descripcion = fct_reorder(descripcion, total_n, .desc = TRUE)) %>%
  ggplot(aes(x = quinquenio, y = n, fill = descripcion)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.title = element_blank())

# 7 quienes no recomiendan? ------------

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, recomienda, sort = TRUE) %>% 
  filter(n > 1, area_trabajo2 != "") %>%
  ggplot(aes(x = reorder(area_trabajo2, n), y=n, fill=recomienda)) + 
  geom_col(position = "fill") + coord_flip() +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2) +
  theme_minimal() +
  scale_fill_viridis_d(option = "viridis", na.value = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.title = element_blank())

table(data$recomienda_CH)


# 8 razones para no recomiendan? ------------

data %>%
  select(marca,recomienda,recomienda_CH) %>%
  mutate(recomienda_CH = case_when(
    recomienda_CH == "BCDC" ~ "Búsqueda de Herramientas Analíticas",
    recomienda_CH == "ICS" ~ "Interés, Gusto y Vocación",
    recomienda_CH == "IPF" ~ "Influencias Personales y Familiares",
    recomienda_CH == "MPH" ~ "Motivaciones Contextuales e Históricas",
    recomienda_CH == "OAP" ~ "Oportunidades Académicas y Profesionales",
    recomienda_CH == "CSS" ~ "Militancia y Transformación Social",
    recomienda_CH == "RS" ~ "Responsabilidad Social",
    TRUE ~ "Otro" #vacio
  )) %>%
  filter(!is.na(recomienda_CH), recomienda_CH!="", recomienda_CH!=".", recomienda_CH!="Otro") %>%
  count(recomienda,recomienda_CH) %>% 
  ggplot(aes(x=reorder(recomienda_CH,n), y=n, fill=recomienda_CH)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~recomienda, scales = "fixed") +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 30, family = "amatic", face = "bold"))

# 9 cruce motivos y razones
data %>%
  select(marca, recomienda, recomienda_CH, motivos_CH) %>%
  filter(!is.na(recomienda_CH), !is.na(motivos_CH)) %>%
  mutate(recomienda2 = paste(recomienda, recomienda_CH, sep = " - ")) %>%
  count(recomienda, motivos_CH, recomienda2) %>%
  ggplot(aes(axis1 = motivos_CH, axis2 = recomienda2, y = n)) +
  facet_wrap(~recomienda, scales = "free_y") +
  geom_alluvium(aes(fill = motivos_CH)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 10, family = "amatic", face = "bold") +
  scale_x_discrete(limits = c("Motivos", "Recomendación"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  scale_fill_viridis_d(option = "turbo", na.value = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 30, family = "amatic", face = "bold"))


# 10 conocen asociaciones? ------------

table(data$asra_conoce)
corte_conoce = 284/nrow(data)

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  group_by(area_trabajo2) %>% 
  mutate(n=n()) %>% filter(n>2, !is.na(area_trabajo2), area_trabajo2!="") %>% ungroup() %>%
  ggplot(aes(area_trabajo2, fill=asra_conoce)) + 
  geom_bar(position = "fill") + coord_flip() +
  scale_fill_viridis_d(option = "viridis", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.title = element_blank(),
        strip.text = element_text(size = 30, family = "amatic", face = "bold")) + 
  geom_hline(yintercept = corte_conoce, color = "black", linetype = "dotted", size = 2)


data %>%
  filter(edad > 19) %>%
  ggplot(aes(edad, fill = asra_conoce)) +
  geom_histogram(binwidth = 5, position = "fill") +
  scale_fill_viridis_d(option = "viridis", na.value = "white") +
  scale_x_continuous(breaks = seq(20, 80, by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.title = element_blank(),
        strip.text = element_text(size = 30, family = "amatic", face = "bold")) + 
  geom_hline(yintercept = corte_conoce, color = "black", linetype = "dotted", size = 2)
