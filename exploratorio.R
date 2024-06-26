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

# SOCIODEMO -------------------- 

glimpse(data)
skim(data)

plot_histogram(data)
plot_bar(data %>% select(-instituciones))

data %>% count(edad)

data %>%
  select(marca, ingreso, egreso) %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), right = FALSE, labels = paste(seq(1965, 2020, by = 5), seq(1970, 2025, by = 5), sep = "-"))) %>%
  count(quinquenio, sort = TRUE) %>%
  filter(!is.na(quinquenio)) %>%
  ggplot(aes(x = quinquenio, y = n, fill = quinquenio)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")

data %>%
  mutate(grupo_edad = cut(edad, 
                          breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90), 
                          right = FALSE, 
                          labels = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"))) %>%
  count(grupo_edad, sort = TRUE) %>%
  filter(!is.na(grupo_edad)) %>%
  ggplot(aes(x = grupo_edad, y = n, fill = grupo_edad)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")

summary(data$edad)

data %>%
  count(universidad) %>%
  filter(!is.na(universidad)) %>%
  ggplot(aes(x = reorder(universidad, desc(n)), y = n, fill = universidad)) +
  geom_col() +
  theme_minimal() +
  # coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")

data %>%
  select(marca, edad) %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), right = FALSE, labels = paste(seq(1965, 2020, by = 5), seq(1970, 2025, by = 5), sep = "-"))) %>%
  count(quinquenio, sort = TRUE) %>%
  filter(!is.na(quinquenio)) %>%
  ggplot(aes(x = quinquenio, y = n, fill = quinquenio)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "none")


data %>%
  count(universidad, provincia) %>%
  ggplot(aes(axis1 = provincia, axis2 = universidad, y = n)) +
  geom_alluvium(aes(fill = provincia)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Provincia","Universidad"), expand = c(0.15, 0.05)) +
  labs(title = "Universidad y Provincia", x = "Categorías", y = "Frecuencia") +
  theme_minimal()


# AREAS DE TRABAJO -------------------- 

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(n > 1, area_trabajo2 != "") %>%
  ggplot(aes(x = reorder(area_trabajo2, n), y = n, fill = area_trabajo2)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 30, family = "amatic", face = "bold"),
        legend.position = "none")


data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(n<2, area_trabajo2!="") %>%
  mutate(t=paste(area_trabajo2, n, collapse = " - ")) %>%
  pull(t) %>% head(1)

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(area_trabajo2!="") %>%

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, genero, sort = TRUE) %>%
  filter(n>10, area_trabajo2!="") %>%
  ggplot(aes(area_trabajo2,n, fill=genero)) + 
    geom_col(position = "fill") + coord_flip()

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(n<2, area_trabajo2!="")


# QUIENES (NO) RECOMIENDAN ESTUDIAR -------------------- 

table(data$recomienda)

corte_recomienda = 444/nrow(data)

data %>%
  filter(!is.na(provincia)) %>%
  ggplot(aes(provincia, fill=recomienda)) + 
  geom_bar() + coord_flip() 

data %>%
  filter(!is.na(provincia)) %>%
  ggplot(aes(provincia, fill=recomienda)) + 
  geom_bar(position = "fill") + coord_flip() +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>% 
  ggplot(aes(universidad, fill=recomienda)) +
  geom_bar(position = "fill") + coord_flip()

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  group_by(area_trabajo2) %>% 
  mutate(n=n()) %>% 
    mutate(area_trabajo3 = case_when(
      n > 10 ~ area_trabajo2,
      n <= 10 ~ "otros"
    )) %>%
    filter(!is.na(area_trabajo2), area_trabajo2!="") %>% ungroup() %>%
  ggplot(aes(area_trabajo3, fill=recomienda)) + 
  geom_bar(position = "fill") + coord_flip() +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>%
  filter(edad>19) %>%
  ggplot(aes(genero, fill=recomienda)) + 
  geom_bar(position = "fill") + coord_flip() +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>% 
  filter(edad>19) %>%
  ggplot(aes(edad, fill=recomienda)) +
  geom_histogram(binwidth = 5, position = "fill") +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>% 
  ggplot(aes(egreso, fill=recomienda)) +
  geom_histogram(binwidth = 5)

data %>% 
  ggplot(aes(ingreso, fill=recomienda)) +
  geom_histogram(binwidth = 5, position="fill") +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)






# QUIENES (NO) CONOCEN ASRA -------------------- 

table(data$asra_conoce)

corte_conoce = 284/nrow(data)

data %>%
  filter(!is.na(provincia)) %>%
  ggplot(aes(provincia, fill=asra_conoce)) + 
  geom_bar() + coord_flip() 

data %>%
  filter(!is.na(provincia)) %>%
  ggplot(aes(provincia, fill=asra_conoce)) + 
  geom_bar(position = "fill") + coord_flip() 

data %>% 
  ggplot(aes(universidad, fill=asra_conoce)) +
  geom_bar(position = "fill") + coord_flip()

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  group_by(area_trabajo2) %>% 
  mutate(n=n()) %>% filter(n>2, !is.na(area_trabajo2), area_trabajo2!="") %>% ungroup() %>%
  ggplot(aes(area_trabajo2, fill=asra_conoce)) + 
  geom_bar(position = "fill") + coord_flip()

data %>%
  ggplot(aes(genero, fill=asra_conoce)) + 
  geom_bar(position = "fill") + coord_flip() 

data %>% 
  filter(edad>19) %>%
  ggplot(aes(edad, fill=asra_conoce)) +
  geom_histogram(binwidth = 5)

data %>% 
  filter(edad>19) %>%
  ggplot(aes(edad, fill=asra_conoce)) +
  geom_histogram(binwidth = 5, position = "fill") +
  geom_hline(yintercept = corte_conoce, color = "black", linetype = "dotted", size = 2)


# RAZONES WORDCLOUD -------------------- 

data_si <- data %>% mutate(recomienda_razones = as.character(recomienda_razones) ) %>% filter(recomienda == "Si")
data_no <- data %>% mutate(recomienda_razones = as.character(recomienda_razones) ) %>% filter(recomienda == "No")

library(tidytext)
library(tm)
library(wordcloud)
library(SnowballC)
library(textstem)



procesar_texto <- function(data, columna_texto) {
  stopwords_es <- stopwords("es")
  
  lematizar <- function(word) {
    lemma <- textstem::lemmatize_words(word, language = "es")
    return(lemma)
  }
  
  data %>%
    select({{columna_texto}}) %>%
    filter(!is.na(!!sym(columna_texto))) %>%
    unnest_tokens(word, {{columna_texto}}) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!word %in% stopwords_es) %>%
    mutate(word = lematizar(word)) %>%
    # mutate(word = wordStem(word, language = "es")) %>%
    count(word, sort = TRUE)
}

text_tokens_si <- procesar_texto(data_si, "recomienda_razones")
text_tokens_no <- procesar_texto(data_no, "recomienda_razones")

wordcloud(words = text_tokens_si$word, freq = text_tokens_si$n, min.freq = 2,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

wordcloud(words = text_tokens_no$word, freq = text_tokens_no$n, min.freq = 2,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

text_tokens_si
text_tokens_no



# RECOMENDACION ------------


glimpse(data)

data %>%
  select(marca,recomienda,recomienda_razones,recomienda_etiquetas) %>%
  mutate(etiquetas = str_split(recomienda_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>% 
  slice_max(order_by = n, n = 15, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
  geom_col() +
  coord_flip() +
  #facet_wrap(~recomienda, scales = "free")
  facet_wrap(~recomienda, scales = "free_y")




# MOTIVOS -------------


data %>%
  select(marca,recomienda,estudio_razones,motivos_etiquetas) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>%
  # slice_max(order_by = n, n = 10, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
  geom_col() +
  coord_flip()

data %>%
  select(marca,recomienda,estudio_razones,motivos_etiquetas) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>%
  slice_max(order_by = n, n = 10, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~recomienda, scales = "free_y")
# facet_wrap(~recomienda, scales = "free")

data %>%
  select(marca,recomienda,estudio_razones,motivos_etiquetas) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(etiquetas, sort = TRUE) 

data %>%
  select(marca,ingreso,recomienda,estudio_razones,motivos_etiquetas) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), right = FALSE, labels = paste(seq(1965, 2020, by = 5), seq(1970, 2025, by = 5), sep = "-"))) %>%
  group_by(quinquenio, etiquetas) %>%
  summarise(conteo = n()) %>%
  filter(!is.na(etiquetas),!is.na(quinquenio)) %>%
  arrange(quinquenio, desc(conteo))  %>%
  ggplot(aes(x = quinquenio, y = conteo, fill = etiquetas)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribución de Etiquetas por Quinquenio de Ingreso",
       x = "Quinquenio de Ingreso",
       y = "Conteo de Etiquetas",
       fill = "Etiquetas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



data %>% count(motivos_CH)

data %>%
  mutate(quinquenio = cut(ingreso, breaks = seq(1965, 2025, by = 5), 
                          right = FALSE, 
                          labels = paste(seq(1965, 2020, by = 5), 
                                         seq(1970, 2025, by = 5), sep = "-"))) %>%
  mutate(descripcion = case_when(
    motivos_CH == "ICS" ~ "Interés por las Ciencias Sociales",
    motivos_CH == "BCDC" ~ "Búsqueda de Conocimiento y Desarrollo Crítico",
    motivos_CH == "IPF" ~ "Influencias Personales y Familiares",
    motivos_CH == "OAP" ~ "Oportunidades Académicas y Profesionales",
    motivos_CH == "CSS" ~ "Compromiso y Sensibilidad Social",
    motivos_CH == "MPH" ~ "Motivaciones Políticas e Históricas",
    TRUE ~ "Otro"
  )) %>%
  count(quinquenio, descripcion) %>%
  filter(!is.na(descripcion), !is.na(quinquenio)) %>%
  ggplot(aes(x = quinquenio, y = n, fill = descripcion)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "magma", na.value = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 30, family = "amatic", face = "bold"),
        axis.text.y = element_text(size = 30, family = "amatic", face = "bold"),
        axis.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 25, family = "amatic", face = "bold"),
        legend.title = element_blank())



data %>%
  select(marca,recomienda,estudio_razones,motivos_etiquetas) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>%
  # slice_max(order_by = n, n = 10, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
  geom_col() +
  coord_flip()



data %>%
  select(marca,recomienda,estudio_razones,motivos_etiquetas, recomienda_CH, motivos_CH) %>%
  mutate(etiquetas = str_split(motivos_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas,  motivos_CH) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  count(etiquetas, motivos_CH) %>%
  ggplot(aes(axis1 = etiquetas, axis2 = motivos_CH, y = n)) +
  geom_alluvium(aes(fill = motivos_CH)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("etiquetas","motivos_CH"), expand = c(0.15, 0.05)) +
  #labs(title = "Universidad y Provincia", x = "Categorías", y = "Frecuencia") +
  theme_minimal()



data %>%
  count(recomienda, recomienda_CH, motivos_CH) %>%
  ggplot(aes(axis1 = recomienda_CH, axis2 = motivos_CH, y = n)) +
  geom_alluvium(aes(fill = motivos_CH)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("recomienda_CH","motivos_CH"), expand = c(0.15, 0.05)) + ~
  #labs(title = "Universidad y Provincia", x = "Categorías", y = "Frecuencia") +
  theme_minimal()
