library(tidyverse)
library(googlesheets4)
library(skimr)
library(DataExplorer)
theme_set(theme_minimal())

googlesheets4::gs4_deauth()

data <- googlesheets4::read_sheet('https://docs.google.comspreadsheets/d/19xjd3petoUF-xbrF-d_0sTyGzuhSCreImtxEsh7dQRc/edit?usp=sharing')


# SOCIODEMO -------------------- 

glimpse(data)
skim(data)

plot_histogram(data)
plot_bar(data %>% select(-instituciones))

# AREAS DE TRABAJO -------------------- 

data %>% 
  separate_rows(area_trabajo2, sep = ",") %>%
  mutate(area_trabajo2 = str_trim(area_trabajo2)) %>%
  count(area_trabajo2, sort = TRUE) %>%
  filter(n>10, area_trabajo2!="") %>%
  ggplot(aes(area_trabajo2,n)) + geom_col() + coord_flip()

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
  filter(n<10, area_trabajo2!="")

# QUIENES (NO) CONOCEN ASRA -------------------- 

table(data$asra_conoce)

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
  geom_histogram(binwidth = 5, position = "fill")

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
  ggplot(aes(genero, fill=recomienda)) + 
  geom_bar(position = "fill") + coord_flip() +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>% 
  filter(edad>19) %>%
  ggplot(aes(edad, fill=recomienda)) +
  geom_histogram(binwidth = 5, position = "fill") +
  geom_hline(yintercept = corte_recomienda, color = "black", linetype = "dotted", size = 2)

data %>% 
  filter(edad>19) %>%
  ggplot(aes(edad, fill=recomienda)) +
  geom_histogram(binwidth = 5)

# RAZONES -------------------- 

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
    # mutate(word = lematizar(word)) %>%
    mutate(word = wordStem(word, language = "es")) %>%
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

# GRUPOS -------------------- 

# grupos? 1 trabajan en privada; 2 investigadores; 3 estado; 4 docencia universitaria; 5 docencia no universitaria; 7 ong






# 2DO: cruzar universidad y residencia
# 2DO: codificacion abierta de razones de recomendacion


# 2DO: ver ethos y razones de la sociologia en distintos momentos (90, 00, y ahora)
# 2DO: una nueva tension caba/federal, asi como profesionales/academicos




# ETIQUETAS ABIERTAS ------------


glimpse(data)

data %>%
  select(marca,recomienda,recomienda_razones,recomienda_etiquetas) %>%
  mutate(etiquetas = str_split(recomienda_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>% 
  slice_max(order_by = n, n = 10, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
    geom_col() +
  coord_flip() +
  facet_wrap(~recomienda, scales = "free")
  
data %>%
  select(marca,recomienda,estudio_razones,estudio_etiquetas) %>%
  mutate(etiquetas = str_split(estudio_etiquetas, pattern=fixed(" "))) %>%
  select(marca, recomienda, etiquetas ) %>%
  unnest_longer(etiquetas, keep_empty = FALSE) %>%
  filter(!is.na(etiquetas)) %>%
  count(recomienda,etiquetas) %>%
  slice_max(order_by = n, n = 10, by = recomienda) %>%
  ggplot(aes(x=etiquetas, y=n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~recomienda, scales = "free")

  
  
  
  
  
