library(tidyverse)
library(googlesheets4)
library(readr)

df_db <- tibble(
  dias = 0:as.numeric((Sys.Date()-as.Date("2022-12-07"))),
  fechas = seq(as.Date("2022-12-07"), Sys.Date(), by="days")
)
df_db

fallecidos <- read_csv("fallecidos_db/data/fallecidos.csv", 
                       col_types = cols(fecha_fallecimiento = col_date(format = "%d/%m/%Y"), 
                                        fecha_hospitalizacion = col_date(format = "%d/%m/%Y"), 
                                        circunstancia = col_factor(levels = c("enfrentamientos", 
                                                                              "accidente o bloqueo"))))

f_x_dia <- fallecidos %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(fallecidos = n(), 
  .groups = "drop") 

df_plot <- left_join(df_db, f_x_dia, by =  c("fechas" = "fecha_fallecimiento")) %>% 
  

ggplot(df_plot, 
       aes(x = fechas, y = cumsum(fallecidos))) + 

geom_line(stat = "identity")  

sum(f_x_dia$fallecidos)



tot_dias <- 


tribble(
  ~"pres", ~"fallecido"
)

# cargar data de google sheets
todes_les_ministres <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1YNbmNsvi84CwhpZGmf0Ziuo-x5GOTloKcJFZC8t8mvA/edit#gid=206694415",
  col_types = "ccccc")


# setear inicio de gobiernos
inicio_gob <- tibble(
  pres = c("Toledo", "García II", "Humala", "Kuczynski", "Vizcarra", 
           "Sagasti", "Castillo"),
  fecha = c("2001-07-28", "2006-07-28", "2011-07-28", "2016-07-28", "2018-03-23",
            "2020-11-17", "2021-07-28"),
  final = c("2006-07-27", "2011-07-27", "2016-07-27", "2018-03-22", "2020-11-16",
            "2021-07-27", NA_character_),
  col = c("#4BA479", "#808080", "#E3B155", "#BAA6A5", "#5297AC",
                   "#5B4E77", "#AC6E69" )) %>% 
                     arrange(desc(fecha))

# inicio_gob$pres <- factor(inicio_gob$pres, c("Toledo", "García II", "Humala", "Kuczynski", "Vizcarra", 
#                                              "Sagasti", "Castillo"))
cols <- setNames(inicio_gob$col, 
                 unique(inicio_gob$pres))


# formatear tabla de todos los ministros por presidente
pres <- todes_les_ministres %>% 
  mutate(pre = Presidente,
         Periodo = str_replace(Periodo, "setiembre", "septiembre"),
         inicio_gob = case_when(
           pre == "Toledo" ~    pull(inicio_gob[inicio_gob$pres=="Toledo"   ,"fecha"]),
           pre == "García II" ~ pull(inicio_gob[inicio_gob$pres=="García II","fecha"]),
           pre == "Humala" ~    pull(inicio_gob[inicio_gob$pres=="Humala"   ,"fecha"]),
           pre == "Kuczynski" ~ pull(inicio_gob[inicio_gob$pres=="Kuczynski","fecha"]),
           pre == "Vizcarra" ~   pull(inicio_gob[inicio_gob$pres=="Vizcarra","fecha"]),
           pre == "Sagasti" ~   pull(inicio_gob[inicio_gob$pres=="Sagasti"  ,"fecha"]),
           pre == "Castillo" ~  pull(inicio_gob[inicio_gob$pres=="Castillo" ,"fecha"])
         )) %>% 
  separate(Periodo, into = c("inicio", "fin"), sep = "-", remove = F) %>% #revisar warning Expected 2 pieces. Additional pieces discarded in 1 rows [291]. 
  mutate(inicio = as.Date(lubridate::parse_date_time(inicio, "dmy", 
                                                     locale = "es_ES.UTF-8"))) %>% 
  mutate(
    dia = as.numeric(
      difftime(as.Date(inicio),
               as.Date(inicio_gob),
               units = "days"))) %>% 
  arrange(inicio)

# crear df para graficar
df<- pres %>% 
  group_by(pre, dia) %>%
  summarise(pre = first(pre),
            n = n(),
            .groups = "drop") %>% 
  complete(nesting(pre), dia = seq(0, 1830, 1L)) %>%
  mutate(n = replace_na(n, 0),
         n = case_when(
           dia <= 10 ~ 0, # se descartan las designaciones de los primeros diez días de gobierno
           TRUE ~ as.numeric(n)
         )) %>%
  group_by(pre) %>% 
  mutate(cambios = cumsum(n)) %>% 
  filter()
df$pre <- factor(df$pre, c("Toledo", "García II", "Humala", "Kuczynski", "Vizcarra", 
                           "Sagasti", "Castillo"))

# crear lista de df para aplicar la función de gráficos
df_5a <- df %>% 
  mutate(label = if_else(dia == max(dia), as.character(pre), NA_character_))

df_castillo <- df %>% 
  filter(dia < as.numeric(Sys.Date()-as.Date(inicio_gob$fecha[inicio_gob$pres=="Castillo"]))) %>% 
  mutate(label = if_else(dia == max(dia), as.character(pre), NA_character_))

lista <- list( '5 años' = df_5a, 'Primeros días' = df_castillo)



graficar <- function(df) {
  ggplot(df, 
         aes(x = dia, y = cambios, group = pre, colour = pre))+
    geom_line(size = 2) +
    scale_colour_manual(values = cols) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
    ggrepel::geom_label_repel(
      aes(label = label),
      family = "Sanchez",
      direction = "y",
      hjust = "left",
      nudge_x = 5,
      min.segment.length = 500) +
    theme(
      text = element_text(family = "Sanchez"),
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(color = "#808080"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E0CF"),
      plot.background = element_rect(fill = "#F9F7F3"),
      panel.background = element_rect(fill = "#F9F7F3"),
      panel.border = element_blank(),
      legend.position = "none")
}

lapply(lista, graficar)

ggsave("fig_output/contador_ministros.png")
