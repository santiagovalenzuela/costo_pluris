rm(list=ls(all=T))

library(showtext)
#font_add_google(name = "Lato", family = "lato")
showtext_auto()

library(openxlsx)
library(tidyverse)
library(treemapify)

# Cargamos PEF 2021
pef21 <- read.xlsx(
  "https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/DatosAbiertos/Bases_de_datos_presupuesto/XLSX/PEF_2021.xlsx")

total_pef <- sum(pef21$MONTO_APROBADO)

# Filtramos las remuneraciones de los diputados (dietas y gratificaciones)
pef21 <- pef21 %>%
  mutate(subgrupo =
           case_when((ID_RAMO == 1) &       # Legislativo 
                       (ID_UR == 100) &     # H. Cámara de Diputados
                       (ID_PARTIDA_ESPECIFICA == 11101|  # Dietas
                          ID_PARTIDA_ESPECIFICA == 13202) ~ "dietas" # Gratificaciones
                     ))

# Calculamos el total de dietas y gratificaciones
total_dietas <- pef21 %>%
  filter(ID_RAMO == 1) %>% # Legislativo
  filter(ID_UR == 100) %>% # H. Cámara de Diputados
  filter(ID_PARTIDA_ESPECIFICA == 11101| # Dietas
           ID_PARTIDA_ESPECIFICA == 13202) %>% # Gratificaciones
  select(MONTO_APROBADO) %>%
  sum()


# Agrupamos por ramo
ramos <- pef21 %>% group_by(DESC_RAMO, subgrupo) %>%
  summarize(monto = sum(MONTO_APROBADO))

ramos <- ramos %>% ungroup()

# Creamos una nueva línea con el costo de las dietas de los pluris
dietas_pluris <- ramos %>% filter(subgrupo == "dietas")
dietas_pluris$monto <- (dietas_pluris$monto*0.40) 
dietas_pluris$DESC_RAMO <- "Dietas plurinominales"

# Restamos el costo de los pluris al df ramos y cambiamos algunas variables
ramos$DESC_RAMO[is.na(ramos$subgrupo == "dietas") == F] <- "Dietas Legislativo"
ramos$monto[is.na(ramos$subgrupo == "dietas") == F] <-
  ramos$monto[is.na(ramos$subgrupo == "dietas") == F]*0.6
ramos$subgrupo[is.na(ramos$subgrupo == "dietas") == F] <- NA

# Añadimos la línea de la dieta de los pluris que creamos a "ramos"
ramos <- ramos %>% ungroup() %>% add_row(dietas_pluris)

# Verificamos que la suma de los montos sea correcta
sum(ramos$monto) == total_pef

# Damos nombre a los colores que usaremos en la gráfica
azul <- "#3421e0"
rojo <- "#e02b21"

# Creamos la gráfica
grafica <-ggplot(ramos, aes(area = monto,
                  label = DESC_RAMO,
                  fill = subgrupo
                  )) +
  
  geom_treemap(color = "white",
               start = "topleft") +
  
  geom_treemap_text(alpha = 0.5,
                    color = "white",
                    start = "topleft",
                    grow = T,
                    reflow = T) +
  
  labs(title='"Si quitáramos los pluris, ahorraríamos mucho dinero"',
       subtitle = "En rojo, lo que nos ahorraríamos",
       caption = "Fuente: Presupuesto de Egresos de la Federación, 2021") +
  
  scale_fill_manual(aesthetics = "fill",
                    na.value= azul,
                    values = rojo) +
  
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  
  coord_cartesian(expand = FALSE, clip = "off") +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "lato"),
        legend.position = "none",
        plot.title = element_text(hjust = 0,
                                  color = azul,
                                  family = "lato",
                                  face = "bold"),
        plot.caption = element_text(hjust = 0))


# Añadimos una flecha para señalar dónde se encuentran las 
grafica +
  geom_segment(
    aes(x = 0.75, y = 0.25, xend = 0.98, yend = 0.02),
    arrow = arrow(length = unit(0.03, "npc"), 
                  type = "closed"),
    color = azul,
    size = 2,
    angle = 0,
    linejoin='mitre'
  ) +

  geom_segment(
  aes(x = 0.75, y = 0.25, xend = 0.98, yend = 0.02),
  arrow = arrow(length = unit(0.03, "npc"), 
                type = "open"),
  color = "white",
  size = 2,
  angle = 0,
  linejoin='mitre'
)


png(file= "costo_pluris.png")

showtext_auto(FALSE)
