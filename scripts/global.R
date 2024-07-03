# Instalar pacman si no se encuentra instalado
if (!require("pacman")) install.packages("pacman")

# Cargar librerías
pacman::p_load(
  shinydashboard,
  plotly,
  tidyverse
)

# Cargar datos
load(file = "../data/campania.rdata")


# paleta de colores primarios y secundarios
color_principal <- "#192854"
color_secundario <- "#88c0d5"
color_terciario <- "#24b2e3"
color_fondo <- "#ffffff"

# Colores para el diseño
color_disenio <- "#4388ee"
color_disenio2 <- "#283960"
color_disenio3 <- "#0558AA"

# Colores para los textos
color_texto <- "#002E5B"
color_texto2 <- "#121C33"
color_texto3 <- "#4388EE"
color_texto4 <- "#000000"


# Colores segun ciclo de vida 
color_woman <- "#ad56cd"
color_pregnant <- "#4a3b84"
color_puerperium <- "#a1dfff"
color_children_under_one_year <- "#61bbb6"
color_children_from_one_to_five_years <- "#92abc9"

# Colores por la vacuna 
color_opv <- "#f4a261"
color_spr <- "#2a9d8f"
color_dot_spr <- "#7988dd"
color_dot_opv <- "#a44878"


# variables de la campania
fecha_inicio_campania <- parse_date("2024-08-03") #Inicia la campania el 3 de agosto de 2024
fecha_fin_campania <- parse_date("2024-10-05") #Finaliza la campania el 5 de octubre de 2024
dias_por_semana <- 7 #Dias por semana

#Filtros de la campania para agrupar las campanias en semanas 
input_semana_inicial <- 1
input_semana_final <- 9

# Funcion que calcula la semana de la campania y lo agrega a la data
calcular_semana <- function(fecha_inicio, fecha_fin, dias_por_semana){
  # Calculo de la cantidad de dias entre dos fechas
  dias <- as.numeric(difftime(fecha_fin, fecha_inicio, units = "days"))
  # Calculo de la cantidad de semanas
  semanas <- (dias/dias_por_semana) + 1
  return(semanas)
}