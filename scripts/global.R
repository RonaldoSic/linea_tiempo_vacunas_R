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
color_fondo <- "#ffffff"

# Colores por la vacuna 
color_opv <- "#00c0ef"
color_spr <- "#3c8dbc"
color_dot_spr <- "#00004d"
color_dot_opv <- "#600080"
color_line_spr <- "#002E5B"
color_line_opv <- "#002E5B"
