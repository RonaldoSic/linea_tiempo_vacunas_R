# rm(list = ls()[grep("^color_", ls())])
# rm(bd_poblacion)
# rm(list = ls()[grep("^bd_", ls())])

filtro_departamento <- 1
# rm(filtro_departamento)
anio_cohorte <- 2019
# rm(anio_cohorte)
has_filter <- filtro_departamento >= 1 & filtro_departamento <= 22
# rm(has_filter)


# ------------- SE EXTRAEN LOS A LA POBLACIÓN VACUNADA Y A LAS DOSIS APLICADAS -------------
bd_poblacion <- df_campania_vacunacion_persona %>% select(
  FECHA_VACUNACION,
  DEPARTAMENTO_NOMBRE,
  DEPARTAMENTO_ID,
  MUNICIPIO_NOMBRE,
  MUNICIPIO_ID,
  TOTAL_NINOS,
  TOTAL_NINAS
) %>% mutate(
  POBLACION_TOTAL_VACUNADA = TOTAL_NINOS + TOTAL_NINAS
) %>% group_by(FECHA_VACUNACION, DEPARTAMENTO_ID, DEPARTAMENTO_NOMBRE) %>% 
  summarise(
    POBLACION_TOTAL_VACUNADA = sum(POBLACION_TOTAL_VACUNADA)
  )

if (has_filter) {
  bd_poblacion <- bd_poblacion %>% filter(DEPARTAMENTO_ID == filtro_departamento)
}

bd_poblacion

# ---- TRATAMIENTO DE DATOS DE VACUNAS ----

bd_vacunas <- df_campania_vacunacion_dosis %>% select(
  FECHA_VACUNACION,
  DEPARTAMENTO_NOMBRE,
  DEPARTAMENTO_ID,
  TOTAL_DOSIS_SPR,
  TOTAL_DOSIS_OPV,
  ANIO_COHORTE
) %>% group_by(FECHA_VACUNACION, DEPARTAMENTO_ID, DEPARTAMENTO_NOMBRE) %>%
  summarise(
    TOTAL_DOSIS_SPR = sum(TOTAL_DOSIS_SPR),
    TOTAL_DOSIS_OPV = sum(TOTAL_DOSIS_OPV)
  )

# bd_vacunas <- df_campania_vacunacion_dosis %>% pivot_wider(
#   names_from = ANIO_COHORTE,
#   values_from = c(TOTAL_DOSIS_SPR, TOTAL_DOSIS_OPV)
# )


if (has_filter) {
  bd_vacunas <- bd_vacunas %>% filter(DEPARTAMENTO_ID == filtro_departamento)
}

bd_vacunas <- bd_vacunas %>% mutate(
  cobertura_spr_acumulado = cumsum(TOTAL_DOSIS_SPR + TOTAL_DOSIS_SPR),
  cobertura_opv_acumulado = cumsum(TOTAL_DOSIS_OPV)
)

bd_vacunas


# bd_nacidos_ine <- df_nacidos_vivos_ine %>% select(
#   ANIO, 
#   DEPARTAMENTO,
#   IDDEP,
#   POBLACION
# ) %>% group_by(ANIO, IDDEP, DEPARTAMENTO) %>% 
#   summarise(
#     POBLACION_TOTAL = sum(POBLACION),
#   )

bd_nacidos_ine <- df_nacidos_vivos_ine %>% pivot_wider(
  names_from = ANIO,
  values_from = POBLACION
) %>% select(
  IDDEP,
  DEPARTAMENTO,
  MUNICIPIO,
  IDMUN,
  `2019`,
  `2020`,
  `2021`,
  `2022`
) %>% rename(
  POBLACION_2019 = `2019`,
  POBLACION_2020 = `2020`,
  POBLACION_2021 = `2021`,
  POBLACION_2022 = `2022`
) %>% mutate(
  POBLACION_TOTAL = POBLACION_2019 + POBLACION_2020 + POBLACION_2021 + POBLACION_2022
) %>% select(
  IDDEP,
  DEPARTAMENTO,
  POBLACION_2019,
  POBLACION_2020,
  POBLACION_2021,
  POBLACION_2022,
  POBLACION_TOTAL
) %>% group_by(IDDEP, DEPARTAMENTO) %>% 
  summarise(
    POBLACION_TOTAL = sum(POBLACION_TOTAL)
    
  )

bd_nacidos_ine

if (has_filter) {
  bd_nacidos_ine <- bd_nacidos_ine %>% filter(IDDEP == filtro_departamento)
} else {
  # sumar todos los departamentos para obtener la población total aunque no se pueda agrupar por departamento ni por id
  bd_nacidos_ine_nacional <- bd_nacidos_ine %>% summarise(
    IDDEP = 0,
    POBLACION_TOTAL = sum(POBLACION_TOTAL, na.rm = TRUE)
  ) %>% group_by(IDDEP) %>% 
    summarise(
      POBLACION_TOTAL = sum(POBLACION_TOTAL)
    )
}

bd_nacidos_ine
# bd_nacidos_ine_nacional

# rm(tabla_vacunados)
tabla_vacunados <- bd_poblacion %>% left_join(bd_vacunas, 
                                              by = c("FECHA_VACUNACION", "DEPARTAMENTO_ID", "DEPARTAMENTO_NOMBRE")) %>% 
  mutate(
    # si tiene filtro se usa la población total de nacidos vivos del INE, de lo contrario se usa la población total de nacidos vivos del INE nacional
    VACUNAS_SPR_ACUMULADO = cumsum(TOTAL_DOSIS_SPR), #Debe de ser la suma acumulada de las vacunas aplicadas, por ejemplo si en el primer día se aplicaron 10 vacunas, en el segundo día 20, en el tercer día 30, entonces el acumulado del tercer día sería 60 
    VACUNAS_OPV_ACUMULADO = cumsum(TOTAL_DOSIS_OPV),
    COBERTURA_SPR_ACUMULADO = ifelse(has_filter, round((VACUNAS_SPR_ACUMULADO / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2), 
                           round((VACUNAS_SPR_ACUMULADO / bd_nacidos_ine_nacional$POBLACION_TOTAL) * 100, 2)),
    COBERTURA_OPV_ACUMULADO = ifelse(has_filter, round((VACUNAS_OPV_ACUMULADO / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2), 
                           round((VACUNAS_OPV_ACUMULADO / bd_nacidos_ine_nacional$POBLACION_TOTAL) * 100, 2))
  ) %>% select(
    DEPARTAMENTO_ID,
    DEPARTAMENTO_NOMBRE,
    FECHA_VACUNACION,
    POBLACION_TOTAL_VACUNADA,
    TOTAL_DOSIS_SPR,
    TOTAL_DOSIS_OPV,
    COBERTURA_SPR_ACUMULADO,
    COBERTURA_OPV_ACUMULADO,
    VACUNAS_SPR_ACUMULADO,
    VACUNAS_OPV_ACUMULADO,
  )

tabla_vacunados

# ---- CONSTRUYENDO LA GRAFICA ----
# rm(grafica_avance_campania_vacunacion)
grafica_avance_campania_vacunacion <- plot_ly(
  tabla_vacunados, 
  x = ~FECHA_VACUNACION, 
  y = ~TOTAL_DOSIS_SPR,
  type = 'bar', 
  name = 'Vacuna SPR', 
  hovertemplate = "%{y}",
  marker = list(color = color_principal)
) %>% add_trace(
  y = ~TOTAL_DOSIS_OPV,
  name = 'Vacuna OPV',
  hovertemplate = "%{y}",
  marker = list(color = color_secundario)
) %>% add_trace(
  y = ~COBERTURA_SPR, 
  name = 'Cobertura SPR (%)',
  type = 'scatter',
  mode = 'lines', 
  yaxis = 'y2',
  hovertemplate = "%{y}%",
  line = list(color = color_terciario, width = 2, dash = 'dashdot'),
  marker = list(color = color_disenio)
) %>% add_trace(
  y = ~COBERTURA_OPV, 
  name = 'Cobertura OPV (%)',
  type = 'scatter',
  mode = 'lines', 
  yaxis = 'y2',
  hovertemplate = "%{y}%",
  line = list(color = color_disenio, width = 2, dash = 'dashdot'),
  marker = list(color = color_disenio3)
) %>% layout(
  title = 'Avance de la campaña de vacunación',
  xaxis = list(title = 'Fecha de vacunación', 
               showgrid = FALSE, 
               zeroline = FALSE, 
               fixedrange = TRUE, 
               tickformat = '%d-%B-%Y'),
  yaxis = list(title = 'Total de dosis aplicadas', 
               rangemode = 'tozero', 
               showgrid = FALSE, 
               zeroline = FALSE, 
               fixedrange = TRUE, 
               tickformat = ',d'),
  yaxis2 = list(title = 'Cobertura (%)', 
                overlaying = 'y', 
                side = 'right', 
                rangemode = 'tozero', 
                showgrid = FALSE,
                zeroline = FALSE,
                fixedrange = TRUE),
  barmode = 'group',
  titlefont = list(size = 16),
  margin = list(l = 50, r = 50, b = 50, t = 50),
  hovermode = 'x unified',
  paper_bgcolor = color_fondo,
  plot_bgcolor = color_fondo
) %>% config(
  locale = 'es',
  displayModeBar = FALSE,
  modeBarButtonsToRemove = c("select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "zoom", "pan", "autoscale"),
  toImageButtonOptions = list(
    format = "png",
    filename = "comparacion_vacunas_diaria",
    height = 600,
    width = 1600,
    scale = 1
  )
)

grafica_avance_campania_vacunacion



# ---------------------
# fig <- plot_ly(tabla_vacunados, x = ~fecha, y = ~vacuna_srp, 
#                type = 'bar', name = 'Vacuna SPR', 
#                hovertemplate = "%{y}",
#                marker = list(color = color_spr)
# ) %>% 
#   add_trace(y = ~vacuna_opv, name = 'Vacuna OPV',
#             hovertemplate = "%{y}",
#             marker = list(color = color_opv)
#   ) %>% 
#   add_trace(y = ~cober_srp_acum, type = 'scatter', name ="Cobertura SPR (%)", 
#             mode = 'lines', yaxis = "y2",
#             hovertemplate = "%{y}",
#             #color = color_line_cob_spr, 
#             #marker = list(color = color_line_cob_spr)
#             line = list(color = color_line_cob_spr, dash = 'dashdot', width = 2),
#             marker = list(color = "red", symbol = 'circle')
#   ) %>% 
#   add_trace(y = ~cober_opv_acum, type = 'scatter', name = "Cobertura OPV (%)",
#             mode = 'lines', 
#             yaxis = "y2",
#             hovertemplate = "%{y}",
#             # color = color_line_cob_opv,
#             # marker = list(color = color_line_cob_opv)
#             line = list(color = color_line_cob_opv, dash = 'dot', width = 2),
#             marker = list(color = "green", symbol = 'square')
#   ) %>%
#   layout(
#     yaxis = list(
#       title = 'Cantidad de vacunas administradas',
#       showgrid = FALSE,
#       showline = TRUE,
#       rangemode = "tozero",
#       fixedrange = TRUE
#     ),
#     yaxis2 = list(
#       title = 'Cobertura (%)', 
#       overlaying = "y1",
#       rangemode = "tozero",
#       side = 'right',
#       showgrid = FALSE,
#       showline = TRUE,
#       # disable zooming
#       fixedrange = TRUE
#     ),
#     xaxis = list(
#       title = 'Fecha',
#       showgrid = FALSE,
#       showline = TRUE,
#       # disable zooming
#       fixedrange = TRUE,
#       tickformatstops = list(
#         list(dtickrange = list(NULL, "M1"), value = "%d-%b-%Y"), # Formato de fecha para días
#         list(dtickrange = list("M1", NULL), value = "%b-%Y")    # Formato de fecha para meses
#       )
#     ),
#     title = list(
#       text = "Vacunación diaria de SPR y OPV",
#       x = 0.5,
#       size = 28
#     ),
#     barmode = 'group',
#     # Add margin for legend
#     margin = list(l = 40, r = 40, t = 100, b = 40),
#     # Other plot configs
#     legend = list(orientation = 'h', y = -0.3, x = 0.5, xanchor = 'center'), # Centrar la leyenda
#     plot_bgcolor = "#ededed",
#     paper_bgcolor = "#FFFFFF",
#     hovermode = 'x unified'
#   ) %>%
#   config(
#     locale = 'es',
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = c("select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "zoom", "pan", "autoscale"),
#     toImageButtonOptions = list(
#       format = "png",
#       filename = "comparacion_vacunas_diaria",
#       height = 600,
#       width = 1600,
#       scale = 1
#     )
#   )





# ----------------- Otros calculos -----------------

# Población total de nacidos vivos por departamento o a nivel nacional

other_bd_ine = df_nacidos_vivos_ine %>% 
  select(
    ANIO,
    DEPARTAMENTO,
    IDDEP,
    POBLACION
  ) %>% 
  group_by(IDDEP, DEPARTAMENTO) %>%
  summarise(
    POBLACION_TOTAL = sum(POBLACION)
  ) %>% filter(IDDEP == 3)


other_bd_ine

bd_nacidos_ine



# Calculando el total de dosis SPR y OPV aplicada por cada año de cohorte, 2019, 2020, 2021 y 2022
other_bd_dosis = df_campania_vacunacion_dosis %>% 
  select(
    FECHA_VACUNACION,
    DEPARTAMENTO_NOMBRE,
    DEPARTAMENTO_ID,
    TOTAL_DOSIS_SPR,
    TOTAL_DOSIS_OPV,
    ANIO_COHORTE
  ) %>% 
  group_by(ANIO_COHORTE) %>% 
  summarise(
    TOTAL_DOSIS_SPR = sum(TOTAL_DOSIS_SPR),
    TOTAL_DOSIS_OPV = sum(TOTAL_DOSIS_OPV)
  ) %>% 
  filter(DEPARTAMENTO_ID == 3)

other_bd_dosis
