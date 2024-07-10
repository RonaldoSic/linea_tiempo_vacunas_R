source("global.R")
shinyServer(function(input, output) {
# REACTIVE DATA
  reactive_df_campania_persona <- reactive({
    # df_campania_vacunacion_persona %>%
    #   filter(DEPARTAMENTO_ID == input$departamento | input$departamento == "Todos")
    # El filtro debe ser eliminando todos los departamentos que no estén en el rango de 1 a 22
    df_campania_vacunacion_persona %>%
      filter(DEPARTAMENTO_ID == input$departamento | input$departamento == "Todos") %>%
      filter(DEPARTAMENTO_ID >= 1 & DEPARTAMENTO_ID <= 22)
  })

  reactive_df_campania_dosis <- reactive({
    df_campania_vacunacion_dosis %>%
      filter(DEPARTAMENTO_ID == input$departamento | input$departamento == "Todos")
  })
  
  reactive_df_nacidos_vivos_ine <- reactive({
    df_nacidos_vivos_ine %>%
      filter(IDDEP == input$departamento | input$departamento == "Todos") %>% 
      filter(IDDEP >= 1 & IDDEP <= 22)
  })
  
  reactive_has_filter <- reactive({
    ifelse(input$departamento != "Todos" & as.numeric(input$departamento) >= 1 & as.numeric(input$departamento) <= 22, TRUE, FALSE)
  })
  
  
  # Filtros para todos los gráficos -------------------------------------------
  # 
  # output$departamento_ddriss <- renderUI({
  #   if (input$rb_seleccion_in == "Departamento") {
  #     label <- "El filtro se aplica por departamento"
  #     choise <- unique(df_campania_vacunacion_persona$DEPARTAMENTO_NOMBRE) %>% sort(decreasing = FALSE) %>% .[-1]
  #     titleBox <- paste0("Linea de tiempo por el departamento de", input$rb_seleccion_in)
  #   }else {
  #     label <- "El filtro se aplica por DDRISS"
  #     choise <- unique(df_campania_vacunacion_persona$AREA_SALUD) %>% sort(decreasing = FALSE)
  #     titleBox <- paste0("Linea de tiempo por la DDRISS de", input$rb_seleccion_in)
  #   }
  #   selectInput(inputId = "departamento", label = label, choices = c("Todos", choise))
  # })
  valor_id_depto_select <- 0
  valor_id_ddriss_select <- 0
  has_filter <- FALSE
  # remove(valor_id_ddriss_select, valor_id_depto_select, has_filter)
  # remove(has_filter)
  
  
  # -------------Espacio para visualizar los filtros seleccionados ----------------
  output$departamento_ddriss <- renderUI({
    if (input$rb_seleccion_in == "Departamento") {
      label <- "El filtro se aplica por departamento"
      choices_df <- df_campania_vacunacion_persona %>%
        distinct(DEPARTAMENTO_ID, DEPARTAMENTO_NOMBRE) %>%
        filter(DEPARTAMENTO_ID >= 1 & DEPARTAMENTO_ID <= 22) %>%
        arrange(DEPARTAMENTO_NOMBRE)
      choices <- setNames(choices_df$DEPARTAMENTO_ID, choices_df$DEPARTAMENTO_NOMBRE)
      valor_id_depto_select <- as.numeric(input$departamento_ddriss)
    }else{
      label <- "El filtro se aplica por DDRISS"
      choices_df <- df_campania_vacunacion_persona %>%
        distinct(IDAS, AREA_SALUD) %>%
        arrange(AREA_SALUD)
      choices <- setNames(choices_df$IDAS, choices_df$AREA_SALUD)
      valor_id_ddriss_select <- as.numeric(input$departamento_ddriss)
    }
    selectInput(inputId = "departamento", label = label, choices = c("Todos" = "Todos", choices))
  })

  
  # ---------------- Mensajes dinamico que retorna el id del departamento seleccionado, concatenado con el nombre del departamento
  # output$id_depto_seleccionado <- renderText({
  #   if (input$departamento == "Todos") {
  #     "Todos los departamentos"
  #   } else {
  #     paste0("ID: ", input$departamento, " - ", df_campania_vacunacion_persona$DEPARTAMENTO_NOMBRE[df_campania_vacunacion_persona$DEPARTAMENTO_ID == as.numeric(input$departamento)][1])
  #   }
  # })
  # 
  # output$id_ddriss_seleccionada <- renderText({
  #   if (input$departamento == "Todos") {
  #     "Todas las DDRISS"
  #   } else {
  #     paste0("ID: ", input$departamento, " - ", df_campania_vacunacion_persona$AREA_SALUD[df_campania_vacunacion_persona$IDAS == as.numeric(input$departamento)][1])
  #   }
  # })
  # 
  # 
  # output$aplica_filtro <- renderText({
  #   has_filter <- reactive_has_filter()
  #   ifelse(has_filter, "Si", "No")
  # })
  
  # "Avance de la campaña de vacunación "
  
  output$titulo_tabla_vacunados <- renderText({
    if (input$rb_seleccion_in == "Departamento") {
      if (input$departamento == "Todos") {
        titleBox <- "Línea de tiempo nivel nacional"
      } else {
        # titleBox <- paste0("Linea de tiempo por el departamento de", df_campania_vacunacion_persona$AREA_SALUD[df_campania_vacunacion_persona$IDAS == as.numeric(input$departamento)][1])
        titleBox <-paste0("Línea de tiempo del departamento de ", df_campania_vacunacion_persona$DEPARTAMENTO_NOMBRE[df_campania_vacunacion_persona$DEPARTAMENTO_ID == as.numeric(input$departamento)][1])
      }
    }else {
      if (input$departamento == "Todos") {
        titleBox <- "Línea de tiempo nivel nacional"
      } else {
        titleBox <- paste0("Linea de tiempo por la DDRISS de", df_campania_vacunacion_persona$AREA_SALUD[df_campania_vacunacion_persona$IDAS == as.numeric(input$departamento)][1])
      }
    }
    titleBox
  })
  

  # Grafica de linea de tiempo por departamento, DDRISS y si es en total para todos --------- 
  output$avance_campania_vacunacion <- renderPlotly({
    # Tratado de datos para la grafica de avance de la campaña de vacunación
    bd_poblacion <- reactive_df_campania_persona() %>% select(
      FECHA_VACUNACION,
      DEPARTAMENTO_NOMBRE,
      DEPARTAMENTO_ID,
      MUNICIPIO_NOMBRE,
      MUNICIPIO_ID,
      TOTAL_NINOS,
      TOTAL_NINAS
    ) %>% mutate(
      TOTAL_POBLACION_VACUNADOS_DIARIO = TOTAL_NINOS + TOTAL_NINAS
    ) %>% group_by(FECHA_VACUNACION, DEPARTAMENTO_ID, MUNICIPIO_ID) 

    bd_vacunas <- reactive_df_campania_dosis() %>% select(
      FECHA_VACUNACION,
      DEPARTAMENTO_NOMBRE,
      DEPARTAMENTO_ID,
      TOTAL_DOSIS_SPR,
      TOTAL_DOSIS_OPV,
      ANIO_COHORTE
    ) %>% group_by(FECHA_VACUNACION, DEPARTAMENTO_ID) %>%
      summarise(
        TOTAL_DOSIS_SPR = sum(TOTAL_DOSIS_SPR),
        TOTAL_DOSIS_OPV = sum(TOTAL_DOSIS_OPV)
      )

    bd_nacidos_ine <- reactive_df_nacidos_vivos_ine() %>% pivot_wider(
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
      summarise(POBLACION_TOTAL = sum(POBLACION_TOTAL))
    
    if (reactive_has_filter()) {
      bd_nacidos_ine <- bd_nacidos_ine %>% filter(IDDEP == as.numeric(input$departamento))
    } else {
      # sumar todos los departamentos para obtener la población total aunque no se pueda agrupar por departamento ni por id
      bd_nacidos_ine <- bd_nacidos_ine %>% summarise(
        IDDEP = 0,
        POBLACION_TOTAL = sum(POBLACION_TOTAL, na.rm = TRUE)
      ) %>% group_by(IDDEP) %>%
        summarise(
          POBLACION_TOTAL = sum(POBLACION_TOTAL)
        )
    }
    # 
    # rm(tabla_vacunados)
    tabla_vacunados <- bd_vacunas %>% group_by(FECHA_VACUNACION) %>%
      summarise(TOTAL_DOSIS_SPR = sum(TOTAL_DOSIS_SPR),
                TOTAL_DOSIS_OPV = sum(TOTAL_DOSIS_OPV) 
                ) %>% mutate(
        # si tiene filtro se usa la población total de nacidos vivos del INE, de lo contrario se usa la población total de nacidos vivos del INE nacional
        VACUNAS_SPR_ACUMULADO = cumsum(TOTAL_DOSIS_SPR),
        VACUNAS_OPV_ACUMULADO = cumsum(TOTAL_DOSIS_OPV),
        COBERTURA_SPR_ACUMULADO = round((VACUNAS_SPR_ACUMULADO / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2),
        COBERTURA_OPV_ACUMULADO = round((VACUNAS_OPV_ACUMULADO / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2),
        FECHA_VACUNACION = as.Date(FECHA_VACUNACION)
      )
    
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
      y = ~COBERTURA_SPR_ACUMULADO, 
      name = 'Cobertura SPR (%)',
      type = 'scatter',
      mode = 'lines',
      yaxis = 'y2',
      hovertemplate = "%{y}%",
      line = list(color = "#10FF00", dash = 'dashdot', width = 2),
      marker = list(color = "#333333", symbol = 'circle')
    ) %>% add_trace(
      y = ~COBERTURA_OPV_ACUMULADO,
      name = 'Cobertura OPV (%)',
      type = 'scatter',
      mode = 'lines', 
      yaxis = 'y2',
      hovertemplate = "%{y}%",
      line = list(color = "#202020", width = 2, dash = 'dashdot'),
      marker = list(color = "#fFaafF")
    ) %>% layout(
      title = 'Avance de la campaña de vacunación',
      xaxis = list(title = 'Fecha de vacunación',
                   fill = 'tozeroy',
                   rangemode = 'tozero',
                   showgrid = FALSE,
                   showline = TRUE,
                   # zeroline = FALSE, 
                   fixedrange = TRUE,
                   tickformatstops = list(
                     list(dtickrange = list(NULL, "M1"), value = "%d-%b-%Y") # Formato de fecha para días
                     # list(dtickrange = list("M1", NULL), value = "%b-%Y")    # Formato de fecha para meses
                    )
                   # El formato de fecha quiero que sea asi como este 01-04-24
                   # tickformat = "%d-%m-%y"
                   ),
      yaxis = list(title = 'Total de dosis aplicadas', 
                   rangemode = 'tozero', 
                   showgrid = FALSE, 
                   showline = TRUE,
                   # zeroline = FALSE, 
                   fixedrange = TRUE
                   # tickformat = ',d'
                   ),
      yaxis2 = list(title = 'Cobertura (%)', 
                    overlaying = 'y1', 
                    side = 'right', 
                    rangemode = 'tozero', 
                    showgrid = FALSE,
                    showline = TRUE,
                    zeroline = FALSE,
                    fixedrange = TRUE
                    ),
      barmode = 'group',
      titlefont = list(size = 16),
      margin = list(l = 40, r = 40, b = 100, t = 50),
      legend = list(orientation = 'h', y = -0.8, x = 0.5, xanchor = 'center'), # Centrar la leyenda
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
  })
  
  output$avance_campania_vacunacion_ddriss <- renderPlotly({
  })
  
  
  
})