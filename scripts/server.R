source("global.R")
shinyServer(function(input, output) {
# REACTIVE DATA
  reactive_df_campania_persona <- reactive({
    df_campania_vacunacion_persona %>%
      filter(DEPARTAMENTO_ID == input$departamento | input$departamento == "Todos")
  })

  reactive_df_campania_dosis <- reactive({
    df_campania_vacunacion_dosis %>%
      filter(DEPARTAMENTO_ID == input$departamento | input$departamento == "Todos")
  })
  
  reactive_df_nacidos_vivos_ine <- reactive({
    df_nacidos_vivos_ine %>%
      filter(IDDEP == input$departamento | input$departamento == "Todos")
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
  output$departamento_ddriss <- renderUI({
    if (input$rb_seleccion_in == "Departamento") {
      label <- "El filtro se aplica por departamento"
      choices_df <- df_campania_vacunacion_persona %>%
        distinct(DEPARTAMENTO_ID, DEPARTAMENTO_NOMBRE) %>%
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
    # si ha seleccionado un departamento y el id esta entre 1 y 22 entonces si tiene filtro
    # if (input$departamento != "Todos" & as.numeric(input$departamento) >= 1 & as.numeric(input$departamento) <= 22) {
    #   has_filter <- TRUE
    # }
    # selectInput(inputId = "departamento", label = label, choices = c("Todos", choices))
    selectInput(inputId = "departamento", label = label, choices = c("Todos" = "Todos", choices))
  })
  
  has_filter <- reactive({
    if (input$departamento != "Todos" & as.numeric(input$departamento) >= 1 & as.numeric(input$departamento) <= 22) {
      TRUE
    } else {
      FALSE
    }
  })
  
  

  # Grafica de linea de tiempo por departamento, DDRISS y si es en total para todos --------- 
  output$avance_campania_vacunacion <- renderPlotly({
    
    bd_poblacion <- reactive_df_campania_persona() %>% select(
      FECHA_VACUNACION,
      DEPARTAMENTO_NOMBRE,
      DEPARTAMENTO_ID,
      MUNICIPIO_NOMBRE,
      MUNICIPIO_ID,
      TOTAL_NINOS,
      TOTAL_NINAS
    ) %>% mutate(
      POBLACION_TOTAL_VACUNADA = TOTAL_NINOS + TOTAL_NINAS,
      ANIO = year(FECHA_VACUNACION)
    ) %>% group_by(FECHA_VACUNACION, DEPARTAMENTO_ID, DEPARTAMENTO_NOMBRE, ANIO) %>% 
      summarise(
        POBLACION_TOTAL_VACUNADA = sum(POBLACION_TOTAL_VACUNADA)
      )
    
    if (has_filter) {
      bd_poblacion <- bd_poblacion %>% filter(DEPARTAMENTO_ID == filtro_departamento)
    }
    
    bd_vacunas <- reactive_df_campania_dosis() %>% select(
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
    
    if (has_filter) {
      bd_vacunas <- bd_vacunas %>% filter(DEPARTAMENTO_ID == filtro_departamento)
    }
    

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
      summarise(
        POBLACION_TOTAL = sum(POBLACION_TOTAL)
      )
    
    
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
    
    # rm(tabla_vacunados)
    tabla_vacunados <- bd_poblacion %>% left_join(bd_vacunas, 
                                                  by = c("FECHA_VACUNACION", "DEPARTAMENTO_ID", "DEPARTAMENTO_NOMBRE")) %>% 
      mutate(
        # si tiene filtro se usa la población total de nacidos vivos del INE, de lo contrario se usa la población total de nacidos vivos del INE nacional
        COBERTURA_SPR = ifelse(has_filter, round((TOTAL_DOSIS_SPR / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2), 
                               round((TOTAL_DOSIS_SPR / bd_nacidos_ine_nacional$POBLACION_TOTAL) * 100, 2)),
        COBERTURA_OPV = ifelse(has_filter, round((TOTAL_DOSIS_OPV / bd_nacidos_ine$POBLACION_TOTAL) * 100, 2), 
                               round((TOTAL_DOSIS_OPV / bd_nacidos_ine_nacional$POBLACION_TOTAL) * 100, 2))
      ) %>% select(
        DEPARTAMENTO_ID,
        DEPARTAMENTO_NOMBRE,
        FECHA_VACUNACION,
        POBLACION_TOTAL_VACUNADA,
        TOTAL_DOSIS_SPR,
        TOTAL_DOSIS_OPV,
        COBERTURA_SPR,
        COBERTURA_OPV
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
    
    
    
    
  })
  
  output$avance_campania_vacunacion_ddriss <- renderPlotly({
  })
  
  
  
})