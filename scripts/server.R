source("global.R")
shinyServer(function(input, output) {
# REACTIVE DATA
  reactive_df_campania_persona <- reactive({
    df_campania_vacunacion_persona %>%
      filter(DEPARTAMENTO_NOMBRE == input$departamento | input$departamento == "Todos")
  })

  reactive_df_campania_dosis <- reactive({
    df_campania_vacunacion_dosis %>%
      filter(DEPARTAMENTO_NOMBRE == input$departamento | input$departamento == "Todos")
  })
  
  reactive_df_nacidos_vivos_ine <- reactive({
    df_nacidos_vivos_ine %>%
      filter(DEPARTAMENTO == input$departamento | input$departamento == "Todos")
  })
  
  # Filtros para todos los gráficos -------------------------------------------
  
  output$departamento_ddriss <- renderUI({
    if (input$rb_seleccion_in == "Departamento") {
      label <- "El filtro se aplica por departamento"
      choise <- unique(df_campania_vacunacion_persona$DEPARTAMENTO_NOMBRE) %>% sort(decreasing = FALSE) %>% .[-1]
      titleBox <- paste0("Linea de tiempo por el departamento de", input$rb_seleccion_in)
    }else {
      label <- "El filtro se aplica por DDRISS"
      choise <- unique(df_campania_vacunacion_persona$AREA_SALUD) %>% sort(decreasing = FALSE)
      titleBox <- paste0("Linea de tiempo por la DDRISS de", input$rb_seleccion_in)
    }
    selectInput(inputId = "departamento", label = label, choices = c("Todos", choise))
  })

  # Grafica de linea de tiempo por departamento, DDRISS y si es en total para todos --------- 
  output$avance_campania_vacunacion <- renderPlotly({
    # obtiene la informacion de la poblacion por departamento 
    new_bd_poblacion <- reactive_df_campania_persona() %>% 
      select(
        DEPARTAMENTO_NOMBRE,
        TOTAL_NINAS,
        TOTAL_NINOS,
        FECHA_VACUNACION
      ) %>% gruop_by(
        DEPARTAMENTO_NOMBRE,
        FECHA_VACUNACION
      )%>% mutate(
        POBLACION = TOTAL_NINAS + TOTAL_NINOS
      ) %>% summarise(
        POBLACION = sum(POBLACION)
      )
    
    # obtiene la informacion de la vacunacion por departamento
    new_bd_vacunacion <- reactive_df_campania_dosis () %>% 
      mutate(
        TOTAL_DOSIS = TOTAL_DOSIS_SPR + TOTAL_DOSIS_OPV,
      ) %>% sumarise(
        TOTAL_DOSIS = sum(TOTAL_DOSIS),
        TOTAL_DOSIS_SPR = sum(TOTAL_DOSIS_SPR),
        TOTAL_DOSIS_OPV = sum(TOTAL_DOSIS_OPV)
      )
      select (
        ANIO_COHORTE,
        DEPARTAMENTO_NOMBRE,
        AREA_SALUD,
        TOTAL_DOSIS_SPR,
        TOTAL_DOSIS_OPV,
        FECHA_VACUNACION
      ) %>% group_by(
        ANIO_COHORTE,
        DEPARTAMENTO_NOMBRE,
        AREA_SALUD, 
        FECHA_VACUNACION
      )
      
      #Creando la tabla con la informacion de la vacunacion y la poblacion
      tabla_cacunados <- merge(new_bd_poblacion, new_bd_vacunacion, by = "DEPARTAMENTO_NOMBRE")
      
      # creando la grafic
      

    
    
  })
  
  output$avance_campania_vacunacion_ddriss <- renderPlotly({
    df <- reactive_df_campania_persona()
    df <- df %>% group_by(FECHA_VACUNACION) %>% summarise(Total = sum(TOTAL))
    df <- df %>% arrange(FECHA_VACUNACION)
    plot_ly(df, x = ~FECHA_VACUNACION, y = ~Total, type = 'scatter', mode = 'lines', name = 'Total', line = list(color = 'blue')) %>%
      layout(title = "Avance de la campaña de vacunación por DDRISS", xaxis = list(title = "Fecha de vacunación"), yaxis = list(title = "Total de vacunados"))
  })
  
  
  
})