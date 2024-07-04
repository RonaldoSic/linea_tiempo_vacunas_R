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
    new_bd_poblacion <- reactive_df_campania_persona() %>% 
      group_by(FECHA_VACUNACION = as.Date(FECHA_VACUNACION)) %>%
      select(
        DEPARTAMENTO = DEPARTAMENTO_NOMBRE,
        MUNICIPIO = MUNICIPIO_NOMBRE
      )
    
    
    
  })
  
  output$avance_campania_vacunacion_ddriss <- renderPlotly({
  })
  
  
  
})