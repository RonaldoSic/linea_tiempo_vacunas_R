source("global.R")
shinyServer(function(input, output) {
# REACTIVE DATA
  reactive_df_campania_persona <- reactive({
    df_campania_vacunacion_persona %>%
      fileter(DEPARTAMENTO_ID != range(1, 22))
  })

  reactive_df_campania_dosis <- reactive({
    df_campania_vacunacion_dosis %>%
      filter(DEPARTAMENTO_ID != range(1, 22))
  })

})