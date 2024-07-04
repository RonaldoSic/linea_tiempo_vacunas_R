source("global.R")

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "dashboard", icon = icon("home")),
      menuItem("Glosario", tabName = "widgets", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        # Apartado de filtros
        fluidRow(
          box(
            title = "Filtros",
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            position = "left",
            width = 12,
            column(
              width = 4,
              radioButtons(
                inputId = "rb_seleccion_in",
                label = "Seleccionar",
                choices = c("Departamento", "DDRISS"),
                selected = "Departamento"
              )
            ),
            column(
              width = 8,
              uiOutput(outputId = "departamento_ddriss")
            )
          ),
          tabBox(
            title = "Avance de la campaña de vacunación",
            id = "id_tab_linea_tiempo",
            tabPanel(
              "Linea del tiempo",
              h3("Avance de la campaña de vacunación por departamento"),
              width = 12,
              minHeight = "450px",
              height = "auto",
              collapsible = TRUE,
              plotlyOutput(outputId = "avance_campania_vacunacion")
            ),
            tabPanel(
              "Linea de tiempo por DDRISS",
              h3("Avance de la campaña por DDRISS"),
              width = 12,
              minHeight = "450px",
              height = "auto",
              collapsible = TRUE,
              plotlyOutput(outputId = "avance_campania_vacunacion_ddriss"),
            ),
            width = 12
          )          
        )
      )
    ), 
    fluidRow(
      box(
        title = "Glosario",
        status = "primary",
        collapsible = TRUE,
        width = 12,
        column(
          width = 12,
          h2("Glosario"),
          p("En esta sección se presenta un glosario de términos utilizados en la aplicación.")
        )
      )
    ), 
    fluidRow(
      box(
        title = "Información",
        status = "primary",
        collapsible = TRUE,
        width = 12,
        column(
          width = 12,
          h2("Información"),
          p("En esta sección se presenta información sobre la aplicación.")
        )
      )
    )
  )
)
