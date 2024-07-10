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
            # Un espacio para poder mostrar el id del departamento seleccionado y el id de la DDRIS seleccionada
            # column(
            #   width = 12,
            #   h3("Departamento seleccionado:"),
            #   h4(textOutput(outputId = "id_depto_seleccionado")),
            #   h3("DDRISS seleccionada:"),
            #   h4(textOutput(outputId = "id_ddriss_seleccionada")),
            #   h3("Se aplica filtro?"),
            #   h4(textOutput(outputId = "aplica_filtro"))
            # )
          ),
          tabBox(
            title = p(textOutput(outputId = "titulo_tabla_vacunados")),
            id = "id_tab_linea_tiempo",
            tabPanel(
              "Linea del tiempo",
              width = 12,
              minHeight = "450px",
              height = "auto",
              collapsible = TRUE,
              plotlyOutput(outputId = "avance_campania_vacunacion")
              # shinydashboard::box(
              #   width = 12,
              #   
              # )
            ),
            tabPanel(
              "Linea de tiempo por DDRISS",
              width = 12,
              minHeight = "450px",
              height = "auto",
              collapsible = TRUE,
              plotlyOutput(outputId = "avance_campania_vacunacion_ddriss")
              # shinydashboard::box(
              #   width = 12,
              # )
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
