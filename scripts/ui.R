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
        fluidRow(
          tabBox(
            title = "Linea de tiempo de la campaña",
            id = "tabset1",
            tabPanel(
              "Linea de tiempo por semana",
              h2("Avance de la campaña"),
              plotOutput("plot1")
            ),
            tabPanel(
              "Linea de tiempo por Distrito",
              h2("Avance de la campaña por distrito"),
              plotOutput("plot2")
            ),
            width = 12
          )          
        )
      )
    )
  )
)
