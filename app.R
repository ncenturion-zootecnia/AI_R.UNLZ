library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

archivo <- "Rendimientos_decada.xlsx"

if (!file.exists(archivo)) {
  stop("No se encontró Rendimientos_decada.xlsx en el directorio de la aplicación.")
}

datos <- read_excel(archivo, na = c("", "NA", "SD"))

variables_requeridas <- c(
  "Cultivo", "Campaña", "Sup. Sembrada", "Sup. Cosechada"
)
variables_faltantes <- setdiff(variables_requeridas, names(datos))

if (length(variables_faltantes) > 0) {
  stop(
    "Faltan variables requeridas en el Excel: ",
    paste(variables_faltantes, collapse = ", ")
  )
}

if (!is.numeric(datos[["Sup. Sembrada"]]) ||
    !is.numeric(datos[["Sup. Cosechada"]])) {
  stop("Las columnas de superficie deben ser numéricas.")
}

cultivos <- sort(unique(stats::na.omit(datos$Cultivo)))

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Seleccioná un cultivo:",
        choices = c("Todos los cultivos", cultivos),
        selected = "Todos los cultivos"
      ),
      helpText(
        "Las superficies se suman por campaña para el cultivo seleccionado."
      )
    ),
    mainPanel(
      plotOutput("grafico", height = "500px"),
      textOutput("aviso")
    )
  )
)

server <- function(input, output, session) {
  datos_filtrados <- reactive({
    if (input$cultivo == "Todos los cultivos") {
      datos
    } else {
      filter(datos, Cultivo == input$cultivo)
    }
  })

  resumen <- reactive({
    datos_filtrados() |>
      group_by(Campaña) |>
      summarise(
        `Sup. Sembrada` = if (all(is.na(.data[["Sup. Sembrada"]]))) {
          NA_real_
        } else {
          sum(.data[["Sup. Sembrada"]], na.rm = TRUE)
        },
        `Sup. Cosechada` = if (all(is.na(.data[["Sup. Cosechada"]]))) {
          NA_real_
        } else {
          sum(.data[["Sup. Cosechada"]], na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      arrange(
        suppressWarnings(as.integer(sub("/.*$", "", as.character(Campaña)))),
        Campaña
      ) |>
      mutate(Campaña = factor(Campaña, levels = unique(Campaña))) |>
      pivot_longer(
        cols = c("Sup. Sembrada", "Sup. Cosechada"),
        names_to = "Superficie",
        values_to = "Hectareas"
      )
  })

  output$grafico <- renderPlot({
    grafico_datos <- resumen()
    validate(
      need(nrow(grafico_datos) > 0, "No hay datos para el cultivo seleccionado."),
      need(
        any(!is.na(grafico_datos$Hectareas)),
        "No hay superficies disponibles para mostrar."
      )
    )

    ggplot(
      grafico_datos,
      aes(
        x = Campaña,
        y = Hectareas,
        color = Superficie,
        group = Superficie
      )
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      scale_color_manual(
        values = c("Sup. Sembrada" = "#2878B5", "Sup. Cosechada" = "#E07A2D")
      ) +
      labs(
        x = "Campaña",
        y = "Superficie (hectáreas)",
        color = "Tipo de superficie"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$aviso <- renderText({
    filas_sin_superficie <- sum(
      is.na(datos_filtrados()[["Sup. Sembrada"]]) &
        is.na(datos_filtrados()[["Sup. Cosechada"]])
    )
    if (filas_sin_superficie > 0) {
      paste(
        filas_sin_superficie,
        "observaciones sin superficies disponibles se excluyen del gráfico."
      )
    }
  })
}

shinyApp(ui = ui, server = server)
