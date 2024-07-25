library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(readxl)
library(pracma)
library(dplyr)
library(MASS)
library(qcc)
library(knitr)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Control de Calidad Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pantalla Principal", tabName = "principal", icon = icon("home")),
      menuItem("Método de Cameron", tabName = "cameron", icon = icon("tasks")),
      menuItem("Método de MIL STD 414", tabName = "mil_std_414", icon = icon("chart-bar")),
      menuItem("Método de T² de Hotelling", tabName = "hotelling", icon = icon("line-chart")),
      menuItem("Control Fuera de Línea (Taguchi)", tabName = "taguchi", icon = icon("cogs")),
      menuItem("Six Sigma", tabName = "sixsigma", icon = icon("tasks"))
    )
  ),
  dashboardBody(
    tags$head(
      # Agregar CSS personalizado
      tags$style(HTML("
        body {
          background-color: #2e2e2e;
          color: #eaeaea;
        }
        .box {
          background-color: #ffffff;
          color: #000000;
        }
        .main-header .logo {
          background-color: #1e1e1e;
          color: #eaeaea;
        }
        .main-header .navbar {
          background-color: #1e1e1e;
        }
        .sidebar {
          background-color: #1e1e1e;
          color: #eaeaea;
        }
        .sidebar .sidebar-menu > li > a {
          color: #eaeaea;
        }
        .sidebar .sidebar-menu > li.active > a {
          background-color: #333333;
        }
        .control-sidebar {
          background-color: #1e1e1e;
          color: #eaeaea;
        }
        .content-wrapper {
          background-color: #2e2e2e;
        }
        .content {
          background-color: #2e2e2e;
        }
        .table {
          color: #eaeaea;
        }
        .table thead th {
          background-color: #444444;
          color: #eaeaea;
        }
        .table tbody td {
          background-color: #333333;
          color: #eaeaea;
        }
        .table-striped tbody tr:nth-of-type(odd) {
          background-color: #2e2e2e;
        }
        .table-bordered {
          border: 1px solid #444444;
        }
        .table-bordered th,
        .table-bordered td {
          border: 1px solid #444444;
        }
        .taguchi-results-table {
          background-color: #ffffff;
          color: #000000; /* Color del texto en la tabla de resultados */
        }
        .taguchi-results-table thead th {
          background-color: #e0e0e0; /* Fondo blanco claro para los encabezados */
          color: #000000;
        }
        .taguchi-results-table tbody td {
          background-color: #ffffff;
          color: #000000;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "principal",
              fluidRow(
                column(width = 12,
                       box(
                         title = "Bienvenido al Dashboard de Control de Calidad", 
                         status = "primary", 
                         solidHeader = TRUE, 
                         width = NULL,
                         h3("Introducción"),
                         p("Este dashboard proporciona herramientas avanzadas para el análisis y diseño de control de calidad en diversos contextos. Está diseñado para ayudar a los usuarios a aplicar técnicas estadísticas y de ingeniería de calidad para mejorar procesos y productos."),
                         p("Utiliza las secciones a continuación para explorar y aplicar diferentes métodos de control de calidad y análisis. Cada sección está equipada con herramientas específicas que te permitirán realizar análisis detallados y tomar decisiones informadas."),
                         p("Navega a través de los menús para acceder a las siguientes secciones:"),
                         tags$ul(
                           tags$li("Plan de Muestreo (Atributo): Diseña planes de muestreo utilizando el método de Cameron para evaluar la calidad de los productos."),
                           tags$li("Plan de Muestreo (Variable): Realiza análisis basados en el método MIL STD 414 para evaluar variables continuas."),
                           tags$li("Control Multivariado: Aplica el método T² de Hotelling para el análisis multivariado de datos de control."),
                           tags$li("Control Fuera de Línea (Taguchi): Utiliza la filosofía de Taguchi para mejorar la robustez y calidad del proceso."),
                           tags$li("Six Sigma: Evalúa y mejora la calidad del proceso utilizando herramientas Six Sigma.")
                         ),
                         p("Cada sección está equipada con herramientas interactivas para que puedas ingresar datos, calcular resultados y visualizar gráficos."),
                         #h3("Gráfico de Ejemplo"),
                         plotOutput("examplePlot")
                       )
                )
              )
      ),
      tabItem(tabName = "cameron",
              fluidRow(
                column(width = 6,
                       box(title = "Diseño de Plan de Muestreo para Atributo (Método de Cameron)", status = "primary", solidHeader = TRUE, width = NULL,
                           h3("Ingreso de Parámetros"),
                           numericInput("N", "Tamaño del lote (N):", value = "", min = 1),
                           numericInput("NCA", "Número de Calidad Aceptable (NCA) (%):", value = "", min = 0, max = 100, step = 0.01),
                           numericInput("NCL", "Nivel de Calidad Límite (NCL) (%):", value = "", min = 0, max = 100, step = 0.01),
                           numericInput("alpha", "Nivel de significancia (alpha):", value = "", min = 0, max = 1, step = 0.01),
                           numericInput("beta", "Nivel de riesgo del productor (beta):", value = "", min = 0, max = 1, step = 0.01),
                           actionButton("calcularBtn", "Calcular"),
                           verbatimTextOutput("errorMessage")
                       )
                ),
                column(width = 6,
                       box(title = "Resultado", status = "success", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("resultado")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Curva CO (Curva Operativa)", status = "warning", solidHeader = TRUE, width = NULL,
                           plotOutput("cameronPlot")
                       )
                ),
              )
      ),
      tabItem(tabName = "mil_std_414",
              fluidRow(
                column(width = 6,
                       box(title = "Diseño de Plan de Muestreo para Variable (Método MIL STD 414)", status = "primary", solidHeader = TRUE, width = NULL,
                           
                             numericInput("lote_size", "Tamaño del lote:", value = "", min = 1),
                             selectInput("nivel_inspeccion", "Nivel de Inspección:", choices = c("I", "II", "III", "IV", "V")),
                             numericInput("nivel_calidad", "Nivel de calidad ):", value = "", min = 0.001, max = 0.1, step = 0.001),
                             actionButton("calcular414", "calcular")
                           
                           
                           
                       )
                ),
                column(width = 6,
                       box(title = "Plan de Muestreo para Variable (MIL STD 414)", status = "success", solidHeader = TRUE, width = NULL,
                           textOutput("tamanio_muestra"),
                           textOutput("muestra"),
                           textOutput("valor_tabla")
                       )
                ),
                column(width = 12,
                       box(title = "Grafica para Variable (MIL STD 414)", status = "success", solidHeader = TRUE, width = NULL,
                            plotOutput("grafica")
                       )
                ),
              )
      ),
      tabItem(tabName = "hotelling",
              fluidRow(
                column(width = 6,
                       box(title = "Cartas de Control Multivariado (Método T² de Hotelling)", status = "primary", solidHeader = TRUE, width = NULL,
                           fileInput("file3", "Selecciona el archivo de Excel", accept = ".xlsx"),
                           numericInput("n", "Tamaño de la muestra:", ""),
                           numericInput("alphaT2", "Nivel de significancia (0.01):", "", min = 0.001, max = 0.1, step = 0.001),
                           numericInput("g", "Número de subgrupos:", ""),
                           actionButton("updateT2", "Actualizar Análisis")
                       )
                ),
                column(width = 6,
                       box(title = "Resultados de T² de Hotelling", status = "success", solidHeader = TRUE, width = NULL,
                           tableOutput("dataHotelling")
                       )
                ),
                column(width = 12,
                       box(title = "Resultados de T² de Hotelling", status = "success", solidHeader = TRUE, width = NULL,
                           
                           plotOutput("hotellingPlot")
                       )
                )
              )
      ),

  #tguchi
  tabItem(tabName = "taguchi",
          fluidRow(
            box(title = "Filosofía de Taguchi para el Control Fuera de Línea", status = "primary", solidHeader = TRUE, width = 12,
                fileInput("file4", "Selecciona el archivo de Excel", accept = ".xlsx"),
                radioButtons("method", "Seleccionar Método",choices = c("Nominal es mejor", "Menor es mejor", "Mayor es mejor")),
                uiOutput("paramsUI"),
                actionButton("update", "Actualizar Análisis"),
                DTOutput("results")
            ))),

        tabItem(tabName = "sixsigma",
                fluidRow(
                  column(width = 6,
                         box(title = "Six Sigma", status = "success", solidHeader = TRUE, width = 12,
                             fileInput("file5", "Seleccionar archivo Excel", accept = c(".xlsx")),
                             uiOutput("columnSelector"),
                             numericInput("LSL", "Límite Inferior de Especificación (LSL):", ""),
                             numericInput("USL", "Límite Superior de Especificación (USL):", ""),
                             numericInput("numSigmas", "Número de Sigmas:", value = "", step = 1, min = 1),
                             actionButton("updatesix", "Actualizar Análisis")
                         )
                  ),
                  column(width = 6,
                         box(title = "Resultados Six Sigma", status = "primary", solidHeader = TRUE, width = NULL,
                             plotOutput("controlChart")
                         )
                  ),
                  column(width = 12,
                         box(title = "Resultados Six Sigma", status = "primary", solidHeader = TRUE, width = NULL,
                             plotOutput("capabilityChart")
                         )
                  ),
                  column(width = 6,
                         box(title = "Resultados Six Sigma", status = "primary", solidHeader = TRUE, width = NULL,
                             verbatimTextOutput("capabilityResults")
                         )
                  )
                )
        )
    )
  )
)

# Definir el servidor
server <- function(input, output) {

  output$examplePlot <- renderPlot({
    # Generar datos de ejemplo
    set.seed(123) # Para reproducibilidad
    n <- 100
    data <- data.frame(
      x = 1:n,
      y = rnorm(n, mean = 50, sd = 10) # Datos simulados con media 50 y desviación estándar 10
    )
    
    # Calcular los límites de control para Six Sigma
    mean_y <- mean(data$y)
    sd_y <- sd(data$y)
    lower_limit <- mean_y - 3 * sd_y
    upper_limit <- mean_y + 3 * sd_y
    
    # Crear gráfico
    ggplot(data, aes(x = x, y = y)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = mean_y, color = "black", linetype = "dashed", size = 1) +
      geom_hline(yintercept = lower_limit, color = "red", linetype = "dotted", size = 1) +
      geom_hline(yintercept = upper_limit, color = "red", linetype = "dotted", size = 1) +
      ggtitle("Gráfico de Control") +
      xlab("Observaciones") +
      ylab("Valor") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
  })
  
  
  observeEvent(input$calcularBtn, {
    # Leer parámetros del usuario
    N <- req(input$N)
    NCA <- req(input$NCA)
    NCL <- req(input$NCL)
    alpha <- req(input$alpha)
    beta <- req(input$beta)
    
    valid_alpha <- c(0.05, 0.01)
    valid_beta <- c(0.10, 0.05, 0.01)
    
    # Mostrar mensaje de error si los valores no son válidos
    validate(
      need(alpha %in% valid_alpha, "Valor de alpha no soportado. Debe ser 0.05 o 0.01."),
      need(beta %in% valid_beta, "Valor de beta no soportado. Debe ser 0.10, 0.05 o 0.01.")
    )
    
    tabla_cameron <- data.frame(
      c = 0:25,
      R_alpha_0_05_beta_0_10 = c(44.89, 10.95, 6.51, 4.89, 4.06, 3.55, 3.21, 2.96, 2.77, 2.62, 2.50, 2.40, 2.31, 2.24, 2.17, 2.12, 2.07, 2.03, 1.99, 1.95, 1.92, 1.89, 1.86, 1.84, 1.82, 1.79),
      R_alpha_0_05_beta_0_05 = c(58.80, 13.35, 7.70, 5.67, 4.65, 4.02, 3.60, 3.30, 3.07, 2.75, 2.63, 2.53, 2.44, 2.37, 2.30, 2.25, 2.19, 2.14, 2.10, 2.06, 2.03, 2.00, 1.97, 1.94, 1.92, 1.92),
      R_alpha_0_05_beta_0_01 = c(89.78, 18.68, 10.28, 7.35, 5.89, 4.43, 4.01, 3.70, 3.46, 3.26, 3.10, 2.96, 2.85, 2.75, 2.66, 2.58, 2.52, 2.45, 2.40, 2.35, 2.30, 2.26, 2.22, 2.19, 2.15, 2.12),
      R_alpha_0_01_beta_0_10 = c(229.1, 18.68, 12.20, 8.11, 6.24, 5.19, 4.52, 4.05, 3.70, 3.44, 3.22, 3.05, 2.91, 2.79, 2.69, 2.60, 2.52, 2.45, 2.39, 2.33, 2.28, 2.24, 2.20, 2.16, 2.12, 2.09),
      R_alpha_0_01_beta_0_05 = c(298.1, 31.93, 14.43, 9.41, 7.15, 5.88, 5.08, 4.52, 4.11, 3.80, 3.55, 3.35, 3.18, 3.04, 2.92, 2.82, 2.73, 2.65, 2.58, 2.51, 2.45, 2.40, 2.35, 2.31, 2.27, 2.23),
      R_alpha_0_01_beta_0_01 = c(458.2, 44.68, 19.27, 12.20, 9.07, 7.34, 6.25, 5.50, 4.96, 4.47, 4.22, 3.95, 3.74, 3.55, 3.40, 3.26, 3.13, 3.02, 2.92, 2.82, 2.73, 2.67, 2.61, 2.51, 2.31, 2.09)
    )
    n_p1_values <- c(0.05, 0.35, 0.82, 1.36, 1.97, 2.61, 3.28, 3.98, 4.69, 5.42, 6.16, 6.92, 7.69, 8.46, 9.24, 10.03, 10.83, 11.63, 12.44, 13.25, 14.07, 14.89, 15.71, 16.54, 17.38, 18.21)
    
    
    # Calcular p1 y p2
    p1 <- NCA / 100
    p2 <- NCL / 100
    
    # Calcular Rc
    Rc <- p2 / p1
    
    # Determinar R
    R <- N * p1
    
    # Seleccionar la columna correcta basada en alpha y beta
    if (alpha == 0.05 & beta == 0.10) {
      columna_R <- "R_alpha_0_05_beta_0_10"
    } else if (alpha == 0.05 & beta == 0.05) {
      columna_R <- "R_alpha_0_05_beta_0_05"
    } else if (alpha == 0.05 & beta == 0.01) {
      columna_R <- "R_alpha_0_05_beta_0_01"
    } else if (alpha == 0.01 & beta == 0.10) {
      columna_R <- "R_alpha_0_01_beta_0_10"
    } else if (alpha == 0.01 & beta == 0.05) {
      columna_R <- "R_alpha_0_01_beta_0_05"
    } else if (alpha == 0.01 & beta == 0.01) {
      columna_R <- "R_alpha_0_01_beta_0_01"
    } else {
      stop("Combinación de alpha y beta no soportada en esta tabla simplificada.")
    }
    # Encontrar el índice más cercano
    index <- which.min(abs(tabla_cameron[[columna_R]] - Rc))
    c <- tabla_cameron$c[index]
    n <- n_p1_values[index] / p1
    

    p_values <- seq(0, 0.1, by = 0.001)
    Pa <- sapply(p_values, function(p) {
      sum(dbinom(0:c, size = n, prob = p))
    })
  

    
    # Resultado de Cameron
    resultado_cameron <- paste("c = ", c, "\n")
    resultado_cameron <- paste(resultado_cameron, "n = ", n )
    
    output$resultado <- renderText({
      resultado_cameron
    })
    
    output$dataInspeccion <- renderDT({
      tabla_cameron
    })
    
    output$cameronPlot <- renderPlot({
      # Graficar la curva CO
      plot(p_values, Pa, type = "l", col = "blue", lwd = 2, xlab = "Proporción de artículos defectuosos en el lote (p)", ylab = "Probabilidad de aceptación Pa", main = "Curva CO")
      
      # Añadir líneas y etiquetas para NCA, NCL, alpha y beta
      abline(v = p1, col = "red", lty = 2)
      abline(v = p2, col = "red", lty = 2)
      abline(h = 1 - alpha, col = "green", lty = 2)
      abline(h = beta, col = "green", lty = 2)
      
      # Añadir texto a las líneas
      text(p1, 0.5, "NCA", pos = 4, col = "red")
      text(p2, 0.5, "NCL", pos = 4, col = "red")
      text(0.02, 1 - alpha, expression(1 - alpha), pos = 4, col = "green")
      text(0.02, beta, expression(beta), pos = 4, col = "green")
      
      # Añadir caja de información
      legend("topright", legend = c(paste("n =", round(n, 2)), paste("c =", round(c, 2)), paste("NCA =", NCA, "%"), paste("NCL =", NCL, "%")), box.lty = 1)
      
    })
  })
  
  observeEvent(input$file, {
    req(input$file)
    data <- read_excel(input$file$datapath)
    output$column_selector <- renderUI({
      selectInput("selected_column", "Selecciona la columna de datos:", choices = names(data))
    })
  })
  
  observeEvent(input$calcular414, {
 
    table_12_16 <- data.frame(
      lote_size_min = c(3, 9, 16, 26, 41, 66, 111, 181, 301, 501, 801, 1301, 3201, 8001, 22001, 110001, 550001),
      lote_size_max = c(8, 15, 25, 40, 65, 110, 180, 300, 500, 800, 1300, 3200, 8000, 22000, 110000, 550000, Inf),
      I =   c("B", "B", "B", "B", "B", "B", "B", "B", "C", "D", "E", "F", "G", "H", "I", "I", "I"),
      II =  c("B", "B", "B", "B", "B", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "K", "K"),
      III = c("B", "B", "B", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L", "M", "N", "O", "P"),
      IV =  c("B", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
      V =   c("C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "L", "M", "N", "O", "P", "Q", "Q")
    )
    
    # Definir la tabla 12-17
    niveles_calidad <- c(0.04, 0.065, 0.10, 0.15, 0.25, 0.40, 0.65, 1.00, 1.50, 2.50, 4.00, 6.50, 10.00, 15.00)
    tamanio_muestra <- c(3, 4, 5, 7, 10, 15, 20, 25, 30, 35, 40, 50, 75, 100, 150, 200)
    letra_codigo <- c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
    
    table_12_17 <- list(
      c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7.59, 18.86, 26.94, 33.69, 40.47),
      c(NA, NA, NA, NA, NA, NA, NA, NA, 1.53, 5.50, 10.92, 16.45, 22.86, 29.45, 36.90),
      c(NA, NA, NA, NA, NA, NA, 1.33, 3.32, 5.83, 9.80, 14.39, 20.19, 26.56, 33.99),
      c(NA, NA, 0.422, 1.06, 2.14, 3.55, 5.35, 8.40, 12.00, 17.35, 23.29, 30.27),
      c(NA, NA, 0.349, 0.716, 1.30, 2.17, 3.26, 4.77, 7.29, 10.54, 15.17, 20.74, 27.57),
      c(0.099, 0.099, 0.312, 0.503, 0.818, 1.31, 2.11, 3.05, 4.31, 6.56, 9.46, 13.71, 18.94, 25.61),
      c(0.135, 0.135, 0.365, 0.544, 0.846, 1.29, 2.05, 2.93, 4.09, 6.17, 8.92, 12.57, 17.51, 23.53),
      c(0.155, 0.156, 0.380, 0.551, 0.877, 1.29, 2.00, 2.86, 3.97, 5.67),
      c(0.179, 0.179, 0.413, 0.581, 0.879, 1.29, 1.98, 2.83, 3.91, 5.86, 8.47, 12.36, 17.24, 23.58),
      c(0.170, 0.170, 0.388, 0.535, 0.847, 1.23, 1.87, 2.68, 3.70, 5.57, 8.10, 11.87, 16.65, 22.21),
      c(0.179, 0.179, 0.401, 0.566, 0.873, 1.26, 1.88, 2.71, 3.72, 5.58, 8.09, 11.85, 16.61, 22.20),
      c(0.163, 0.163, 0.363, 0.503, 0.789, 1.17, 1.71, 2.45, 3.27, 4.61, 6.62, 9.37, 13.05, 17.85),
      c(1.147, 1.147, 0.330, 0.467, 0.720, 1.10, 1.60, 2.29, 3.10, 4.32, 6.24, 9.08, 13.11, 18.23),
      c(0.145, 0.145, 0.317, 0.447, 0.689, 1.02, 1.53, 2.20, 3.07, 4.69, 6.91, 10.32, 14.75, 20.62),
      c(0.134, 0.134, 0.293, 0.413, 0.638, 0.94, 1.43, 2.05, 2.89, 4.43, 6.57, 9.88, 14.20, 20.02),
      c(0.135, 0.135, 0.294, 0.414, 0.637, 0.94, 1.43, 2.05, 2.89, 4.43, 6.57, 9.88, 14.12, 19.92)
    )
    
    # Definir letras y niveles de calidad
    
    
    # Función para obtener la letra correspondiente al tamaño del lote
    obtener_letra <- function(lote_size, nivel_inspeccion) {
      result <- table_12_16[table_12_16$lote_size_min <= lote_size & table_12_16$lote_size_max >= lote_size, ]
      letra <- result[[nivel_inspeccion]]
      return(as.character(letra))
    }
    obtener_tamanio_muestra <- function(letra) {
      indice <- match(letra, letra_codigo)
      if (is.na(indice)) {
        return(NA)
      }
      return(tamanio_muestra[indice])
    }
    
    # Función para obtener el valor de la tabla 12-17 basado en la letra y el nivel de calidad
    obtener_valor_tabla <- function(letra, nivel_calidad) {
      indice_fila <- match(letra, letra_codigo)
      indice_columna <- match(nivel_calidad, niveles_calidad)
      
      if (is.na(indice_fila) || is.na(indice_columna)) {
        return(NA)
      }
      
      valor <- table_12_17[[indice_fila]][indice_columna]
      return(valor)
    }
    
    lote_size <- input$lote_size
    nivel_inspeccion <- input$nivel_inspeccion
    nivel_calidad <- input$nivel_calidad
    
    letra_IV <- obtener_letra(lote_size, nivel_inspeccion)
    valor_tabla <- obtener_valor_tabla(letra_IV, nivel_calidad)
    
    output$tamanio_muestra <- renderText({
      paste("Letra de muestra para el nivel de inspección", nivel_inspeccion, "es:", letra_IV)
    })
    output$muestra<-renderText({
      paste("Tamaño de Muestra: ",tamanio)
    }
      
    )
    
    output$valor_tabla <- renderText({
paste("Nivel de calidad aceptable: ",valor_tabla)
    })
    
    output$grafica <- renderPlot({
      # Datos para la gráfica
      letra_fila <- match(letra_IV, letra_codigo)
      if (is.na(letra_fila)) return(NULL)
      
      # Crear data frame para la gráfica
      df <- data.frame(
        Nivel_Calidad = niveles_calidad,
        Valor = table_12_17[[letra_fila]]
      )
      
      ggplot(df, aes(x = Nivel_Calidad, y = Valor)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10") +
        labs(title = paste("Valores de la tabla 12-17 para la letra", letra_IV),
             x = "Nivel de Calidad",
             y = "Valor") +
        theme_minimal()
    })
    
  })
  
  observeEvent(input$updateT2, {
    # Verificar archivo
    req(input$file3)
    data <- read_excel(input$file3$datapath)
    
    # Validación de datos
    req(nrow(data) > 0)
    
    # Calcular T² y límite superior de control
    x_bar <- colMeans(data[, -1])
    S <- cov(data[, -1])
    p <- ncol(data) - 1
    
    hotelling_T2 <- function(x, x_bar, S) {
      T2 <- t(x - x_bar) %*% solve(S) %*% (x - x_bar)
      return(as.numeric(T2))
    }
    
    # Aplicar T²
    T2_values <- apply(data[, -1], 1, function(row) hotelling_T2(row, x_bar, S))
    data$T2 <- T2_values
    
    # Límite de control superior (LCS)
    LCS <- (p * (input$g + 1) * (input$g - 1)) / (input$g * (input$g - p)) * qf(1 - input$alphaT2, input$g, input$g - p)
    
    output$dataHotelling <- renderTable({
      data
    })
    
    # Gráfico de Hotelling T²
    output$hotellingPlot <- renderPlot({
      ggplot(data, aes(x = seq_len(nrow(data)), y = T2)) +
        geom_line() +
        geom_hline(yintercept = LCS, linetype = "dashed", color = "red") +
        labs(x = "Subgrupo", y = "T²", title = "Carta de Control T² de Hotelling") +
        theme_minimal()
    })
  })

        #taguchi
  
  observeEvent(input$file4, {
    req(input$file4)
    data <- read_excel(input$file4$datapath)
    
    # Gráfico de Taguchi
    
    output$paramsUI <- renderUI({
      if (input$method == "Nominal es mejor") {
        tagList(
          numericInput("m", "Valor objetivo (m):", value = 8),
          numericInput("K_nominal", "Constante de la función de pérdida (K):", value = 25)
        )
      } else if (input$method == "Menor es mejor") {
        numericInput("K_menor", "Constante de la función de pérdida (K):", value = 35.55)
      } else if (input$method == "Mayor es mejor") {
        numericInput("K_mayor", "Constante de la función de pérdida (K):", value = 5e-8)
      }
    })
    
    observeEvent(input$update, {
      req(input$file4)
      data <- read_excel(input$file4$datapath)
      
      if (input$method == "Nominal es mejor") {
        m <- input$m
        K <- input$K_nominal
        
        results <- data.frame(
          Lugar = colnames(data),
          Media = sapply(data, function(x) round(mean(x), 2)),
          Desviacion = sapply(data, function(x) round(sd(x), 2)),
          MSD = sapply(data, function(x) round(sd(x)^2 + (mean(x) - m)^2, 5)),
          Perdida = sapply(data, function(x) round(K * (sd(x)^2 + (mean(x) - m)^2), 10))
        )
        
      } else if (input$method == "Menor es mejor") {
        K <- input$K_menor
        
        results <- data.frame(
          Proveedor = colnames(data),
          Media = sapply(data, function(x) round(mean(x), 2)),
          Varianza = sapply(data, function(x) round(var(x), 2)),
          MSD = sapply(data, function(x) round(sd(x)^2 + mean(x), 5)),
          Perdida = sapply(data, function(x) round(K * (sd(x)^2 + mean(x)), 10))
        )
        
      } else if (input$method == "Mayor es mejor") {
        K <- input$K_mayor
        
        results <- data.frame(
          #Proveedor = colnames(data),
          Media = sapply(data, function(x) round(mean(x), 2)),
          Varianza = sapply(data, function(x) round(var(x), 2)),
          MSD = sapply(data, function(x) round(mean(1 / x^2), 5)),
          Perdida = sapply(data, function(x) round(K * mean(1 / x^2), 10))
        )
      }
      
      output$results <- renderDT({
        datatable(results, options = list(
          columnDefs = list(
            list(
              targets = "Perdida",
              render = JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    var value = parseFloat(data);",
                "    return value < 1e-10 ? value.toExponential(10) : value.toFixed(10);",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        ))
      })
    })})
  
    #######3
  dataset <- reactive({
    req(input$file5)
    read_excel(input$file5$datapath, sheet = 1)
  })
  
  output$columnSelector <- renderUI({
    df <- dataset()
    req(df)
    selectInput("column", "Seleccionar columna para análisis:", choices = names(df))
  })
  
  observeEvent(input$updatesix, {
    req(dataset())
    df <- dataset()
    col_name <- input$column
    
    data <- df[[col_name]]
    
    # Crear una carta de control tipo X-bar
    q <- qcc(data, type = "xbar.one")
    
    # Realizar el análisis de capacidad del proceso
    capability <- process.capability(q, spec.limits = c(input$LSL, input$USL))
    
    # Calcular los valores de sigma
    mean_value <- mean(data, na.rm = TRUE)
    sd_value <- sd(data, na.rm = TRUE)
    sigmas <- as.numeric(input$numSigmas)
    
    sigma_lines <- data.frame(
      sigma = c(-sigmas, -sigmas/2, -1, 1, sigmas/2, sigmas),
      value = mean_value + c(-sigmas, -sigmas/2, -1, 1, sigmas/2, sigmas) * sd_value
    )
    
    # Mostrar la carta de control
    output$controlChart <- renderPlot({
      plot(q)
    })
    
    # Generar gráfico de capacidad del proceso con etiquetado de sigmas
    output$capabilityChart <- renderPlot({
      ggplot(df, aes(x = data)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black") +
        geom_density(color = "red", size = 1) +
        geom_vline(aes(xintercept = input$LSL), color = "blue", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = input$USL), color = "blue", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = mean_value), color = "green", size = 1) +
        geom_vline(data = sigma_lines, aes(xintercept = value, color = as.factor(sigma)), linetype = "dotted", size = 1) +
        geom_text(data = sigma_lines, aes(x = value, y = 0.02, label = paste0(sigma, "σ")), color = "black", angle = 90, vjust = -0.5) +
        scale_color_manual(values = c("red", "red", "red", "red", "red", "red")) +
        labs(title = "Capacidad del Proceso",
             x = "Valor",
             y = "Densidad",
             color = "Sigma") +
        theme_minimal()
    })
    
    # Mostrar los resultados del análisis de capacidad
    output$capabilityResults <- renderTable({
      kable(as.data.frame(capability), format = "html", caption = "Resultados del Análisis de Capacidad")
    })
  })
}

shinyApp(ui = ui, server = server)
