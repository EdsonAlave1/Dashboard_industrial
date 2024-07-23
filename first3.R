library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(readxl)
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
                           numericInput("N", "Tamaño del lote (N):", value = 1000, min = 1),
                           numericInput("NCA", "Número de Calidad Aceptable (NCA) (%):", value = 0.4, min = 0, max = 100, step = 0.01),
                           numericInput("NCL", "Nivel de Calidad Límite (NCL) (%):", value = 2.5, min = 0, max = 100, step = 0.01),
                           numericInput("alpha", "Nivel de significancia (alpha):", value = 0.05, min = 0, max = 1, step = 0.01),
                           numericInput("beta", "Nivel de riesgo del productor (beta):", value = 0.10, min = 0, max = 1, step = 0.01),
                           actionButton("calcularBtn", "Calcular")
                       )
                ),
                column(width = 6,
                       box(title = "Datos de Inspección", status = "primary", solidHeader = TRUE, width = NULL,
                           DTOutput("dataInspeccion")
                       ),
                       box(title = "Resultado", status = "success", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("resultado")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
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
                           numericInput("sample_size", "Tamaño de la muestra (n):", value = 30, min = 1),
                           numericInput("mean", "Media esperada (μ):", value = 0),
                           numericInput("sd", "Desviación estándar (σ):", value = 1),
                           numericInput("L", "Límite de especificación (L):", value = 2),
                           numericInput("k", "Factor de seguridad (k):", value = 1),
                           numericInput("AQL", "Nivel de Calidad Aceptable (AQL):", value = 0.01),
                           numericInput("RQL", "Nivel de Calidad Rechazable (RQL):", value = 0.05),
                           actionButton("calcularMilBtn", "Calcular")
                       )
                ),
                column(width = 6,
                       box(title = "Plan de Muestreo para Variable (MIL STD 414)", status = "primary", solidHeader = TRUE, width = NULL,
                           plotOutput("milStd414Plot")
                       )
                )
              )
      ),
      tabItem(tabName = "hotelling",
              fluidRow(
                column(width = 6,
                       box(title = "Cartas de Control Multivariado (Método T² de Hotelling)", status = "primary", solidHeader = TRUE, width = NULL,
                           fileInput("file3", "Selecciona el archivo de Excel", accept = ".xlsx"),
                           numericInput("n", "Tamaño de la muestra:", 10),
                           numericInput("alphaT2", "Nivel de significancia (0.01):", 0.01, min = 0.001, max = 0.1, step = 0.001),
                           numericInput("g", "Número de subgrupos:", 10),
                           actionButton("updateT2", "Actualizar Análisis")
                       )
                ),
                column(width = 6,
                       box(title = "Resultados de T² de Hotelling", status = "primary", solidHeader = TRUE, width = NULL,
                           tableOutput("dataTableT2"),
                           plotOutput("controlPlotT2")
                       )
                )
              )
      ),

  #tguchi
  tabItem(tabName = "hotelling",
          fluidRow(
            box(title = "Cartas de Control Multivariado (Método T² de Hotelling)", status = "primary", solidHeader = TRUE, width = 12,
                fileInput("file3", "Selecciona el archivo de Excel", accept = ".xlsx"),
                numericInput("n", "Tamaño de la muestra:", 10),
                numericInput("alphaT2", "Nivel de significancia (0.01):", 0.01, min = 0.001, max = 0.1, step = 0.001),
                numericInput("g", "Número de subgrupos:", 10),
                actionButton("updateHotelling", "Actualizar Análisis"),
                tableOutput("dataHotelling"),
                plotOutput("hotellingPlot")
            )
          )
  ),
  tabItem(tabName = "taguchi",
          fluidRow(
            box(title = "Filosofía de Taguchi para el Control Fuera de Línea", status = "primary", solidHeader = TRUE, width = 12,
                fileInput("file4", "Selecciona el archivo de Excel", accept = ".xlsx"),
                radioButtons("method", "Seleccionar Método",choices = c("Nominal es mejor", "Menor es mejor", "Mayor es mejor")),
                uiOutput("paramsUI"),
                actionButton("update", "Actualizar Análisis"),
                DTOutput("results")
            )),
                   
             
              
            
          
  ),
        tabItem(tabName = "sixsigma",
              fluidRow(
                column(width = 6,
                       box(title = "Análisis Six Sigma", status = "primary", solidHeader = TRUE, width = NULL,
                           numericInput("sigmaTarget", "Meta de Sigma:", value = 6),
                           numericInput("defects", "Número de Defectos:", value = 0),
                           numericInput("units", "Número de Unidades:", value = 100),
                           actionButton("calcularSixSigmaBtn", "Calcular")
                       )
                ),
                column(width = 6,
                       box(title = "Resultados Six Sigma", status = "primary", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("resultadoSixSigma")
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
    
    # Calcular p1 y p2
    p1 <- NCA / 100
    p2 <- NCL / 100
    
    # Calcular Rc
    Rc <- p2 / p1
    
    # Determinar R
    R <- N * p1
    
    #xdxdxd
    index <- which.min(abs(tabla_cameron[[columna_R]] - Rc))
    c <- tabla_cameron$c[index]
    n <- n_p1_values[index] / p1
    n_p1_values[index]
    p1
    n_p1_values[index]/p1
    
    # Tabla de Cameron
    tabla_cameron <- data.frame(
      c = 0:25,
      R_alpha_0_05_beta_0_10 = c(44.89, 10.95, 6.51, 4.89, 4.06, 3.55, 3.21, 2.96, 2.77, 2.62, 2.50, 2.40, 2.31, 2.24, 2.17, 2.12, 2.07, 2.03, 1.99, 1.95, 1.92, 1.89, 1.86, 1.84, 1.82, 1.79),
      R_alpha_0_05_beta_0_05 = c(58.80, 13.35, 7.70, 5.67, 4.65, 4.02, 3.60, 3.30, 3.07, 2.75, 2.63, 2.53, 2.44, 2.37, 2.30, 2.25, 2.19, 2.14, 2.10, 2.06, 2.03, 2.00, 1.97, 1.94, 1.92, 1.92),
      R_alpha_0_05_beta_0_01 = c(89.78, 18.68, 10.28, 7.35, 5.89, 4.43, 4.01, 3.70, 3.46, 3.26, 3.10, 2.96, 2.85, 2.75, 2.66, 2.58, 2.52, 2.45, 2.40, 2.35, 2.30, 2.26, 2.22, 2.19, 2.15, 2.12),
      stringsAsFactors = FALSE
    )
    
    # Resultado de Cameron
    resultado_cameron <- paste("c = ", c, "\n")
    resultado_cameron <- paste(resultado_cameron, "n = ", n )
    
    output$resultado <- renderText({
      resultado_cameron
    })
    
    output$dataInspeccion <- renderDT({
      tabla_cameron
    })
    
    # Gráfico de Cameron
    output$cameronPlot <- renderPlot({
      ggplot(tabla_cameron, aes(x = c)) +
        geom_line(aes(y = R_alpha_0_05_beta_0_10, color = "Alpha 0.05, Beta 0.10")) +
        geom_line(aes(y = R_alpha_0_05_beta_0_05, color = "Alpha 0.05, Beta 0.05")) +
        geom_line(aes(y = R_alpha_0_05_beta_0_01, color = "Alpha 0.05, Beta 0.01")) +
        labs(x = "Número de Defectos", y = "Valor de R", title = "Curva CO para el Plan de Muestreo (Atributo)") +
        theme_minimal()
    })
  })
  
  observeEvent(input$calcularMilBtn, {
    # Leer parámetros del usuario
    n <- req(input$sample_size)
    mean <- req(input$mean)
    sd <- req(input$sd)
    L <- req(input$L)
    k <- req(input$k)
    AQL <- req(input$AQL)
    RQL <- req(input$RQL)
    
    # Calcular límites de especificación
    LCL <- mean - k * sd / sqrt(n)
    UCL <- mean + k * sd / sqrt(n)
    
    # Resultados
    output$milStd414Plot <- renderPlot({
      x <- seq(mean - 4*sd, mean + 4*sd, length.out = 100)
      y <- dnorm(x, mean, sd)
      
      ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
        geom_line() +
        geom_vline(xintercept = LCL, linetype = "dashed", color = "red") +
        geom_vline(xintercept = UCL, linetype = "dashed", color = "red") +
        labs(x = "Valor", y = "Densidad", title = "Plan de Muestreo para Variable (MIL STD 414)") +
        theme_minimal()
    })
  })
  
  observeEvent(input$updateHotelling, {
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
  
  observeEvent(input$updatesix, {
    req(dataset())
    data <- dataset()
    
    # Crear una carta de control tipo X-bar
    q <- qcc(data$Lote1, type = "xbar.one")
    
    # Realizar el análisis de capacidad del proceso
    capability <- process.capability(q, spec.limits = c(input$LSL, input$USL))
    
    # Calcular los valores de sigma
    mean_value <- mean(data$Lote1)
    sd_value <- sd(data$Lote1)
    sigma_lines <- data.frame(
      sigma = c(-3, -2, -1, 1, 2, 3),
      value = mean_value + c(-3, -2, -1, 1, 2, 3) * sd_value
    )
    
    # Mostrar la carta de control
    output$controlChart <- renderPlot({
      plot(q)
    })
    
    # Generar gráfico de capacidad del proceso con etiquetado de sigmas
    output$capabilityChart <- renderPlot({
      ggplot(data, aes(x = Lote1)) +
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
    output$capabilityResults <- renderPrint({
      print(capability)
      
      cat("Indicadores de Capacidad del Proceso:\n")
      cat("Cp:", capability$cp, "\n")
      cat("Cpk:", capability$cpk, "\n")
      cat("Pp:", capability$pp, "\n")
      cat("Ppk:", capability$ppk, "\n")
    })
  })

}

shinyApp(ui = ui, server = server)
