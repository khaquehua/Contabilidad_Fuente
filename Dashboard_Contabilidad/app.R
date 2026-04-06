#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::    LIBRARIES TO USE     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(sf)
library(tm)
library(DBI)
library(RMySQL)
library(digest)
library(lubridate)
library(httr)
library(cowplot)
library(giscoR)
library(jsonlite)
library(readxl)
library(openxlsx)
library(reactable)
library(tidyr)
library(stringr)
library(tibble)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::       READ THE DATA     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
etiquetas <- function(x) {
  paste0("S/. ",x)
}

etiquetas2 <- function(x) {
  paste0(x," %")
}

etiquetas3 <- function(x) {
  paste0(x," días")
}

meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
              "Julio", "Agosto", "Septiembre", "Octubre","Noviembre","Diciembre")

# READ data de caja
update_caja_fuente <- function() {
  # VERIFICAR
  # Llamar los datos de caja
  dbcaja <- dbConnect(MySQL(), user="kevin1", host="192.168.50.45", password="NomH3-avJFoapBVN", dbname="facturacion_fuente")
  factura <- dbGetQuery(dbcaja, statement = "SELECT tipo, serie, numero, codigo, nombre, fecha, total from factura where fecha >= '2025-01-01'")
  dfactura <- dbGetQuery(dbcaja, statement = "SELECT serie, numero, producto, descripcion, cantidad, 
                         precio, total from detalle_factura")
  
  # Generar ID de merge
  factura$ID <- paste0(factura$serie,"-",factura$numero)
  dfactura$ID <- paste0(dfactura$serie,"-",dfactura$numero)
  
  # Cambiar nombre
  factura <- factura %>% select(ID, tipo, codigo, nombre, fecha, total_boleta = total)
  dfactura <- dfactura %>% select(ID, producto, descripcion, cantidad_prod = cantidad, pu = precio, cantidad_pu = total)
  
  # Unamos la data de caja
  caja <- merge(x = factura, y = dfactura, by = "ID")
  
  caja <- caja %>%
    filter(!descripcion %in% c("NO FACTURAR","AYUDA ELECT. (BV)"))
  
  # Preprocesamiento
  # Fecha
  caja$fecha <- as.Date(caja$fecha)
  
  # Descripcion
  #caja$descripcion <- ifelse(caja$descripcion %in% c("LIQ. LIMPIADOR DE LENTES","OPT. LUIQUIDO LENTES CONT.","OPT. LUIQUIDO LENTES CONT.","OPT. LIQUIDO LENTES CONT."),"LIQUIDO LIMPIADOR LENTES",
  #                           ifelse(caja$descripcion %in% c("OPT. MONTURA","OPT. MONTURAL"),"MONTURAS",caja$descripcion))
  
  dbDisconnect(dbcaja)
  
  # Leer datos del Excel
  productos <- read_excel("productos.xlsx")
  caja <- merge(x = caja, y = productos, by.x = 'producto', by.y = 'PRODUCTO', all.x = TRUE)
  
  caja$DESCORTO <- ifelse(is.na(caja$DESCORTO), caja$descripcion, caja$DESCORTO)
  
  ver <- caja %>% filter(is.na(FAMILIA)) %>% group_by(DESCORTO) %>% summarise(Cantidad = n())
  
  caja$FAMILIA <- ifelse(caja$DESCORTO %in% c("EE - ULTRABIOM (UBM)","EE. INLASER OCT CEL GANGLIONARES",
                                              "EE. INLASER OSIRIS"),"REFRACTIVA",
                         ifelse(caja$DESCORTO %in% c("EST. LENTES"), "OPTICA",
                                ifelse(caja$DESCORTO %in% c("EXAMEN ESP OFTALM OCT CELULAS GLANGLIONARES"),"OFTALMOLOGIA",
                                       ifelse(caja$DESCORTO %in% c("")))))
  
  return(caja)
}

update_mysql_login <- function() {
  dbjosue <- dbConnect(MySQL(), user="eksnnfsb_sistemas", host="66.225.201.150", password="k7vn5@IbemUj", dbname="eksnnfsb_patrimonio")
  query <- dbGetQuery(dbjosue, statement = "select name, password from kusers")
  dbDisconnect(dbjosue)
  return(query)
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::    DEVELOPMENT THE APP     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Function to create the dashboard user interface
dashboard_ui <- function() {
  dashboardPage(
    title = "OPTICA",
    header = dashboardHeader(
      title = dashboardBrand(
        title = h4("OPT FUENTE"),
        href = "https://clinicalafuente.com/",
        image = "https://clinicalafuente.com/favicon.png"
      ),
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          notificationItem(
            inputId = "Check1",
            text = "Análisis BD",
            status = "success",
            icon("check")
          )#,
          #notificationItem(
          #  inputId = "Check2",
          #  text = "Análisis programa Catarata",
          #  status = "info",
          #  icon("circle-info")
          #),
          #notificationItem(
          #  inputId = "Check3",
          #  text = "Análisis de cirugías y cotizaciones",
          #  status = "success",
          #  icon("check")
          #)
      ))
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem(
          text = "Información general",
          tabName = "tab1",
          icon = icon("info"),
          selected = TRUE # Selecciona tab1 por defecto
        ),
        menuItem(
          text = "Sistema Óptica",
          icon = icon("glasses"),
          startExpanded = FALSE,  # No está expandido por defecto para permitir el comportamiento de expandir/contraer
          menuSubItem(
            text = "Análisis",
            tabName = "subtab2_1",
            icon = icon("chart-line")
          )#,
          #menuSubItem(
          #  text = "Análisis Individual",
          #  tabName = "subtab2_2",
          #  icon = icon("user")
          #)
        ),
        menuItem(
          text = "Sistema Caja",
          tabName = "tab3", #No se necesita
          icon = icon("cash-register"),
          startExpanded = FALSE,  # No está expandido por defecto para permitir el comportamiento de expandir/contraer
          menuSubItem(
            text = "Análisis General",
            tabName = "subtab2_2",
            icon = icon("chart-line")
          )#,
        #  menuSubItem(
        #    text = "Análisis Individual",
        #    tabName = "subtab3_2",
        #    icon = icon("user")
        #  )
        )#,
        #menuItem(
        #  text = "Análisis ad1",
          #tabName = "tab4", No se necesita
        #  icon = icon("address-book"),
          #startExpanded = FALSE,
          #menuSubItem(
          #  text = "Info",
          #  tabName = "subtab4_1",
          #  icon = icon("info")
          #),
          #menuSubItem(
          #  text = "Análisis",
          #  tabName = "subtab4_2",
          #  icon = icon("chart-line")
          #)
        #)
      )
    ),
    body = dashboardBody(
      tags$style(HTML("
        :root {
      /* Modo Claro */
      --bg-light: white;
      --text-light: black;
      --line-light: #15715d;
      --point-light: #00bcf5;
      --grid-light: #cccccc;

      /* Modo Oscuro */
      --bg-dark: #181818;
      --text-dark: white;
      --line-dark: #0aa1c6;
      --point-dark: #ff9800;
      --grid-dark: #333333;
    }

    body.light-mode {
      --bg: var(--bg-light);
      --text: var(--text-light);
      --line: var(--line-light);
      --point: var(--point-light);
      --grid: var(--grid-light);
    }

    body.dark-mode {
      --bg: var(--bg-dark);
      --text: var(--text-dark);
      --line: var(--line-dark);
      --point: var(--point-dark);
      --grid: var(--grid-dark);
    }
        
        .selectize-input {
        background-color: #f0f0f0 !important; /* Fondo gris para el área de selección */
        color: #333 !important; /* Color de texto */
        }
        .selectize-input .item {
        background-color: #d3d3d3 !important; /* Fondo gris para las opciones seleccionadas */
        color: #000 !important; /* Color de texto de las opciones seleccionadas */
        }
        .selectize-dropdown-content .option:hover {
        background-color: #ccc !important; /* Fondo gris más oscuro al pasar el ratón */
        color: #000 !important; /* Color de texto al pasar el ratón */
        }
        
        .dynamic-title-light {color: #000000 !important;} /* Texto fijo en negro para modo claro */
        .dynamic-title-dark {color: #ffffff !important;} /* Texto fijo en blanco para modo oscuro */
        .pruebas {color: #007bff !important; text-decoration: underline;} /* Azul primario para número de cotizaciones en modo claro */
        .pruebas-dark {color: #C6FBFC !important; text-decoration: underline;} /* Gris oscuro para número de cotizaciones en modo oscuro */
        
        .apto-light {background-color: #d4edda !important; color: black !important;}
        .icl-light {background-color: #fff3cd !important; color: black !important;}
        .no-apto-light {background-color: #faa0f4 !important; color: black !important;}
        .no-revisado-light {background-color: #f8d7da !important; color: black !important;}
        .sin-registrar-light {background-color: #ffffff !important; color: black !important;}
        
        .apto-dark {background-color: #155724 !important; color: white !important;}
        .icl-dark {background-color: #856404 !important; color: white !important;}
        .no-apto-dark {background-color: #76016f !important; color: white !important;}
        .no-revisado-dark {background-color: #721c24 !important; color: white !important;}
        .sin-registrar-dark {background-color: #000000 !important; color: white !important;}
        
        .este-mes-light {background-color: #d4edda !important; color: black !important;}
        .mes-anterior-light {background-color: #fff3cd !important; color: black !important;}
        .no-registrado-light {background-color: #f8d7da !important; color: black !important;}
        
        .este-mes-dark {background-color: #155724 !important; color: white !important;}
        .mes-anterior-dark {background-color: #856404 !important; color: white !important;}
        .no-registrado-dark {background-color: #721c24 !important; color: white !important;}
        
        .social-light {background-color: #fff3cd !important; color: black !important;}
        .regular-light {background-color: #cce5ff !important; color: black !important;}
        .costo-cero-light {background-color: #f8d7da !important; color: black !important;}
        
        .social-dark {background-color: #856404 !important; color: white !important;}
        .regular-dark {background-color: #004085 !important; color: white !important;}
        .costo-cero-dark {background-color: #721c24 !important; color: white !important;}
        
        .completo-light {background-color: #d4edda !important; color: black !important;}
        .falta-light {background-color: #f8d7da !important; color: black !important;}
        .caso-social-light {background-color: #fff3cd !important; color: black !important;}
        
        .completo-dark {background-color: #155724 !important; color: white !important;}
        .falta-dark {background-color: #721c24 !important; color: white !important;}
        .caso-social-dark {background-color: #856404 !important; color: white !important;}
        
        .textdinamic {color: #007bff !important; text-decoration: underline;} /* Azul primario para número de cotizaciones en modo claro */
        .textdinamic-dark {color: #C6FBFC !important; text-decoration: underline;} /* Gris oscuro para número de cotizaciones en modo oscuro */
        
      ")),
      uiOutput("content")
    ),
    footer = bs4DashFooter(
      left = "Desarollado por: Kevin Heberth Haquehua Apaza / Estadístico / Matemático / Ciencia de datos / Análisis de datos / Analista de sistemas",
      right = tagList(
        tags$a(href = "https://www.linkedin.com/in/kevinhaquehua/", target = "_blank", 
               icon("linkedin", lib = "font-awesome"), style = "margin-left: 10px;"),
        tags$a(href = "https://github.com/khaquehua", target = "_blank", 
               icon("github", lib = "font-awesome"), style = "margin-left: 10px;"),
        tags$a(href = "https://wa.link/u69n53", target = "_blank", 
               icon("whatsapp", lib = "font-awesome"), style = "margin-left: 10px;")
      )
    )
  )
}

# Interfaz de usuario principal
ui <- dashboard_ui()
       
add_favicon <- function() {
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "https://clinicalafuente.com/favicon.png")
  )
}

# Server
server <- function(input, output, session) {
  useAutoColor()
  theme_mode <- reactiveVal("light")
  
  data <- reactiveValues(OPT = NULL, LOG = NULL, CAJA = NULL)
  update_data <- function() {
    data$OPT <- update_opt_fuente()
    data$CAJA <- update_caja_fuente()
    #data$RF <- update_google_sheets_RF()
    data$LOG <- update_mysql_login()
  }
  
  update_login <- function() {
    data$LOG <- update_mysql_login()
  }
  
  update_dataOPT <- function() {
    data$OPT <- update_opt_fuente()
  }
  
  update_dataCAJA <- function() {
    data$CAJA <- update_caja_fuente()
  }
  
  #update_dataQX <- function() {
  #  data$QX <- update_google_sheets_QX()
  #}
  
  #update_dataRF <- function() {
  #  data$RF <- update_google_sheets_RF()
  #}
  
  update_login()
  
  user <- reactiveValues(authenticated = FALSE)
  
  #output$sidebar_content <- renderUI({
    
  #})
  
  # Mostrar modal de inicio de sesión al inicio
  showModal(
    modalDialog(
      div(
        class = "modal-header bg-primary text-center",
        tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
        tags$h4("Inicio de sesión", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
      ),
      textInput("user_name", "Username"),
      passwordInput("password", "Password"),
      actionButton("login_button", "Log in"),
      easyClose = FALSE,
      footer = NULL
    )
  )
  
  # Observar el botón de inicio de sesión
  observeEvent(input$login_button, {
    req(input$user_name, input$password)
    
    # Encontrar el hash de la contraseña correspondiente al email
    db_password_hash <- data$LOG$password[data$LOG$name == input$user_name]
    
    if (length(db_password_hash) == 1) {
      db_password_hash <- db_password_hash[1]
      input_password_hash <- digest(input$password, algo = "md5", serialize = FALSE)
      
      if (input_password_hash == db_password_hash) {
        if (input$user_name %in% c("admin","ccruzj","vmazaneth","alfred_c_1@icloud.com",
                                   "Idelmoral")) {
          user$authenticated <- TRUE
          user$name <- data$LOG$name[data$LOG$name == input$user_name]  # Almacenar el nombre de usuario
          
          showModal(
            modalDialog(
              div(
                class = "modal-header bg-success text-center",
                div(
                  style = "background-color: #F1FECC; padding: 5px;",
                  tags$img(src = "https://clinicalafuente.com/favicon.png", height = 30),
                ),
                tags$h4(paste0("Bienvenido ",user$name), style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
              ),
              "Desde ahora puedes empezar a navegar por las pestañas de la parte izquierda",
              easyClose = TRUE,
              footer = actionButton("close_welcome_modal", "Aceptar", class = "btn-success")
            )
          )
          
          removeModal()
        } else {
          showModal(
            modalDialog(
              div(
                class = "modal-header bg-danger text-center",
                div(
                  style = "background-color: #FCDBDA; padding: 5px;",
                  tags$img(src = "https://clinicalafuente.com/favicon.png", height = 30),
                ),
                tags$h4("Acceso Denegado", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
              ),
              "Usted no tiene acceso para el Dashboard pongase en contacto con el área de Administración y/o Estadística",
              easyClose = FALSE,
              footer = actionButton("try_again_button", "Volver a intentar", class = "btn-danger")
            )
          )
        }
      } else {
        showModal(
          modalDialog(
            div(
              class = "modal-header bg-danger text-center",
              div(
                style = "background-color: #FCDBDA; padding: 5px;",
                tags$img(src = "https://clinicalafuente.com/favicon.png", height = 30),
              ),
              tags$h4("Acceso Denegado", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
            ),
            "Nombre de usuario o contraseña incorrecta",
            easyClose = FALSE,
            footer = actionButton("try_again_button", "Volver a intentar", class = "btn-danger")
          )
        )
      }
    } else {
      showModal(
        modalDialog(
          div(
            class = "modal-header bg-danger text-center",
            div(
              style = "background-color: #FCDBDA; padding: 5px;",
              tags$img(src = "https://clinicalafuente.com/favicon.png", height = 30),
            ),
            tags$h4("Acceso Denegado", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
          ),
          "Nombre de usuario o contraseña incorrecta",
          easyClose = FALSE,
          footer = actionButton("try_again_button", "Volver a intentar", class = "btn-danger")
        )
      )
    }
  })
  
  # Observar el botón "Try Again" para volver a mostrar el modal de inicio de sesión
  observeEvent(input$try_again_button, {
    showModal(
      modalDialog(
        div(
          class = "modal-header bg-info text-center",
          tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
          tags$h4("Inicio de sesión", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
        ),
        textInput("user_name", "Username"),
        passwordInput("password", "Password"),
        actionButton("login_button", "Log in"),
        easyClose = FALSE,
        footer = NULL
      )
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::      APARTADO DE NOTIFICACIONES      ::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  observeEvent(input$Check1, {
    showModal(
      modalDialog(
        div(
          class = "modal-header bg-primary text-center",
          tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
          tags$h4("Se actualizó análisis Optica Fuente", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
        ),
        HTML("Actualizado el 27/01/2026<br>
           Ahora puedes visualizar los resultados de los procesos de óptica en la interfaz
           Recuerda llenar la información del sistema para ver los resultados<br>
           <strong>PRUEBALO!!!</strong><br><br>
           Atte: Lic. Kevin Haquehua"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  #observeEvent(input$Check2, {
  #  showModal(
  #    modalDialog(
  #      div(
  #        class = "modal-header bg-primary text-center",
  #        tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
  #        tags$h4("Se actualizó análisis programa Catarata", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
  #      ),
  #      HTML("Actualizado el 04/06/2025<br>
  #         Ahora puedes visualizar el análisis del programa de Catarata
  #         Tanto en la parte analítica general e individual<br>
  #         <strong>PRUEBALO!!!</strong><br><br>
  #         Atte: Kevin Haquehua"),
  #      easyClose = TRUE,
  #      footer = NULL
  #    )
  #  )
  #})
  
  #observeEvent(input$Check3, {
  #  showModal(
  #    modalDialog(
  #      div(
  #        class = "modal-header bg-primary text-center",
  #        tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
  #        tags$h4("Se agregaron estadísticas de cirugías", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
  #      ),
  #      HTML("Actualizado el 17/07/2025<br>
  #         Ahora puedes visualizar el análisis KPI de las cirugías realizadas
  #         Por parte de los pacientes de PDEO y CATARATA<br>
  #         <strong>PRUEBALO!!!</strong><br><br>
  #         Atte: Kevin Haquehua"),
  #      easyClose = TRUE,
  #      footer = NULL
  #    )
  #  )
  #})
  
  #observeEvent(input$Check2, {
  #  showModal(
  #    modalDialog(
  #      div(
  #        class = "modal-header bg-teal text-center",
  #        tags$img(src = "https://clinicalafuente.com/favicon.png", height = 50, width = 50),
  #        tags$h4("Se actualizó referencias", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
  #      ),
  #      HTML("Actualizado el 13/03/2025<br>
  #         Ahora puedes visualizar el análisis de las referencias externas
  #         Tanto en la parte analítica como las tablas de informes elaboradas<br>
  #         <strong>PRUEBALO!!!</strong><br><br>
  #         Atte: Kevin Haquehua"),
  #      easyClose = TRUE,
  #      footer = NULL
  #    )
  #  )
  #})
  
  observeEvent(input$close_welcome_modal, {
    removeModal()
  })
  
  filtered_data_OPT <- reactive({
    
    # Filtrar las fechas
    selected_date_receta <- input$selectDate_Receta
    #selected_date_cotizacion <- input$selectDate_Cotizacion
    #selected_date_orden <- input$selectDate_Orden
    #selected_date_caja <- input$selectDate_Caja
    # Filtrar si tiene
    selected_tiene_cotizacion <- input$selectTiene_Cotizacion
    selected_tiene_orden <- input$selectTiene_Orden
    selected_tiene_caja <- input$selectTiene_Caja
    # Personas (optometras y usuarios)
    selected_optometra <- input$selectOptometra
    selected_usuario_cotizacion <- input$selectUsuario_Cotizacion
    selected_usuario_orden <- input$selectUsuario_Orden
    # Caracteristicas cotizacion
    selected_tipluna <- input$selectTipluna
    selected_tipmateriales <- input$selectTipmateriales
    selected_propiedades <- input$selectPropiedades
    # Caracteristicas orden
    selected_laboratorio <- input$selectLaboratorio
    selected_tipmontura <- input$selectTipmontura
    selected_color <- input$selectColor
    
    filtered <- data$OPT
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_tiene_cotizacion) > 1,
      length(selected_tiene_orden) > 1,
      length(selected_tiene_caja) > 1,
      length(selected_optometra) > 1,
      length(selected_usuario_cotizacion) > 1,
      length(selected_usuario_orden) > 1,
      length(selected_tipluna) > 1,
      length(selected_tipmateriales) > 1,
      length(selected_propiedades) > 1,
      length(selected_laboratorio) > 1,
      length(selected_tipmontura) > 1,
      length(selected_color) > 1,
      !is.null(selected_date_receta)#,
      #!is.null(selected_date_cotizacion),
      #!is.null(selected_date_orden),
      #!is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$OPT
      
      if (!"Todos" %in% selected_tiene_cotizacion) {
        filtered <- filtered[filtered$Tiene_cotizacion %in% selected_tiene_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_tiene_orden) {
        filtered <- filtered[filtered$estatus %in% selected_tiene_orden, ]
      }
      
      if (!"Todos" %in% selected_tiene_caja) {
        filtered <- filtered[filtered$Tiene_pago %in% selected_tiene_caja, ]
      }
      
      if (!"Todos" %in% selected_optometra) {
        filtered <- filtered[filtered$optometra %in% selected_optometra, ]
      }
      
      if (!"Todos" %in% selected_usuario_cotizacion) {
        filtered <- filtered[filtered$usuario_cotizacion %in% selected_usuario_cotizacion, ]
      }
  
      if (!"Todos" %in% selected_usuario_orden) {
        filtered <- filtered[filtered$usuario_orden %in% selected_usuario_orden, ]
      }
      
      if (!"Todos" %in% selected_tipluna) {
        filtered <- filtered[filtered$tipluna %in% selected_tipluna, ]
      }
      
      if (!"Todos" %in% selected_tipmateriales) {
        filtered <- filtered[filtered$tipmateriales %in% selected_tipmateriales, ]
      }
      
      if (!"Todos" %in% selected_propiedades) {
        filtered <- filtered[filtered$propiedades %in% selected_propiedades, ]
      }
      
      if (!"Todos" %in% selected_laboratorio) {
        filtered <- filtered[filtered$laboratorio %in% selected_laboratorio, ]
      }
      
      if (!"Todos" %in% selected_tipmontura) {
        filtered <- filtered[filtered$tipo_montura %in% selected_tipmontura, ]
      }
      
      if (!"Todos" %in% selected_color) {
        filtered <- filtered[filtered$color %in% selected_color, ]
      }
      
      if (!is.null(selected_date_receta)) {
        filtered <- filtered[filtered$reg_receta >= selected_date_receta[1] & filtered$reg_receta <= selected_date_receta[2], ]
      }
      
      #if (!is.null(selected_date_cotizacion)) {
      #  filtered <- filtered[filtered$reg_cotizacion >= selected_date_cotizacion[1] & filtered$reg_cotizacion <= selected_date_cotizacion[2], ]
      #}
      
      #if (!is.null(selected_date_orden)) {
      #  filtered <- filtered[filtered$fecha_op >= selected_date_orden[1] & filtered$fecha_op <= selected_date_orden[2], ]
      #}
      
      #if (!is.null(selected_date_caja)) {
      #  filtered <- filtered[filtered$fecha_pago_caja >= selected_date_caja[1] & filtered$fecha_pago_caja <= selected_date_caja[2], ]
      #}
      
    } else {
      filtered <- data$OPT
    }
    
    return(filtered)
  })
  
  filtered_data_OPT2 <- reactive({
    
    # Filtrar las fechas
    selected_date_receta <- input$selectDate_Receta
    selected_date_cotizacion <- input$selectDate_Cotizacion
    #selected_date_orden <- input$selectDate_Orden
    #selected_date_caja <- input$selectDate_Caja
    # Filtrar si tiene
    selected_tiene_cotizacion <- input$selectTiene_Cotizacion
    selected_tiene_orden <- input$selectTiene_Orden
    selected_tiene_caja <- input$selectTiene_Caja
    # Personas (optometras y usuarios)
    selected_optometra <- input$selectOptometra
    selected_usuario_cotizacion <- input$selectUsuario_Cotizacion
    selected_usuario_orden <- input$selectUsuario_Orden
    # Caracteristicas cotizacion
    selected_tipluna <- input$selectTipluna
    selected_tipmateriales <- input$selectTipmateriales
    selected_propiedades <- input$selectPropiedades
    # Caracteristicas orden
    selected_laboratorio <- input$selectLaboratorio
    selected_tipmontura <- input$selectTipmontura
    selected_color <- input$selectColor
    
    filtered <- data$OPT
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_tiene_cotizacion) > 1,
      length(selected_tiene_orden) > 1,
      length(selected_tiene_caja) > 1,
      length(selected_optometra) > 1,
      length(selected_usuario_cotizacion) > 1,
      length(selected_usuario_orden) > 1,
      length(selected_tipluna) > 1,
      length(selected_tipmateriales) > 1,
      length(selected_propiedades) > 1,
      length(selected_laboratorio) > 1,
      length(selected_tipmontura) > 1,
      length(selected_color) > 1,
      !is.null(selected_date_receta),
      !is.null(selected_date_cotizacion)#,
      #!is.null(selected_date_orden),
      #!is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$OPT
      
      if (!"Todos" %in% selected_tiene_cotizacion) {
        filtered <- filtered[filtered$Tiene_cotizacion %in% selected_tiene_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_tiene_orden) {
        filtered <- filtered[filtered$estatus %in% selected_tiene_orden, ]
      }
      
      if (!"Todos" %in% selected_tiene_caja) {
        filtered <- filtered[filtered$Tiene_pago %in% selected_tiene_caja, ]
      }
      
      if (!"Todos" %in% selected_optometra) {
        filtered <- filtered[filtered$optometra %in% selected_optometra, ]
      }
      
      if (!"Todos" %in% selected_usuario_cotizacion) {
        filtered <- filtered[filtered$usuario_cotizacion %in% selected_usuario_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_usuario_orden) {
        filtered <- filtered[filtered$usuario_orden %in% selected_usuario_orden, ]
      }
      
      if (!"Todos" %in% selected_tipluna) {
        filtered <- filtered[filtered$tipluna %in% selected_tipluna, ]
      }
      
      if (!"Todos" %in% selected_tipmateriales) {
        filtered <- filtered[filtered$tipmateriales %in% selected_tipmateriales, ]
      }
      
      if (!"Todos" %in% selected_propiedades) {
        filtered <- filtered[filtered$propiedades %in% selected_propiedades, ]
      }
      
      if (!"Todos" %in% selected_laboratorio) {
        filtered <- filtered[filtered$laboratorio %in% selected_laboratorio, ]
      }
      
      if (!"Todos" %in% selected_tipmontura) {
        filtered <- filtered[filtered$tipo_montura %in% selected_tipmontura, ]
      }
      
      if (!"Todos" %in% selected_color) {
        filtered <- filtered[filtered$color %in% selected_color, ]
      }
      
      if (!is.null(selected_date_receta)) {
        filtered <- filtered[filtered$reg_receta >= selected_date_receta[1] & filtered$reg_receta <= selected_date_receta[2], ]
      }
      
      if (!is.null(selected_date_cotizacion)) {
        filtered <- filtered[filtered$reg_cotizacion >= selected_date_cotizacion[1] & filtered$reg_cotizacion <= selected_date_cotizacion[2], ]
      }
      
      #if (!is.null(selected_date_orden)) {
      #  filtered <- filtered[filtered$fecha_op >= selected_date_orden[1] & filtered$fecha_op <= selected_date_orden[2], ]
      #}
      
      #if (!is.null(selected_date_caja)) {
      #  filtered <- filtered[filtered$fecha_pago_caja >= selected_date_caja[1] & filtered$fecha_pago_caja <= selected_date_caja[2], ]
      #}
      
    } else {
      filtered <- data$OPT
    }
    
    return(filtered)
  })
  
  filtered_data_OPT3 <- reactive({
    
    # Filtrar las fechas
    selected_date_receta <- input$selectDate_Receta
    selected_date_cotizacion <- input$selectDate_Cotizacion
    selected_date_orden <- input$selectDate_Orden
    #selected_date_caja <- input$selectDate_Caja
    # Filtrar si tiene
    selected_tiene_cotizacion <- input$selectTiene_Cotizacion
    selected_tiene_orden <- input$selectTiene_Orden
    selected_tiene_caja <- input$selectTiene_Caja
    # Personas (optometras y usuarios)
    selected_optometra <- input$selectOptometra
    selected_usuario_cotizacion <- input$selectUsuario_Cotizacion
    selected_usuario_orden <- input$selectUsuario_Orden
    # Caracteristicas cotizacion
    selected_tipluna <- input$selectTipluna
    selected_tipmateriales <- input$selectTipmateriales
    selected_propiedades <- input$selectPropiedades
    # Caracteristicas orden
    selected_laboratorio <- input$selectLaboratorio
    selected_tipmontura <- input$selectTipmontura
    selected_color <- input$selectColor
    
    filtered <- data$OPT
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_tiene_cotizacion) > 1,
      length(selected_tiene_orden) > 1,
      length(selected_tiene_caja) > 1,
      length(selected_optometra) > 1,
      length(selected_usuario_cotizacion) > 1,
      length(selected_usuario_orden) > 1,
      length(selected_tipluna) > 1,
      length(selected_tipmateriales) > 1,
      length(selected_propiedades) > 1,
      length(selected_laboratorio) > 1,
      length(selected_tipmontura) > 1,
      length(selected_color) > 1,
      !is.null(selected_date_receta),
      !is.null(selected_date_cotizacion),
      !is.null(selected_date_orden)#,
      #!is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$OPT
      
      if (!"Todos" %in% selected_tiene_cotizacion) {
        filtered <- filtered[filtered$Tiene_cotizacion %in% selected_tiene_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_tiene_orden) {
        filtered <- filtered[filtered$estatus %in% selected_tiene_orden, ]
      }
      
      if (!"Todos" %in% selected_tiene_caja) {
        filtered <- filtered[filtered$Tiene_pago %in% selected_tiene_caja, ]
      }
      
      if (!"Todos" %in% selected_optometra) {
        filtered <- filtered[filtered$optometra %in% selected_optometra, ]
      }
      
      if (!"Todos" %in% selected_usuario_cotizacion) {
        filtered <- filtered[filtered$usuario_cotizacion %in% selected_usuario_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_usuario_orden) {
        filtered <- filtered[filtered$usuario_orden %in% selected_usuario_orden, ]
      }
      
      if (!"Todos" %in% selected_tipluna) {
        filtered <- filtered[filtered$tipluna %in% selected_tipluna, ]
      }
      
      if (!"Todos" %in% selected_tipmateriales) {
        filtered <- filtered[filtered$tipmateriales %in% selected_tipmateriales, ]
      }
      
      if (!"Todos" %in% selected_propiedades) {
        filtered <- filtered[filtered$propiedades %in% selected_propiedades, ]
      }
      
      if (!"Todos" %in% selected_laboratorio) {
        filtered <- filtered[filtered$laboratorio %in% selected_laboratorio, ]
      }
      
      if (!"Todos" %in% selected_tipmontura) {
        filtered <- filtered[filtered$tipo_montura %in% selected_tipmontura, ]
      }
      
      if (!"Todos" %in% selected_color) {
        filtered <- filtered[filtered$color %in% selected_color, ]
      }
      
      if (!is.null(selected_date_receta)) {
        filtered <- filtered[filtered$reg_receta >= selected_date_receta[1] & filtered$reg_receta <= selected_date_receta[2], ]
      }
      
      if (!is.null(selected_date_cotizacion)) {
        filtered <- filtered[filtered$reg_cotizacion >= selected_date_cotizacion[1] & filtered$reg_cotizacion <= selected_date_cotizacion[2], ]
      }
      
      if (!is.null(selected_date_orden)) {
        filtered <- filtered[filtered$fecha_op >= selected_date_orden[1] & filtered$fecha_op <= selected_date_orden[2], ]
      }
      
      #if (!is.null(selected_date_caja)) {
      #  filtered <- filtered[filtered$fecha_pago_caja >= selected_date_caja[1] & filtered$fecha_pago_caja <= selected_date_caja[2], ]
      #}
      
    } else {
      filtered <- data$OPT
    }
    
    return(filtered)
  })
  
  filtered_data_OPT4 <- reactive({
    
    # Filtrar las fechas
    selected_date_receta <- input$selectDate_Receta
    selected_date_cotizacion <- input$selectDate_Cotizacion
    selected_date_orden <- input$selectDate_Orden
    selected_date_caja <- input$selectDate_Caja
    # Filtrar si tiene
    selected_tiene_cotizacion <- input$selectTiene_Cotizacion
    selected_tiene_orden <- input$selectTiene_Orden
    selected_tiene_caja <- input$selectTiene_Caja
    # Personas (optometras y usuarios)
    selected_optometra <- input$selectOptometra
    selected_usuario_cotizacion <- input$selectUsuario_Cotizacion
    selected_usuario_orden <- input$selectUsuario_Orden
    # Caracteristicas cotizacion
    selected_tipluna <- input$selectTipluna
    selected_tipmateriales <- input$selectTipmateriales
    selected_propiedades <- input$selectPropiedades
    # Caracteristicas orden
    selected_laboratorio <- input$selectLaboratorio
    selected_tipmontura <- input$selectTipmontura
    selected_color <- input$selectColor
    
    filtered <- data$OPT
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_tiene_cotizacion) > 1,
      length(selected_tiene_orden) > 1,
      length(selected_tiene_caja) > 1,
      length(selected_optometra) > 1,
      length(selected_usuario_cotizacion) > 1,
      length(selected_usuario_orden) > 1,
      length(selected_tipluna) > 1,
      length(selected_tipmateriales) > 1,
      length(selected_propiedades) > 1,
      length(selected_laboratorio) > 1,
      length(selected_tipmontura) > 1,
      length(selected_color) > 1,
      !is.null(selected_date_receta),
      !is.null(selected_date_cotizacion),
      !is.null(selected_date_orden),
      !is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$OPT
      
      if (!"Todos" %in% selected_tiene_cotizacion) {
        filtered <- filtered[filtered$Tiene_cotizacion %in% selected_tiene_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_tiene_orden) {
        filtered <- filtered[filtered$estatus %in% selected_tiene_orden, ]
      }
      
      if (!"Todos" %in% selected_tiene_caja) {
        filtered <- filtered[filtered$Tiene_pago %in% selected_tiene_caja, ]
      }
      
      if (!"Todos" %in% selected_optometra) {
        filtered <- filtered[filtered$optometra %in% selected_optometra, ]
      }
      
      if (!"Todos" %in% selected_usuario_cotizacion) {
        filtered <- filtered[filtered$usuario_cotizacion %in% selected_usuario_cotizacion, ]
      }
      
      if (!"Todos" %in% selected_usuario_orden) {
        filtered <- filtered[filtered$usuario_orden %in% selected_usuario_orden, ]
      }
      
      if (!"Todos" %in% selected_tipluna) {
        filtered <- filtered[filtered$tipluna %in% selected_tipluna, ]
      }
      
      if (!"Todos" %in% selected_tipmateriales) {
        filtered <- filtered[filtered$tipmateriales %in% selected_tipmateriales, ]
      }
      
      if (!"Todos" %in% selected_propiedades) {
        filtered <- filtered[filtered$propiedades %in% selected_propiedades, ]
      }
      
      if (!"Todos" %in% selected_laboratorio) {
        filtered <- filtered[filtered$laboratorio %in% selected_laboratorio, ]
      }
      
      if (!"Todos" %in% selected_tipmontura) {
        filtered <- filtered[filtered$tipo_montura %in% selected_tipmontura, ]
      }
      
      if (!"Todos" %in% selected_color) {
        filtered <- filtered[filtered$color %in% selected_color, ]
      }
      
      if (!is.null(selected_date_receta)) {
        filtered <- filtered[filtered$reg_receta >= selected_date_receta[1] & filtered$reg_receta <= selected_date_receta[2], ]
      }
      
      if (!is.null(selected_date_cotizacion)) {
        filtered <- filtered[filtered$reg_cotizacion >= selected_date_cotizacion[1] & filtered$reg_cotizacion <= selected_date_cotizacion[2], ]
      }
      
      if (!is.null(selected_date_orden)) {
        filtered <- filtered[filtered$fecha_op >= selected_date_orden[1] & filtered$fecha_op <= selected_date_orden[2], ]
      }
      
      if (!is.null(selected_date_caja)) {
        filtered <- filtered[filtered$fecha_pago_caja >= selected_date_caja[1] & filtered$fecha_pago_caja <= selected_date_caja[2], ]
      }
      
    } else {
      filtered <- data$OPT
    }
    
    return(filtered)
  })
  
  filtered_data_CAJA <- reactive({
    
    # Filtrar la fecha
    selected_caja <- input$selectDate_Caja_caja
    # Filtrar descripcion
    selected_descripcion <- input$selectDescripcion
    # Filtrar nombre
    selected_nombre <- input$selectNombre
    
    filtered <- data$CAJA
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_descripcion) > 1,
      length(selected_nombre) > 1,
      !is.null(selected_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$CAJA
      
      if (!"Todos" %in% selected_descripcion) {
        filtered <- filtered[filtered$descripcion %in% selected_descripcion, ]
      }
      
      if (!"Todos" %in% selected_nombre) {
        filtered <- filtered[filtered$nombre %in% selected_nombre, ]
      }
      
      if (!is.null(selected_caja)) {
        filtered <- filtered[filtered$fecha >= selected_caja[1] & filtered$fecha <= selected_caja[2], ]
      }
      
    } else {
      filtered <- data$CAJA
    }
    
    return(filtered)
  })
  
  observeEvent(input$updateButtonOPT, {
    showModal(modalDialog(
      title = "Actualizando datos Optica",
      "Por favor espere mientras se actualizan los datos.",
      easyClose = FALSE,
      footer = NULL
    ))
    
    update_dataOPT()
    
    removeModal()
  })
  
  observeEvent(input$updateButtonCAJA, {
    showModal(modalDialog(
      title = "Actualizando datos Relación operados",
      "Por favor espere mientras se actualizan los datos.",
      easyClose = FALSE,
      footer = NULL
    ))
    
    update_dataCAJA()
    
    removeModal()
  })
  
  output$content <- renderUI({
    req(data$LOG)#, data$PR, data$RF, data$QX)
    if (user$authenticated) {
      
      update_dataOPT()
      req(data$OPT)
      
      update_dataCAJA()
      req(data$CAJA)
      
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(
              width = 12,
              h1("Análisis y control Óptica"),
              h3("Objetivo del Proyecto"),
              p("El objetivo del presente dashboard es"),
              tags$ul(
                tags$li("Apoyar al control de operaciones en base a la información que se tiene mediante las diferentes bases de datos: Sistema Fuente y Caja"),
                tags$li("Mostrar los estadísticos descriptivos así como los KPIs principales en cada función"),
                tags$li("Realizar el reporte de los indicadores principales"),
                tags$li("Indicar el uso y una pequeña guía de tal forma que el Dashboard se utilice de la manera más óptima posible")
              ),
              h3("Partes del Dashboard"),
              p("A continuación veamos a detalle las partes principales que contiene el Dashboard para su exploración"),
              tags$ul(
                tags$li("En la parte izquierda se muestran las diferentes opciones que se tienen para Análisis de Datos de cada área"),
                br(),
                tags$img(src = "https://inlaser.net/dashboard-img/4.jpg", width = "1200px", height = "600px"),
                br(),
                br(),
                tags$li("Puedes desplegar cada pestaña, adicionalmente siempre cada parte tendra su opción 'Info' el cual brinda información del item"),
                tags$img(src = "https://inlaser.net/dashboard-img/3.jpg", width = "1200px", height = "600px"),
                br(),
                br(),
                tags$li("En la parte superior tienes el modo oscuro que ayuda si tienes problema para la visión y en la parte inferior la información acerca del Autor"),
                tags$img(src = "https://inlaser.net/dashboard-img/2.jpg", width = "1200px", height = "600px"),
              ),
              br(),
              br(),
              h2("Entonces comencemos")
            )
          )
        ),
        tabItem(
          tabName = "subtab2_1",
          h1("Registro de procedimientos Óptica"),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Filtros"),
            fluidRow(
              column(3, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha Receta"
                ),
                dateRangeInput("selectDate_Receta", label = NULL, start = min(data$OPT$reg_receta, na.rm = TRUE), 
                               end = max(data$OPT$reg_receta, na.rm = TRUE))
              )),
              column(3, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha Cotización"
                ),
                dateRangeInput("selectDate_Cotizacion", label = NULL, start = min(data$OPT$reg_cotizacion, na.rm = TRUE), 
                               end = max(data$OPT$reg_cotizacion, na.rm = TRUE))
              )),
              column(3, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha Orden de Compra"
                ),
                dateRangeInput("selectDate_Orden", label = NULL, start = min(data$OPT$fecha_op, na.rm = TRUE), 
                               end = max(data$OPT$fecha_op, na.rm = TRUE))
              )),
              column(3, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha Pago Caja"
                ),
                dateRangeInput("selectDate_Caja", label = NULL, start = min(data$OPT$fecha_pago_caja, na.rm = TRUE), 
                               end = max(data$OPT$fecha_pago_caja, na.rm = TRUE))
              ))
            ),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hand-holding-hand", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Tiene cotización"
                ),
                selectInput("selectTiene_Cotizacion", label = NULL, choices = c("Todos", names(table(data$OPT$Tiene_cotizacion))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("handshake", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Tiene orden"
                ),
                selectInput("selectTiene_Orden", label = NULL, choices = c("Todos", names(table(data$OPT$estatus))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hand-holding-dollar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Tiene pago"
                ),
                selectInput("selectTiene_Caja", label = NULL, choices = c("Todos", names(table(data$OPT$Tiene_pago))), selected = "Todos", multiple = TRUE)
              ))
            ),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("user-doctor", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Optometra"
                ),
                selectInput("selectOptometra", label = NULL, choices = c("Todos", names(table(data$OPT$optometra))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("address-book", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Usuario cotización"
                ),
                selectInput("selectUsuario_Cotizacion", label = NULL, choices = c("Todos", names(table(data$OPT$usuario_cotizacion))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("id-badge", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Usuario orden"
                ),
                selectInput("selectUsuario_Orden", label = NULL, choices = c("Todos", names(table(data$OPT$usuario_orden))), selected = "Todos", multiple = TRUE)
              ))
            ),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("moon", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Luna"
                ),
                selectInput("selectTipluna", label = NULL, choices = c("Todos", unique(data$OPT$tipluna)), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("toolbox", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Materiales"
                ),
                selectInput("selectTipmateriales", label = NULL, choices = c("Todos", unique(data$OPT$tipmateriales)), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hands-holding-circle", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Propiedades"
                ),
                selectInput("selectPropiedades", label = NULL, choices = c("Todos", unique(data$OPT$propiedades)), selected = "Todos", multiple = TRUE)
              ))
            ),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("flask-vial", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Laboratorio"
                ),
                selectInput("selectLaboratorio", label = NULL, choices = c("Todos", unique(data$OPT$laboratorio)), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("glasses", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Montura"
                ),
                selectInput("selectTipmontura", label = NULL, choices = c("Todos", unique(data$OPT$tipo_montura)), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("palette", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Color"
                ),
                selectInput("selectColor", label = NULL, choices = c("Todos", unique(data$OPT$color)), selected = "Todos", multiple = TRUE)
              ))
            )
          ),
          fluidRow(
            column(4,
                   div(
                     actionButton("updateButtonOPT", "Actualizar datos", class = "btn btn-primary")#,
                     #id = "updateButton-container"
                   )
            )
          ),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Resultados Generales"),
            br(),
            tabsetPanel(
              id = "tabsetGenerales",
              tabPanel(
                title = "General",
                icon = icon("users"),
                #fluidRow(
                #  column(12, uiOutput("dynamicTitleRegisterCirugias"))
                #),
                fluidRow(
                  
                    column(3, infoBoxOutput("Recetas_realizadas", width = 12)),
                    column(3, infoBoxOutput("Cotizaciones_realizadas", width = 12)),
                    column(3, infoBoxOutput("Ordenes_realizadas", width = 12)),
                    column(3, infoBoxOutput("Pagos_realizados", width = 12))
                  ,
                  fluidRow(
                    column(8, 
                           tabBox(
                             title = "Evolución procesos",
                             selected = "Recetas (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Recetas (G)",
                               width = 12,
                               plotOutput("plotts_recetas", height = "600px")
                             ),
                             tabPanel(
                               title = "Cotizaciones (G)",
                               width = 12,
                               plotOutput("plotts_cotizaciones", height = "600px")
                             ),
                             tabPanel(
                               title = "Ordenes (G)",
                               width = 12,
                               plotOutput("plotts_ordenes", height = "600px")
                             ),
                             tabPanel(
                               title = "Pagos (G)",
                               width = 12,
                               plotOutput("plotts_pagos", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla resumen",
                               width = 12,
                               downloadButton("downloadExceltabla_ts", "Descargar Excel"),
                               dataTableOutput('tabla_ts')
                             ),
                             tabPanel(
                               title = "Tabla recetas",
                               width = 12,
                               downloadButton("downloadExceltabla_recetas", "Descargar Excel"),
                               dataTableOutput('tabla_recetas')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Optometra",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plotoptometra", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_optometra", "Descargar Excel"),
                               dataTableOutput('tab_optometra')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                ),
                div(
                  fluidRow(
                    column(4, 
                           tabBox(
                             title = "Cotizaciones",
                             selected = "Luna (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Luna (G)",
                               width = 12,
                               plotOutput("plot_luna", height = "600px")
                             ),
                             tabPanel(
                               title = "Material (G)",
                               width = 12,
                               plotOutput("plot_material", height = "600px")
                             ),
                             tabPanel(
                               title = "Propiedad (G)",
                               width = 12,
                               plotOutput("plot_propiedad", height = "600px")
                             ),
                             tabPanel(
                               title = "Luna (T)",
                               width = 12,
                               downloadButton("downloadExcel_luna", "Descargar Excel"),
                               dataTableOutput('tab_luna')
                             ),
                             tabPanel(
                               title = "Material (T)",
                               width = 12,
                               downloadButton("downloadExcel_material", "Descargar Excel"),
                               dataTableOutput('tab_material')
                             ),
                             tabPanel(
                               title = "Propiedad (T)",
                               width = 12,
                               downloadButton("downloadExcel_propiedad", "Descargar Excel"),
                               dataTableOutput('tab_propiedad')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Orden",
                             selected = "Laboratorio (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Laboratorio (G)",
                               width = 12,
                               plotOutput("plot_laboratorio", height = "600px")
                             ),
                             tabPanel(
                               title = "Montura (G)",
                               width = 12,
                               plotOutput("plot_montura", height = "600px")
                             ),
                             tabPanel(
                               title = "Color (G)",
                               width = 12,
                               plotOutput("plot_color", height = "600px")
                             ),
                             tabPanel(
                               title = "Laboratorio (T)",
                               width = 12,
                               downloadButton("downloadExcel_laboratorio", "Descargar Excel"),
                               dataTableOutput('tab_laboratorio')
                             ),
                             tabPanel(
                               title = "Montura (T)",
                               width = 12,
                               downloadButton("downloadExcel_montura", "Descargar Excel"),
                               dataTableOutput('tab_montura')
                             ),
                             tabPanel(
                               title = "Color (T)",
                               width = 12,
                               downloadButton("downloadExcel_color", "Descargar Excel"),
                               dataTableOutput('tab_color')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Pago / Días",
                             selected = "Pagos",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Pagos",
                               width = 12,
                               downloadButton("downloadExcel_pagos", "Descargar Excel"),
                               dataTableOutput('tab_pagos')
                             ),
                             tabPanel(
                               title = "Días",
                               width = 12,
                               downloadButton("downloadExcel_dias", "Descargar Excel"),
                               dataTableOutput('tab_dias')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                )
              ),
              tabPanel(
                title = "Resultados economicos",
                icon = icon("money-bill"),
                fluidRow(
                  
                  column(4, infoBoxOutput("Monto_sis_caja", width = 12)),
                  column(4, infoBoxOutput("Pago_sis_caja", width = 12)),
                  column(4, infoBoxOutput("Debe_sis_caja", width = 12))
                  ,
                  fluidRow(
                    column(4, 
                           tabBox(
                             title = "Luna",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_luna_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_luna_mont", "Descargar Excel"),
                               dataTableOutput('tabla_luna_mont')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Material",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_material_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_material_mont", "Descargar Excel"),
                               dataTableOutput('tabla_material_mont')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Propiedades",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_propiedades_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_propiedades_mont", "Descargar Excel"),
                               dataTableOutput('tabla_propiedades_mont')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                ),
                div(
                  fluidRow(
                    column(4, 
                           tabBox(
                             title = "Montura",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_montura_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_montura_mont", "Descargar Excel"),
                               dataTableOutput('tabla_montura_mont')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Laboratorio",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_laboratorio_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_laboratorio_mont", "Descargar Excel"),
                               dataTableOutput('tabla_laboratorio_mont')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Color",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plottipo_color_mont", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_color_mont", "Descargar Excel"),
                               dataTableOutput('tabla_color_mont')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                )
              ),
              tabPanel(
                title = "Lista pacientes",
                icon = icon("user"),
                fluidRow(
                  column(12, 
                         box(
                           title = "Lista pacientes",
                           status = "primary",
                           solidHeader = TRUE,
                           maximizable = TRUE,
                           width = 12,
                           downloadButton("downloadExcelpacientes", "Descargar Excel"),
                           DT::dataTableOutput('tabla_pacientes')
                         )
                  )
                )
              )
            ),
          style = "margin-top: 20px;"
        )
        ),
        tabItem(
          tabName = "subtab2_2",
          h1("Registro de procedimientos Caja"),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Filtros"),
            fluidRow(
              column(6, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha Movimiento"
                ),
                dateRangeInput("selectDate_Caja_caja", label = NULL, start = min(data$CAJA$fecha, na.rm = TRUE), 
                               end = max(data$CAJA$fecha, na.rm = TRUE))
              )),
              column(6, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("toolbox", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Seleccione Descripción"
                ),
                selectInput("selectDescripcion", label = NULL, choices = c("Todos", unique(data$CAJA$descripcion)), selected = "Todos", multiple = TRUE)
              ))
            ),
            fluidRow(
              column(12, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Cliente"
                ),
                selectInput("selectNombre", label = NULL, choices = c("Todos", names(table(data$CAJA$nombre))), selected = "Todos", multiple = TRUE)
              ))
            )
          ),
          fluidRow(
            column(4,
                   div(
                     actionButton("updateButtonCAJA", "Actualizar datos", class = "btn btn-primary")#,
                     #id = "updateButton-container"
                   )
            )
          ),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Resultados Generales"),
            br(),
            tabsetPanel(
              id = "tabsetGenerales",
              tabPanel(
                title = "Resultados",
                icon = icon("users"),
                fluidRow(
                  column(12, uiOutput("dynamicTitleRegisterActualizar"))
                ),
                div(
                fluidRow(
                  
                  column(6, infoBoxOutput("Boletas_realizadas", width = 12)),
                  column(6, infoBoxOutput("Recaudacion_caja", width = 12))
                ),
                div(
                  fluidRow(
                    column(8, 
                           tabBox(
                             title = "Evolución costos",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plotts_caja_costos", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExceltabla_tscaja_costos", "Descargar Excel"),
                               dataTableOutput('tabla_tscaja_costos')
                             )
                           )),
                    column(4, 
                           tabBox(
                             title = "Descripcion",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plotdescripcion_caja", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_descripcion_caja", "Descargar Excel"),
                               dataTableOutput('tab_descripcion_caja')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                )
                )
                ),
              tabPanel(
                title = "Lista pacientes",
                icon = icon("user"),
                fluidRow(
                  column(12, 
                         box(
                           title = "Lista pacientes",
                           status = "primary",
                           solidHeader = TRUE,
                           maximizable = TRUE,
                           width = 12,
                           downloadButton("downloadExcelpacientes_caja", "Descargar Excel"),
                           DT::dataTableOutput('tabla_pacientes_caja')
                         )
                  )
                )
              )
              )
            ),
            style = "margin-top: 20px;"
          )
        )
      
    } else {
      fluidRow(
        box(
          title = "Acceso Denegado!!!",
          "Por favor vuelva a iniciar"
        )
      )
    }
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::    EMPEZANDO CON LOS DATOS GENERALES      :::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$Recetas_realizadas <- renderInfoBox({
    
    filtered <- filtered_data_OPT()
    
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    cant_receta <- nrow(recetas)
    
    infoBox(
      paste0("Recetas: "),
      cant_receta,
      paste0("realizadas"),
      icon = icon("address-book"),
      color = "info",
      width = NULL
    )
  })
  
  output$Cotizaciones_realizadas <- renderInfoBox({
    
    filtered <- filtered_data_OPT()
    
    cotizaciones <- filtered %>% distinct(nro_cotizacion, .keep_all = TRUE)
    cant_cotizaciones <- nrow(cotizaciones)
    
    infoBox(
      paste0("Cotizaciones: "),
      cant_cotizaciones,
      paste0("realizadas"),
      icon = icon("address-book"),
      color = "info",
      width = NULL
    )
    
  })
  
  output$Ordenes_realizadas <- renderInfoBox({
    
    filtered <- filtered_data_OPT()
    
    ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
    cant_ordenes <- nrow(ordenes)
    
    infoBox(
      paste0("Ordenes: "),
      cant_ordenes,
      paste0("realizadas"),
      icon = icon("address-card"),
      color = "info",
      width = NULL
    )
  })
  
  output$Pagos_realizados <- renderInfoBox({
    
    filtered <- filtered_data_OPT()
    
    ord_pag <- filtered %>% group_by(nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag$Debe <- ord_pag$total - ord_pag$Pagado
    
    ord_pag <- na.omit(ord_pag)
    
    total <- sum(ord_pag$total)
    pagos <- sum(ord_pag$Pagado)
    debe <- sum(ord_pag$Debe)
    
    infoBox(
      paste0("Total: ",scales::dollar(total, prefix = "S/. ")), 
      paste0("Recaudación: ",scales::dollar(pagos, prefix = "S/. ")), 
      paste0("Se debe: ",scales::dollar(debe, prefix = "S/. ")),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "success",
      width = NULL
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::     REALIZAR GRAFICOS TIME SERIES   :::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plotts_recetas <- renderPlot({
    
    filtered <- filtered_data_OPT()
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    
    is_dark_mode <- input$dark_mode
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(recetas) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    recetas_por_mes <- recetas %>%
      group_by(Mes = format(reg_receta, "%Y-%m")) %>%
      summarise(Cantidad = n())
    
    recetas_por_mes$Mes <- as.Date(paste0(recetas_por_mes$Mes, "-01"))
    recetas_por_mes$Año <- format(recetas_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(recetas_por_mes$Año)) != 0)
    
    max_cantidad <- max(recetas_por_mes$Cantidad, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(recetas_por_mes, aes(x = Mes, y = Cantidad)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Cantidad", title = "Recetas realizadas por mes") +
      geom_vline(
        xintercept = as.numeric(recetas_por_mes$Mes[cambios_de_año]),
        color = "#00bcf5",  
        linetype = "dashed",  
        size = 1  
      ) +
      theme(
        plot.background = element_rect(fill = bg_color),  
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "#0aa1c6"),
        axis.title.y = element_text(face = "bold", size = 15, color = text_color),
        axis.title.x = element_text(face = "bold", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, face = "bold", color = text_color),
        axis.text.y = element_text(face = "bold", size = 10, color = text_color)
      ) +
      geom_label(
        data = recetas_por_mes,
        aes(label = paste0(Cantidad)),
        size = 4,
        label.padding = unit(0.5, "lines"),
        label.r = unit(0.15, "lines"),
        fontface = "bold",
        color = text_color,
        fill = bg_color
      ) +
      scale_x_date(
        date_labels = "%Y-%m",
        date_breaks = "1 month"
      ) +
      scale_y_continuous(
        breaks = seq(0, max_redondeado, by = 20),
        labels = seq(0, max_redondeado, by = 20)
      )
  })
  
  output$plotts_cotizaciones <- renderPlot({
    
    filtered <- filtered_data_OPT()
    cotizaciones <- filtered %>% distinct(nro_cotizacion, .keep_all = TRUE)
    
    is_dark_mode <- input$dark_mode
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(cotizaciones) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    cotizaciones_por_mes <- cotizaciones %>%
      group_by(Mes = format(reg_cotizacion, "%Y-%m")) %>%
      summarise(Cantidad = n())
    
    cotizaciones_por_mes$Mes <- as.Date(paste0(cotizaciones_por_mes$Mes, "-01"))
    cotizaciones_por_mes$Año <- format(cotizaciones_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(cotizaciones_por_mes$Año)) != 0)
    
    max_cantidad <- max(cotizaciones_por_mes$Cantidad, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(cotizaciones_por_mes, aes(x = Mes, y = Cantidad)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Cantidad", title = "Cotizaciones realizadas por mes") +
      geom_vline(
        xintercept = as.numeric(cotizaciones_por_mes$Mes[cambios_de_año]),
        color = "#00bcf5",  
        linetype = "dashed",  
        size = 1  
      ) +
      theme(
        plot.background = element_rect(fill = bg_color),  
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "#0aa1c6"),
        axis.title.y = element_text(face = "bold", size = 15, color = text_color),
        axis.title.x = element_text(face = "bold", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, face = "bold", color = text_color),
        axis.text.y = element_text(face = "bold", size = 10, color = text_color)
      ) +
      geom_label(
        data = cotizaciones_por_mes,
        aes(label = paste0(Cantidad)),
        size = 4,
        label.padding = unit(0.5, "lines"),
        label.r = unit(0.15, "lines"),
        fontface = "bold",
        color = text_color,
        fill = bg_color
      ) +
      scale_x_date(
        date_labels = "%Y-%m",
        date_breaks = "1 month"
      ) +
      scale_y_continuous(
        breaks = seq(0, max_redondeado, by = 20),
        labels = seq(0, max_redondeado, by = 20)
      )
  })
  
  output$plotts_ordenes <- renderPlot({
    
    filtered <- filtered_data_OPT()
    ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
    
    is_dark_mode <- input$dark_mode
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(ordenes) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ordenes_por_mes <- ordenes %>%
      group_by(Mes = format(fecha_op, "%Y-%m")) %>%
      summarise(Cantidad = n())
    
    ordenes_por_mes$Mes <- as.Date(paste0(ordenes_por_mes$Mes, "-01"))
    ordenes_por_mes$Año <- format(ordenes_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(ordenes_por_mes$Año)) != 0)
    
    max_cantidad <- max(ordenes_por_mes$Cantidad, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(ordenes_por_mes, aes(x = Mes, y = Cantidad)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Cantidad", title = "Ordenes realizadas por mes") +
      geom_vline(
        xintercept = as.numeric(ordenes_por_mes$Mes[cambios_de_año]),
        color = "#00bcf5",  
        linetype = "dashed",  
        size = 1  
      ) +
      theme(
        plot.background = element_rect(fill = bg_color),  
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "#0aa1c6"),
        axis.title.y = element_text(face = "bold", size = 15, color = text_color),
        axis.title.x = element_text(face = "bold", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, face = "bold", color = text_color),
        axis.text.y = element_text(face = "bold", size = 10, color = text_color)
      ) +
      geom_label(
        data = ordenes_por_mes,
        aes(label = paste0(Cantidad)),
        size = 4,
        label.padding = unit(0.5, "lines"),
        label.r = unit(0.15, "lines"),
        fontface = "bold",
        color = text_color,
        fill = bg_color
      ) +
      scale_x_date(
        date_labels = "%Y-%m",
        date_breaks = "1 month"
      ) +
      scale_y_continuous(
        breaks = seq(0, max_redondeado, by = 20),
        labels = seq(0, max_redondeado, by = 20)
      )
  })
  
  output$plotts_pagos <- renderPlot({
    
    filtered <- filtered_data_OPT()
    
    ord_pag <- filtered %>% group_by(fecha_op, nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag <- ord_pag %>% group_by(Mes = format(fecha_op, "%Y-%m")) %>% summarise(Monto = sum(total), Pago = sum(Pagado))
    
    ord_pag <- na.omit(ord_pag)
    
    ord_pag$Debe <- ord_pag$Monto - ord_pag$Pago
    
    ord_pag_long <- ord_pag %>%
      pivot_longer(
        cols = c(Monto, Pago, Debe),
        names_to = "Tipo",
        values_to = "Monto"
      ) %>%
      rename(Fecha = Mes)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white")  
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#33ff13", "#15715d")
    
    if (nrow(ord_pag_long) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    # Crear una paleta de colores con suficiente cantidad de colores
    color_pago <- ifelse(is_dark_mode, "#33ff13", "#15715d")  
    color_monto <- ifelse(is_dark_mode, "#03FFFF", "#050587")
    color_debe <- ifelse(is_dark_mode, "#FF0000", "#A30303")
    
    # Aplicarlo en el gráfico
    ggplot(ord_pag_long, aes(x = Fecha, y = Monto, color = Tipo, group = Tipo)) +
      geom_line(linetype = "solid", size = 1) +
      geom_point(size = 2, shape = 21, stroke = 1.5) +
      scale_color_manual(values = c("Debe" = color_debe, "Pago" = color_pago, 
                                    "Monto" = color_monto), name = "Tipo") +  # Aplica la paleta de colores
      theme_minimal_hgrid() +
      labs(x = "Mes", y = "Monto (S/.)", title = "Recaudación por mes") +
      theme(
        plot.background = element_rect(fill = bg_color),  
        legend.position = "top",
        legend.text = element_text(size = 12, vjust = 0.5, face = "bold", color = text_color),
        legend.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#0aa1c6"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "#0aa1c6"),
        axis.title.y = element_text(face = "bold", size = 15, color = text_color),
        axis.title.x = element_text(face = "bold", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5, face = "bold", color = text_color),
        axis.text.y = element_text(face = "bold", size = 10, color = text_color)
      ) + 
      scale_y_continuous(labels = etiquetas) +
      geom_label(
        data = ord_pag_long,
        aes(label = paste0(scales::dollar(round(Monto,2), prefix = "S/. "))),
        size = 3.5,
        label.padding = unit(0.5, "lines"),
        label.r = unit(0.15, "lines"),
        fontface = "bold",
        color = text_color,
        fill = bg_color
      )
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::               TABLAS                 :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExceltabla_ts <- downloadHandler(
    filename = function() {
      paste("Procesos_por_mes", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      filtered <- filtered_data_OPT()
      
      recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
      recetas <- recetas %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Recetas = n())
      
      cotizaciones <- filtered %>% distinct(nro_cotizacion, .keep_all = TRUE)
      cotizaciones <- cotizaciones %>% group_by(Mes = format(reg_cotizacion, "%Y-%m")) %>% summarise(Cotizaciones = n())
      
      ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
      ordenes <- ordenes %>% group_by(Mes = format(fecha_op, "%Y-%m")) %>% summarise(Ordenes = n())
      
      ord_pag <- filtered %>% group_by(fecha_op, nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
      ord_pag <- ord_pag %>% group_by(Mes = format(fecha_op, "%Y-%m")) %>% summarise(Monto = sum(total), Pago = sum(Pagado))
      
      unir <- merge(x = recetas, y = cotizaciones, by = "Mes", all.x = TRUE)
      unir <- merge(x = unir, y = ordenes, by = "Mes", all.x = TRUE)
      unir <- merge(x = unir, y = ord_pag, by = "Mes", all.x = TRUE)
      unir$Debe <- unir$Monto - unir$Pago
      
      write.xlsx(unir, file, sheetName = "Procesos_por_mes", row.names = FALSE)
    }
  )
  
  output$tabla_ts <- renderDataTable({
    
    filtered <- filtered_data_OPT()
    
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    recetas <- recetas %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Recetas = n())
    
    cotizaciones <- filtered %>% distinct(nro_cotizacion, .keep_all = TRUE)
    cotizaciones <- cotizaciones %>% group_by(Mes = format(reg_cotizacion, "%Y-%m")) %>% summarise(Cotizaciones = n())
    
    ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
    ordenes <- ordenes %>% group_by(Mes = format(fecha_op, "%Y-%m")) %>% summarise(Ordenes = n())
    
    ord_pag <- filtered %>% group_by(fecha_op, nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag <- ord_pag %>% group_by(Mes = format(fecha_op, "%Y-%m")) %>% summarise(Monto = sum(total), Pago = sum(Pagado))
    
    unir <- merge(x = recetas, y = cotizaciones, by = "Mes", all.x = TRUE)
    unir <- merge(x = unir, y = ordenes, by = "Mes", all.x = TRUE)
    unir <- merge(x = unir, y = ord_pag, by = "Mes", all.x = TRUE)
    unir$Debe <- unir$Monto - unir$Pago
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  output$downloadExceltabla_recetas <- downloadHandler(
    
    filename = function() {
      paste("Recetas_por_mes", Sys.Date(), ".xlsx", sep = "_")
    },
    
    content = function(file) {
      filtered <- filtered_data_OPT()
      
      recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
      recetas <- recetas %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Recetas = n())
      
      recetas_cot <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
      # Filtrar solo a los que tienen recetas
      recetas_cot <- recetas_cot %>% filter(Tiene_cotizacion == "Si")
      recetas_cot <- recetas_cot %>% distinct(nro_cotizacion, .keep_all = TRUE)
      recetas_cot_mes <- recetas_cot %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Cotizaciones = n())
      
      # Filtrar solo a los que tienen ordenes
      recetas_cot_ordenes <- filtered %>% filter(estatus == "ENVIADO A TALLER")
      recetas_cot_ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
      recetas_cot_ordenes_mes <- recetas_cot_ordenes %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Ordenes = n())
      
      # Filtrar solo a los que tienen pagos
      recetas_pagos <- filtered %>% filter(estatus == "ENVIADO A TALLER")
      recetas_pagos <- recetas_pagos %>%
        arrange(nro_orden, desc(Tiene_pago == "Si")) %>%
        distinct(nro_orden, .keep_all = TRUE)
      recetas_pagos <- recetas_pagos %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Pagos = n())
      
      unir <- merge(x = recetas, y = recetas_cot_mes, by = "Mes", all.x = TRUE)
      unir <- merge(x = unir, y = recetas_cot_ordenes_mes, by = "Mes", all.x = TRUE)
      unir <- merge(x = unir, y = recetas_pagos, by = "Mes", all.x = TRUE)
      
      write.xlsx(unir, file, sheetName = "Recetas_seguimiento_por_mes", row.names = FALSE)
    }
  )
  
  output$tabla_recetas <- renderDataTable({
    
    filtered <- filtered_data_OPT()
    
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    recetas <- recetas %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Recetas = n())
    
    recetas_cot <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    # Filtrar solo a los que tienen recetas
    recetas_cot <- recetas_cot %>% filter(Tiene_cotizacion == "Si")
    recetas_cot <- recetas_cot %>% distinct(nro_cotizacion, .keep_all = TRUE)
    recetas_cot_mes <- recetas_cot %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Cotizaciones = n())
    
    # Filtrar solo a los que tienen ordenes
    recetas_cot_ordenes <- filtered %>% filter(estatus == "ENVIADO A TALLER")
    recetas_cot_ordenes <- filtered %>% distinct(nro_orden, .keep_all = TRUE)
    recetas_cot_ordenes_mes <- recetas_cot_ordenes %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Ordenes = n())
    
    # Filtrar solo a los que tienen pagos
    recetas_pagos <- filtered %>% filter(estatus == "ENVIADO A TALLER")
    recetas_pagos <- recetas_pagos %>%
      arrange(nro_orden, desc(Tiene_pago == "Si")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    recetas_pagos <- recetas_pagos %>% group_by(Mes = format(reg_receta, "%Y-%m")) %>% summarise(Pagos = n())
    
    unir <- merge(x = recetas, y = recetas_cot_mes, by = "Mes", all.x = TRUE)
    unir <- merge(x = unir, y = recetas_cot_ordenes_mes, by = "Mes", all.x = TRUE)
    unir <- merge(x = unir, y = recetas_pagos, by = "Mes", all.x = TRUE)
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::     REALIZAR GRAFICOS OPTOMETRAS    :::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plotoptometra <- renderPlot({
    
    filtered <- filtered_data_OPT()
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(recetas) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    optometras <- recetas %>%
      group_by(optometra) %>%
      summarise(Cantidad = n()) %>% mutate(Porcentaje = prop.table(Cantidad)*100)
    
    max_cantidad <- max(optometras$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = optometras, aes(x = reorder(optometra, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Optometras") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_optometra <- downloadHandler(
    filename = function() {
      paste("Optometras_recetas", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT()
      recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
      optometras <- recetas %>%
        group_by(optometra) %>%
        summarise(Cantidad = n()) %>% mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Cantidad)) 
      
      write.xlsx(optometras, file, sheetName = "Optometras_recetas", row.names = FALSE)
    }
  )
  
  output$tab_optometra <- renderDataTable({
    
    filtered <- filtered_data_OPT()
    
    recetas <- filtered %>% distinct(nro_receta, .keep_all = TRUE)
    optometras <- recetas %>%
      group_by(optometra) %>%
      summarise(Cantidad = n()) %>% mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Cantidad)) 
    
    optometras$Porcentaje <- round(optometras$Porcentaje,2)
    
    df <- optometras
    
    datatable(
      df,
      escape = FALSE,
      options = list(
        columnDefs = list(
          list(
            targets = 1, # Índice de la columna nombre
            render = JS(
              "function(data, type, row) {",
              "  var imgSrc = '';",
              "  if (data == 'Dr Deza') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/img/caras/drdeza.jpg'; }",
              "  else if (data == 'Dra Guerrero') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/img/caras/draguerrero.jpg'; }",
              "  else if (data == 'Dr Magno') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/inlaser/img/Dr%20Magno.jpg'; }",
              "  else if (data == 'Dra Obispo') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/inlaser/img/Dra%20Obispo.jpg'; }",
              "  else if (data == 'Dr Joel Benjamin Apaza Suclli') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/img/caras/drjoel.jpg'; }",
              "  else if (data == 'Dr Samaniego') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/inlaser/img/Dr%20Samaniego.jpg'; }",
              "  else if (data == 'SIN OPTOMETRA') { imgSrc = 'https://clinicalafuente.com/favicon.png'; }",
              "  else if (data == 'Dr Indira Sutta') { imgSrc = 'http://190.117.45.164:29000/oftalmologia/img/caras/drasutta.jpg'; }",
              "  else { imgSrc = 'https://clinicalafuente.com/favicon.png'; }",
              "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
              "}"
            )
          )
        )
      ),
      callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::       RELACIONADO A COTIZACION       :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_luna <- renderPlot({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    luna <- cotizacion %>% filter(tipluna != "No Registro") %>%
      group_by(tipluna) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100)
    
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(luna) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(luna$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = luna, aes(x = reorder(tipluna, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Tipo Luna") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_luna <- downloadHandler(
    filename = function() {
      paste("Luna_cotizacion", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT2()
      
      cotizacion <- filtered %>%
        arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_cotizacion, .keep_all = TRUE)
      
      luna <- cotizacion %>% filter(tipluna != "No Registro") %>%
        group_by(tipluna) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(luna, file, sheetName = "Luna_cotizacion", row.names = FALSE)
    }
  )
  
  output$tab_luna <- renderDataTable({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    luna <- cotizacion %>% filter(tipluna != "No Registro") %>%
      group_by(tipluna) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje))
    
    luna$Porcentaje <- round(luna$Porcentaje, 2)
    
    df <- luna
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_material <- renderPlot({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    material <- cotizacion %>% filter(tipmateriales != "No Registro") %>%
      group_by(tipmateriales) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(material) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(material$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = material, aes(x = reorder(tipmateriales, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Material cotización") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_material <- downloadHandler(
    filename = function() {
      paste("Material_cotizacion", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT2()
      
      cotizacion <- filtered %>%
        arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_cotizacion, .keep_all = TRUE)
      
      material <- cotizacion %>% filter(tipmateriales != "No Registro") %>%
        group_by(tipmateriales) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(material, file, sheetName = "Material_cotizacion", row.names = FALSE)
    }
  )
  
  output$tab_material <- renderDataTable({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    material <- cotizacion %>% filter(tipmateriales != "No Registro") %>%
      group_by(tipmateriales) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje))
    
    material$Porcentaje <- round(material$Porcentaje, 2)
    
    df <- material
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_propiedad <- renderPlot({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    propiedad <- cotizacion %>% filter(propiedades != "No Registro") %>%
      group_by(propiedades) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(propiedad) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(propiedad$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = propiedad, aes(x = reorder(propiedades, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Propiedades") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_propiedad <- downloadHandler(
    filename = function() {
      paste("Propiedad_cotizacion", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT2()
      
      cotizacion <- filtered %>%
        arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_cotizacion, .keep_all = TRUE)
      
      propiedad <- cotizacion %>% filter(propiedades != "No Registro") %>%
        group_by(propiedades) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(propiedad, file, sheetName = "Propiedad_cotizacion", row.names = FALSE)
    }
  )
  
  output$tab_propiedad <- renderDataTable({
    
    filtered <- filtered_data_OPT2()
    
    cotizacion <- filtered %>%
      arrange(nro_cotizacion, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE)
    
    propiedad <- cotizacion %>% filter(propiedades != "No Registro") %>%
      group_by(propiedades) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje)) 
    
    propiedad$Porcentaje <- round(propiedad$Porcentaje, 2)
    
    df <- propiedad
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_laboratorio <- renderPlot({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    laboratorio <- orden %>% filter(laboratorio != "No Registro") %>%
      group_by(laboratorio) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(laboratorio) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(laboratorio$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = laboratorio, aes(x = reorder(laboratorio, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Laboratorio") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_laboratorio <- downloadHandler(
    filename = function() {
      paste("Laboratorio_orden", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT3()
      
      orden <- filtered %>%
        arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_orden, .keep_all = TRUE)
      
      laboratorio <- orden %>% filter(laboratorio != "No Registro") %>%
        group_by(laboratorio) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(laboratorio, file, sheetName = "Laboratorio_orden", row.names = FALSE)
    }
  )
  
  output$tab_laboratorio <- renderDataTable({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    laboratorio <- orden %>% filter(laboratorio != "No Registro") %>%
      group_by(laboratorio) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje))
    
    laboratorio$Porcentaje <- round(laboratorio$Porcentaje, 2)
    
    df <- laboratorio
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_montura <- renderPlot({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    montura <- orden %>% filter(tipo_montura != "No Registro") %>%
      group_by(tipo_montura) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(montura) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(montura$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = montura, aes(x = reorder(tipo_montura, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_color
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Montura") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_montura <- downloadHandler(
    filename = function() {
      paste("Montura_orden", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT3()
      
      orden <- filtered %>%
        arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_orden, .keep_all = TRUE)
      
      montura <- orden %>% filter(tipo_montura != "No Registro") %>%
        group_by(tipo_montura) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(montura, file, sheetName = "Montura_orden", row.names = FALSE)
    }
  )
  
  output$tab_montura <- renderDataTable({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    montura <- orden %>% filter(tipo_montura != "No Registro") %>%
      group_by(tipo_montura) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje))
    
    montura$Porcentaje <- round(montura$Porcentaje, 2)
    
    df <- montura
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_color <- renderPlot({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    color <- orden %>% filter(!color %in% c("No Registro","")) %>%
      group_by(color) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>% head(12)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(color) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(color$Porcentaje, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = color, aes(x = reorder(color, Porcentaje), y = Porcentaje)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Porcentaje (%)", title = "Optometras") + 
      geom_label(aes(label = paste0(Cantidad," (",round(Porcentaje, 2), "%)")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas2, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 10))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_color <- downloadHandler(
    filename = function() {
      paste("Color_orden", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT3()
      
      orden <- filtered %>%
        arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
        distinct(nro_orden, .keep_all = TRUE)
      
      color <- orden %>% filter(!color %in% c("No Registro","")) %>%
        group_by(color) %>% summarise(Cantidad = n()) %>% 
        mutate(Porcentaje = prop.table(Cantidad)*100) %>%
        arrange(desc(Porcentaje))
      
      write.xlsx(color, file, sheetName = "Color_orden", row.names = FALSE)
    }
  )
  
  output$tab_color <- renderDataTable({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>%
      arrange(nro_orden, desc(estatus == "ENVIADO A TALLER")) %>%
      distinct(nro_orden, .keep_all = TRUE)
    
    color <- orden %>% filter(!color %in% c("No Registro","")) %>%
      group_by(color) %>% summarise(Cantidad = n()) %>% 
      mutate(Porcentaje = prop.table(Cantidad)*100) %>%
      arrange(desc(Porcentaje))
    
    color$Porcentaje <- round(color$Porcentaje, 2)
    
    df <- color
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::      RELACIONADO A LAS ORDENES       :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_pagos <- downloadHandler(
    filename = function() {
      paste("Pagos_orden", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(usuario_editor) %>% 
        summarise(Monto = sum(precio))
      
      write.xlsx(usuario, file, sheetName = "Pagos_orden", row.names = FALSE)
    }
  )
  
  output$tab_pagos <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(usuario_editor) %>% 
      summarise(Monto = scales::dollar(sum(precio), prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  output$downloadExcel_dias <- downloadHandler(
    filename = function() {
      paste("Dias_prom", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT()
      
      # Empezemos con las Recetas - Cotizacion
      rec_cot <- filtered %>%
        arrange(nro_cotizacion, desc(Tiene_pago == "Si")) %>%
        distinct(nro_cotizacion, .keep_all = TRUE) %>%
        summarise(Media_Dias = round(mean(Dias_rec_cot, na.rm = TRUE),2),
                  Mediana_Dias = median(Dias_rec_cot, na.rm = TRUE)) %>%
        mutate(Tipo = "Receta - cotización") %>%
        select(Tipo, Media_Dias, Mediana_Dias)
      
      # Cotizacion - Ordenes
      cot_ord <- filtered %>%
        arrange(nro_orden, desc(Tiene_pago == "Si")) %>%
        distinct(nro_orden, .keep_all = TRUE) %>%
        summarise(Media_Dias = round(mean(Dias_cot_ord, na.rm = TRUE),2),
                  Mediana_Dias = median(Dias_cot_ord, na.rm = TRUE)) %>%
        mutate(Tipo = "Cotización - orden") %>%
        select(Tipo, Media_Dias, Mediana_Dias)
      
      # Ordenes Pagos
      ord_pag <- filtered %>%
        distinct(nro_orden, .keep_all = TRUE) %>%
        filter(Tiene_pago == "Si") %>%
        summarise(Media_Dias = round(mean(Dias_ord_pag, na.rm = TRUE),2),
                  Mediana_Dias = median(Dias_ord_pag, na.rm = TRUE)) %>%
        mutate(Tipo = "Orden - pago") %>%
        select(Tipo, Media_Dias, Mediana_Dias)
      
      # Unimos
      unir <- rbind(rec_cot, cot_ord)
      unir <- rbind(unir, ord_pag)
      
      write.xlsx(unir, file, sheetName = "Dias_prom", row.names = FALSE)
    }
  )
  
  output$tab_dias <- renderDataTable({
    
    filtered <- filtered_data_OPT()
    
    # Empezemos con las Recetas - Cotizacion
    rec_cot <- filtered %>%
      arrange(nro_cotizacion, desc(Tiene_pago == "Si")) %>%
      distinct(nro_cotizacion, .keep_all = TRUE) %>%
      summarise(Media_Dias = round(mean(Dias_rec_cot, na.rm = TRUE),2),
                Mediana_Dias = median(Dias_rec_cot, na.rm = TRUE)) %>%
      mutate(Tipo = "Receta - cotización") %>%
      select(Tipo, Media_Dias, Mediana_Dias)
    
    # Cotizacion - Ordenes
    cot_ord <- filtered %>%
      arrange(nro_orden, desc(Tiene_pago == "Si")) %>%
      distinct(nro_orden, .keep_all = TRUE) %>%
      summarise(Media_Dias = round(mean(Dias_cot_ord, na.rm = TRUE),2),
                Mediana_Dias = median(Dias_cot_ord, na.rm = TRUE)) %>%
      mutate(Tipo = "Cotización - orden") %>%
      select(Tipo, Media_Dias, Mediana_Dias)
    
    # Ordenes Pagos
    ord_pag <- filtered %>%
      distinct(nro_orden, .keep_all = TRUE) %>%
      filter(Tiene_pago == "Si") %>%
      summarise(Media_Dias = round(mean(Dias_ord_pag, na.rm = TRUE),2),
                Mediana_Dias = median(Dias_ord_pag, na.rm = TRUE)) %>%
      mutate(Tipo = "Orden - pago") %>%
      select(Tipo, Media_Dias, Mediana_Dias)
    
    # Unimos
    unir <- rbind(rec_cot, cot_ord)
    unir <- rbind(unir, ord_pag) 
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::        MOVIMIENTOS ECONOMICOS          ::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$Monto_sis_caja <- renderInfoBox({
    
    filtered <- filtered_data_OPT3()
    
    ord_pag <- filtered %>% group_by(nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag$Debe <- ord_pag$total - ord_pag$Pagado
    
    ord_pag <- na.omit(ord_pag)
    
    total <- sum(ord_pag$total)
    pagos <- sum(ord_pag$Pagado)
    debe <- sum(ord_pag$Debe)
    
    infoBox(
      paste0("Monto Total"), 
      paste0(scales::dollar(total, prefix = "S/. ")), 
      paste0("De las ordenes generadas"),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "info",
      width = NULL
    )
  })
  
  output$Pago_sis_caja <- renderInfoBox({
    
    filtered <- filtered_data_OPT3()
    
    ord_pag <- filtered %>% group_by(nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag$Debe <- ord_pag$total - ord_pag$Pagado
    
    ord_pag <- na.omit(ord_pag)
    
    total <- sum(ord_pag$total)
    pagos <- sum(ord_pag$Pagado)
    debe <- sum(ord_pag$Debe)
    
    infoBox(
      paste0("Pago realizado"), 
      paste0(scales::dollar(pagos, prefix = "S/. ")), 
      paste0("De las ordenes generadas"),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "success",
      width = NULL
    )
  })
  
  output$Debe_sis_caja <- renderInfoBox({
    
    filtered <- filtered_data_OPT3()
    
    ord_pag <- filtered %>% group_by(nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
    ord_pag$Debe <- ord_pag$total - ord_pag$Pagado
    
    ord_pag <- na.omit(ord_pag)
    
    total <- sum(ord_pag$total)
    pagos <- sum(ord_pag$Pagado)
    debe <- sum(ord_pag$Debe)
    
    infoBox(
      paste0("Deuda"), 
      paste0(scales::dollar(debe, prefix = "S/. ")), 
      paste0("De las ordenes generadas"),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "warning",
      width = NULL
    )
  })
  
  #:::::::::::::::::::::            LUNA              ::::::::::::::::::::::::::
  
  output$plottipo_luna_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipluna) %>% 
      summarise(Monto = sum(precio))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(tipluna, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_blank(), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por luna") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_luna_mont <- downloadHandler(
    filename = function() {
      paste("Luna_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(tipluna) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Luna_monto", row.names = FALSE)
    }
  )
  
  output$tab_pagos <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipluna) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::          MATERIAL             :::::::::::::::::::::::::
  
  output$plottipo_material_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipmateriales) %>% 
      summarise(Monto = sum(precio))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(tipmateriales, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_text(face = "bold", size = 10, color = text_color), #text_color
        axis.text.x = element_blank(), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por materiales") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_material_mont <- downloadHandler(
    filename = function() {
      paste("Material_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(tipmateriales) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Material_monto", row.names = FALSE)
    }
  )
  
  output$tabla_material_mont <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipmateriales) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::          PROPIEDADES             ::::::::::::::::::::::
  
  output$plottipo_propiedades_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(propiedades) %>% 
      summarise(Monto = sum(precio))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(propiedades, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_blank(), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por propiedades") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_propiedades_mont <- downloadHandler(
    filename = function() {
      paste("Propiedades_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(propiedades) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Propiedades_monto", row.names = FALSE)
    }
  )
  
  output$tabla_propiedades_mont <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(propiedades) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::            MONTURA             ::::::::::::::::::::::::
  
  output$plottipo_montura_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipo_montura) %>% 
      summarise(Monto = sum(precio))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(tipo_montura, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_blank(), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por montura") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_montura_mont <- downloadHandler(
    filename = function() {
      paste("Montura_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(tipo_montura) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Montura_monto", row.names = FALSE)
    }
  )
  
  output$tabla_montura_mont <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(tipo_montura) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::          LABORATORIO           ::::::::::::::::::::::::
  
  output$plottipo_laboratorio_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(laboratorio) %>% 
      summarise(Monto = sum(precio))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(laboratorio, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_blank(), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por laboratorio") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_laboratorio_mont <- downloadHandler(
    filename = function() {
      paste("Laboratorio_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(laboratorio) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Laboratorio_monto", row.names = FALSE)
    }
  )
  
  output$tabla_laboratorio_mont <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(laboratorio) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::          COLOR           ::::::::::::::::::::::::
  
  output$plottipo_color_mont <- renderPlot({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(color) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      head(15)
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(usuario) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = usuario, aes(x = reorder(color, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_blank(), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por color") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExceltabla_color_mont <- downloadHandler(
    filename = function() {
      paste("Color_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT4()
      
      usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
        group_by(color) %>% 
        summarise(Monto = sum(precio)) %>%
        arrange(desc(Monto))
      
      write.xlsx(usuario, file, sheetName = "Color_monto", row.names = FALSE)
    }
  )
  
  output$tabla_color_mont <- renderDataTable({
    
    filtered <- filtered_data_OPT4()
    
    usuario <- filtered %>% filter(Tiene_pago == "Si") %>% 
      group_by(color) %>% 
      summarise(Monto = sum(precio)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- usuario
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::           LISTA DE PACIENTES           ::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcelpacientes <- downloadHandler(
    filename = function() {
      paste("Pacientes_ordenes", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_OPT3()
      
      ord_pag <- filtered %>% group_by(fecha_op, nro_orden, total) %>% summarise(Pagado = sum(precio, na.rm = TRUE))
      orden <- filtered %>% filter(!is.na(precio)) %>%
        group_by(nro_orden, total, fecha_op) %>% 
        summarise(
          PAGADO = sum(precio),
          HCL = first(nhcl),
          NOMBRE = first(nombre),
          ULTIMO_PAGO = last(fecha_pago_caja),
          .groups = "drop"
        ) %>%
        mutate(DEBE = total - PAGADO) %>%
        select(HCL, NOMBRE, FECHA_ORDEN = fecha_op, MONTO_ORDEN = total,
               PAGADO = PAGADO, FECHA_ULT_PAGO = ULTIMO_PAGO, DEBE) %>%
        mutate(ESTATUS = ifelse(DEBE == 0, "COMPLETO",
                                ifelse(DEBE > 0, "FALTA", "CASO SOCIAL")))
      
      write.xlsx(orden, file, sheetName = "Pacientes_ordenes", row.names = FALSE)
    }
  )
  
  output$tabla_pacientes <- renderDataTable({
    
    filtered <- filtered_data_OPT3()
    
    orden <- filtered %>% filter(!is.na(precio)) %>%
      group_by(nro_orden, total, fecha_op) %>% 
      summarise(
        PAGADO = sum(precio),
        HCL = first(nhcl),
        NOMBRE = first(nombre),
        ULTIMO_PAGO = last(fecha_pago_caja),
        .groups = "drop"
      ) %>%
      mutate(DEBE = total - PAGADO) %>%
      select(HCL, NOMBRE, FECHA_ORDEN = fecha_op, MONTO_ORDEN = total,
             PAGADO = PAGADO, FECHA_ULT_PAGO = ULTIMO_PAGO, DEBE) %>%
      mutate(ESTATUS = ifelse(DEBE == 0, "COMPLETO",
                              ifelse(DEBE > 0, "FALTA", "CASO SOCIAL"))) 
    
    DT::datatable(
      orden,
      options = list(
        pageLength = 20,
        rowCallback = JS(
          "function(row, data, index) {",
          "  var estado = data[8];", # Índice 8 corresponde a la columna `ESTATUS`
          "  var isDark = document.body.classList.contains('dark-mode');",
          "  var estadoClass = estado.toLowerCase().replace(' ', '-') + (isDark ? '-dark' : '-light');",
          "  $(row).addClass(estadoClass);",
          "}"
        )
      ),
      class = 'table table-striped table-hover'
    )
    
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::        ANALISIS DE DATOS DE CAJA          :::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$dynamicTitleRegisterActualizar <- renderUI({
    req(data$CAJA)
    
    fecha <- max(data$CAJA$fecha)
    
    titulo <- paste0(
      "<span class='dynamic-title-light'>Fecha última actualización: </span>",
      "<span class='textdinamic'>",
      fecha,
      " actualizar en Sistemas</span>"
    )
    
    if (isTRUE(input$dark_mode)) {
      titulo <- paste0(
        "<span class='dynamic-title-dark'>Fecha última actualización: </span>",
        "<span class='textdinamic-dark'>",
        fecha,
        " actualizar en Sistemas</span>"
      )
    }
    
    h3(HTML(titulo), style = "font-weight: bold; text-align: center; margin-bottom: 20px;")
  })
  
  output$Boletas_realizadas <- renderInfoBox({
    
    filtered <- filtered_data_CAJA()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    cant_boletas <- nrow(boletas)
    
    infoBox(
      paste0("Boletas: "),
      cant_boletas,
      paste0("realizadas"),
      icon = icon("file-lines"),
      color = "info",
      width = NULL
    )
  })
  
  output$Recaudacion_caja <- renderInfoBox({
    
    filtered <- filtered_data_CAJA()
    
    recaudacion <- sum(filtered$producto_total)
    
    infoBox(
      paste0("Recaudación: "), 
      paste0(scales::dollar(recaudacion, prefix = "S/. ")),
      paste0("en base a los filtros seleccionados"),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "success",
      width = NULL
    )
    
  })
  
  output$plotts_caja_costos <- renderPlot({
    
    filtered <- filtered_data_CAJA()
    
    is_dark_mode <- input$dark_mode
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(filtered) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    recaudacion_por_mes <- filtered %>%
      group_by(Mes = format(fecha, "%Y-%m")) %>%
      summarise(Monto = sum(producto_total))
    
    recaudacion_por_mes$Mes <- as.Date(paste0(recaudacion_por_mes$Mes, "-01"))
    recaudacion_por_mes$Año <- format(recaudacion_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(recaudacion_por_mes$Año)) != 0)
    
    max_cantidad <- max(recaudacion_por_mes$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(recaudacion_por_mes, aes(x = Mes, y = Monto)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Monto", title = "Recaudación por mes") +
      geom_vline(
        xintercept = as.numeric(recaudacion_por_mes$Mes[cambios_de_año]),
        color = "#00bcf5",  
        linetype = "dashed",  
        size = 1  
      ) +
      theme(
        plot.background = element_rect(fill = bg_color),  
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold", color = "#0aa1c6"),
        axis.title.y = element_text(face = "bold", size = 15, color = text_color),
        axis.title.x = element_text(face = "bold", size = 15, color = text_color),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, face = "bold", color = text_color),
        axis.text.y = element_text(face = "bold", size = 10, color = text_color)
      ) +
      scale_y_continuous(labels = etiquetas) +
      geom_label(
        data = recaudacion_por_mes,
        aes(label = scales::dollar(Monto, prefix = "S/. ")),
        size = 4,
        label.padding = unit(0.5, "lines"),
        label.r = unit(0.15, "lines"),
        fontface = "bold",
        color = text_color,
        fill = bg_color
      ) +
      scale_x_date(
        date_labels = "%Y-%m",
        date_breaks = "1 month"
      )
  })
  
  # En tabla
  output$downloadExceltabla_tscaja_costos <- downloadHandler(
    filename = function() {
      paste("Pagos_caja", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_CAJA()
      
      costo <- filtered %>%
        group_by(Mes = format(fecha, "%Y-%m")) %>%
        summarise(Monto = sum(producto_total))
      
      write.xlsx(costo, file, sheetName = "Pagos_caja", row.names = FALSE)
    }
  )
  
  output$tabla_tscaja_costos <- renderDataTable({
    
    filtered <- filtered_data_CAJA()
    
    costo <- filtered %>%
      group_by(Mes = format(fecha, "%Y-%m")) %>%
      summarise(Monto = scales::dollar(sum(producto_total), prefix = "S/. "))
    
    df <- costo
    
    datatable(
      df
    )
  })
  
  output$plotdescripcion_caja <- renderPlot({
    
    filtered <- filtered_data_CAJA()
    
    descripcion <- filtered %>% 
      group_by(descripcion) %>% 
      summarise(Monto = sum(producto_total)) %>%
      arrange(desc(Monto))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(descripcion) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    ggplot(data = descripcion, aes(x = reorder(descripcion, Monto), y = Monto)) + 
      geom_bar(stat = "identity", fill = line_color) + 
      coord_flip() + 
      theme_minimal_hgrid() +
      theme(
        plot.background = element_rect(fill = bg_color), #bg_colou
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = text_color),
        axis.title.y = element_blank(), #text_color
        axis.title.x = element_blank(), #text_color
        axis.text.x = element_text(size = 10, angle = 60, vjust = 0.5, face = "bold", color = text_color), #text_color
        axis.text.y = element_text(face = "bold", size = 10, color = text_color), #text_color
      ) + 
      labs(y = "Monto (S/.)", title = "Montos acumulados por descripcion") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4)
    
  })
  
  output$downloadExcel_descripcion_caja <- downloadHandler(
    filename = function() {
      paste("Descripcion_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_CAJA()
      
      descripcion <- filtered %>% 
        group_by(descripcion) %>% 
        summarise(Cantidad = n(),
          Monto = sum(producto_total)) %>%
        arrange(desc(Monto))
      
      write.xlsx(descripcion, file, sheetName = "Descripcion_monto", row.names = FALSE)
    }
  )
  
  output$tab_descripcion_caja <- renderDataTable({
    
    filtered <- filtered_data_CAJA()
    
    descripcion <- filtered %>% 
      group_by(descripcion) %>% 
      summarise(Cantidad = n(),
                Monto = sum(producto_total)) %>%
      arrange(desc(Monto)) %>%
      mutate(Monto = scales::dollar(Monto, prefix = "S/. "))
    
    df <- descripcion
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::        DESCARGAR DATOS PACIENTES CAJA          ::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcelpacientes_caja <- downloadHandler(
    filename = function() {
      paste("Pacientes_caja", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_CAJA()
      
      px <- filtered %>% select(fecha, nombre, descripcion, cantidad, producto_total)
      
      write.xlsx(descripcion, file, sheetName = "Descripcion_monto", row.names = FALSE)
    }
  )
  
  output$tabla_pacientes_caja <- renderDataTable({
    
    filtered <- filtered_data_CAJA()
    
    px <- filtered %>% select(fecha, nombre, descripcion, cantidad, producto_total)
    
    df <- px
    
    datatable(
      df
    )
  })
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)


