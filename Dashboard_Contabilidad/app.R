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
  factura <- dbGetQuery(dbcaja, statement = "SELECT tipo, serie, numero, codigo, nombre, fecha, total from factura where fecha >= '2025-05-01'")
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
  
  caja$FAMILIA <- ifelse(caja$producto == "113" & is.na(caja$FAMILIA), "OPTICA", caja$FAMILIA)
  caja$FAMILIA <- ifelse(caja$producto == "115" & is.na(caja$FAMILIA), "OFTALMOLOGIA", caja$FAMILIA)
  caja$FAMILIA <- ifelse(is.na(caja$FAMILIA), "FARMACIA", caja$FAMILIA)
  
  caja$FAMILIA <- ifelse(caja$FAMILIA == "E. OPT", "OPTICA",
                         ifelse(caja$FAMILIA == "FARMAC", "FARMACIA",
                                ifelse(caja$FAMILIA == "GLAUCO", "GLAUCOMA",
                                       ifelse(caja$FAMILIA == "OCULO", "OCULOPLASTIA",
                                              ifelse(caja$FAMILIA == "ODONTO", "ODONTOLOGIA",
                                                     ifelse(caja$FAMILIA == "OFTALM", "OFTALMOLOGIA",
                                                            ifelse(caja$FAMILIA %in% c("REFRAC", "REFRACTIVA"), "INLASER",caja$FAMILIA)))))))
  
  caja$SUBFAMILIA <- ifelse(caja$SUBFAMILIA == "CIRUGÍ", "CIRUGIAS",
                            ifelse(caja$SUBFAMILIA %in% c("CONST","CONSUL","CONST."), "CONSULTAS",
                                   ifelse(caja$SUBFAMILIA == "DOCU.", "DOCUMENTOS",
                                          ifelse(caja$SUBFAMILIA %in% c("EX. ES","EX.ESP","EXAM."),"EXAMENES ESPECIALES",
                                                 ifelse(caja$SUBFAMILIA %in% c("PROC.","PROC.E","PROCEDIMIENTO"),"PROCEDIMIENTOS ESPECIALES",
                                                        ifelse(caja$SUBFAMILIA == "ALQUI/","ALQUILER EQUIPOS",caja$SUBFAMILIA))))))
  
  caja$SUBFAMILIA <- ifelse(is.na(caja$SUBFAMILIA), caja$FAMILIA, caja$SUBFAMILIA)
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
    title = "CONTABILIDAD",
    header = dashboardHeader(
      title = dashboardBrand(
        title = h4("CONT. FUENTE"),
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
          text = "Análisis Caja",
          icon = icon("cash-register"),
          startExpanded = FALSE,  # No está expandido por defecto para permitir el comportamiento de expandir/contraer
          menuSubItem(
            text = "Grupo",
            tabName = "subtab2_1",
            icon = icon("layer-group")
          ),
          menuSubItem(
            text = "Productos/Servicios",
            tabName = "subtab2_2",
            icon = icon("capsules")
          ),
          menuSubItem(
            text = "Persona",
            tabName = "subtab2_3",
            icon = icon("users")
          ),
          menuSubItem(
            text = "KARDEX",
            tabName = "subtab2_4",
            icon = icon("file")
          )
        )#,
        #menuItem(
        #  text = "Sistema Caja",
        #  tabName = "tab3", #No se necesita
        #  icon = icon("cash-register"),
        #  startExpanded = FALSE,  # No está expandido por defecto para permitir el comportamiento de expandir/contraer
        #  menuSubItem(
        #    text = "Análisis General",
        #    tabName = "subtab2_2",
        #    icon = icon("chart-line")
        #  )#,
        #  menuSubItem(
        #    text = "Análisis Individual",
        #    tabName = "subtab3_2",
        #    icon = icon("user")
        #  )
        #)#,
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
  
  data <- reactiveValues(LOG = NULL, CAJA = NULL)
  update_data <- function() {
    #data$OPT <- update_opt_fuente()
    data$CAJA <- update_caja_fuente()
    #data$RF <- update_google_sheets_RF()
    data$LOG <- update_mysql_login()
  }
  
  update_login <- function() {
    data$LOG <- update_mysql_login()
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
        if (input$user_name %in% c("admin","ccruzj","vmazaneth")) {
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
        HTML("Actualizado el 06/04/2026<br>
           Ahora puedes visualizar los resultados de los procesos de CAJA en la interfaz
           Recuerda solicitar actualización de datos<br>
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
  
  filtered_data_GRUPO <- reactive({
    
    # Filtrar las fechas
    selected_date_grupo <- input$selectDate_Grupo
    #selected_date_cotizacion <- input$selectDate_Cotizacion
    #selected_date_orden <- input$selectDate_Orden
    #selected_date_caja <- input$selectDate_Caja
    # Filtrar familia y subfamilia
    selected_familia <- input$selectFamilia
    selected_subfamilia <- input$selectSubfamilia
    
    filtered <- data$CAJA
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_familia) > 1,
      length(selected_subfamilia) > 1,
      !is.null(selected_date_grupo)#,
      #!is.null(selected_date_cotizacion),
      #!is.null(selected_date_orden),
      #!is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$CAJA
      
      if (!"Todos" %in% selected_familia) {
        filtered <- filtered[filtered$FAMILIA %in% selected_familia, ]
      }
      
      if (!"Todos" %in% selected_subfamilia) {
        filtered <- filtered[filtered$SUBFAMILIA %in% selected_subfamilia, ]
      }
      
      if (!is.null(selected_date_grupo)) {
        filtered <- filtered[filtered$fecha >= selected_date_grupo[1] & filtered$fecha <= selected_date_grupo[2], ]
      }
      
    } else {
      filtered <- data$CAJA
    }
    
    return(filtered)
  })
  
  filtered_data_PRODUCTO <- reactive({
    
    # Filtrar las fechas
    selected_date_grupo <- input$selectDate_Grupo2
    #selected_date_cotizacion <- input$selectDate_Cotizacion
    #selected_date_orden <- input$selectDate_Orden
    #selected_date_caja <- input$selectDate_Caja
    # Filtrar familia y subfamilia
    selected_familia <- input$selectFamilia2
    selected_subfamilia <- input$selectSubfamilia2
    selected_producto <- input$selectProducto
    
    filtered <- data$CAJA
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_familia) > 1,
      length(selected_subfamilia) > 1,
      length(selected_producto) > 1,
      !is.null(selected_date_grupo)#,
      #!is.null(selected_date_cotizacion),
      #!is.null(selected_date_orden),
      #!is.null(selected_date_caja)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$CAJA
      
      if (!"Todos" %in% selected_familia) {
        filtered <- filtered[filtered$FAMILIA %in% selected_familia, ]
      }
      
      if (!"Todos" %in% selected_subfamilia) {
        filtered <- filtered[filtered$SUBFAMILIA %in% selected_subfamilia, ]
      }
      
      if (!"Todos" %in% selected_producto) {
        filtered <- filtered[filtered$descripcion %in% selected_producto, ]
      }
      
      if (!is.null(selected_date_grupo)) {
        filtered <- filtered[filtered$fecha >= selected_date_grupo[1] & filtered$fecha <= selected_date_grupo[2], ]
      }
      
    } else {
      filtered <- data$CAJA
    }
    
    return(filtered)
  })
  
  #observeEvent(input$updateButtonOPT, {
  #  showModal(modalDialog(
  #    title = "Actualizando datos Optica",
  #    "Por favor espere mientras se actualizan los datos.",
  #    easyClose = FALSE,
  #    footer = NULL
  #  ))
    
  #  update_dataOPT()
    
  #  removeModal()
  #})
  
  observeEvent(input$updateButtonCAJA, {
    showModal(modalDialog(
      title = "Actualizando datos Caja",
      "Por favor espere mientras se actualizan los datos.",
      easyClose = FALSE,
      footer = NULL
    ))
    
    update_dataCAJA()
    
    removeModal()
  })
  
  observeEvent(input$updateButtonCAJA2, {
    showModal(modalDialog(
      title = "Actualizando datos Caja",
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
      
      update_dataCAJA()
      req(data$CAJA)
      
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(
              width = 12,
              h1("Análisis y control Contabilidad"),
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
          h1("Registro de procedimientos Caja por Grupo"),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Filtros"),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha"
                ),
                dateRangeInput("selectDate_Grupo", label = NULL, start = min(data$CAJA$fecha, na.rm = TRUE), 
                               end = max(data$CAJA$fecha, na.rm = TRUE))
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Area"
                ),
                selectInput("selectFamilia", label = NULL, choices = c("Todos", names(table(data$CAJA$FAMILIA))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital-user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " SubArea"
                ),
                selectInput("selectSubfamilia", label = NULL, choices = c("Todos", names(table(data$CAJA$SUBFAMILIA))), selected = "Todos", multiple = TRUE)
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
            h3("Resultados"),
            br(),
            tabsetPanel(
              id = "tabsetGenerales",
              tabPanel(
                title = "General",
                icon = icon("industry"),
                fluidRow(
                  column(12, uiOutput("dynamicTitleFechaCaja"))
                ),
                fluidRow(
                  
                    column(4, infoBoxOutput("Boletas_emitidas_grupo", width = 12)),
                    column(4, infoBoxOutput("Cantidades_producto_grupo", width = 12)),
                    column(4, infoBoxOutput("Recaudacion_grupo", width = 12))
                  ),
                  fluidRow(
                    column(12, 
                           tabBox(
                             title = "Evolución costos",
                             selected = "Costos (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Costos (G)",
                               width = 12,
                               plotOutput("plotts_costos_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla resumen",
                               width = 12,
                               downloadButton("downloadExceltabla_ts_costos_grupo", "Descargar Excel"),
                               dataTableOutput('tabla_ts_costos_grupo')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                ),
                div(
                  fluidRow(
                    column(6, 
                           tabBox(
                             title = "Área",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_familia_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_familia_grupo", "Descargar Excel"),
                               dataTableOutput('tab_familia_grupo')
                             )
                           )),
                    column(6, 
                           tabBox(
                             title = "Sub Area",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_subfamilia_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_subfamilia_grupo", "Descargar Excel"),
                               dataTableOutput('tab_subfamilia_grupo')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                )
              )
            ),
          style = "margin-top: 20px;"
        ),
        tabItem(
          tabName = "subtab2_2",
          h1("Registro de procedimientos Caja por Producto/Servicio"),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Filtros"),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha"
                ),
                dateRangeInput("selectDate_Grupo2", label = NULL, start = min(data$CAJA$fecha, na.rm = TRUE), 
                               end = max(data$CAJA$fecha, na.rm = TRUE))
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Area"
                ),
                selectInput("selectFamilia2", label = NULL, choices = c("Todos", names(table(data$CAJA$FAMILIA))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital-user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " SubArea"
                ),
                selectInput("selectSubfamilia2", label = NULL, choices = c("Todos", names(table(data$CAJA$SUBFAMILIA))), selected = "Todos", multiple = TRUE)
              ))
            ),
            fluidRow(
              column(12, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hand-holding-hand", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Producto"
                ),
                selectInput("selectProducto", label = NULL, choices = c("Todos", names(table(data$CAJA$descripcion))), selected = "Todos", multiple = TRUE)
              ))
            )
          ),
          fluidRow(
            column(4,
                   div(
                     actionButton("updateButtonCAJA2", "Actualizar datos", class = "btn btn-primary")#,
                     #id = "updateButton-container"
                   )
            )
          ),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Resultados"),
            br(),
            tabsetPanel(
              id = "tabsetGenerales",
              tabPanel(
                title = "General",
                icon = icon("industry"),
                fluidRow(
                  column(12, uiOutput("dynamicTitleFechaCaja2"))
                ),
                fluidRow(
                  
                  column(4, infoBoxOutput("Boletas_emitidas_grupo2", width = 12)),
                  column(4, infoBoxOutput("Cantidades_producto_grupo2", width = 12)),
                  column(4, infoBoxOutput("Recaudacion_grupo2", width = 12))
                  ),
                  fluidRow(
                    column(12, 
                           tabBox(
                             title = "Evolución costos",
                             selected = "Costos (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Costos (G)",
                               width = 12,
                               plotOutput("plotts_costos_producto", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla resumen",
                               width = 12,
                               downloadButton("downloadExceltabla_ts_costos_producto", "Descargar Excel"),
                               dataTableOutput('tabla_ts_costos_producto')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                ),
                div(
                  fluidRow(
                    column(6, 
                           tabBox(
                             title = "Sub Area",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_subfamilia_producto", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_subfamilia_producto", "Descargar Excel"),
                               dataTableOutput('tab_subfamilia_producto')
                             )
                           )),
                    column(6, 
                           tabBox(
                             title = "Tipo",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_tipo_producto", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_tipo_producto", "Descargar Excel"),
                               dataTableOutput('tab_tipo_producto')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                )
              )
            ),
            style = "margin-top: 20px;"
          ),
        tabItem(
          tabName = "subtab2_3",
          h1("Registro de procedimientos Caja por Persona"),
          tags$div(
            style = "border: 2px solid #0aa1c6; padding: 15px; border-radius: 10px; margin-bottom: 15px;",
            h3("Filtros"),
            fluidRow(
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Fecha"
                ),
                dateRangeInput("selectDate_Grupo", label = NULL, start = min(data$CAJA$fecha, na.rm = TRUE), 
                               end = max(data$CAJA$fecha, na.rm = TRUE))
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " Area"
                ),
                selectInput("selectFamilia", label = NULL, choices = c("Todos", names(table(data$CAJA$FAMILIA))), selected = "Todos", multiple = TRUE)
              )),
              column(4, tags$div(
                class = "input-group",
                tags$span(
                  tags$img(icon("hospital-user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                  " SubArea"
                ),
                selectInput("selectSubfamilia", label = NULL, choices = c("Todos", names(table(data$CAJA$SUBFAMILIA))), selected = "Todos", multiple = TRUE)
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
            h3("Resultados"),
            br(),
            tabsetPanel(
              id = "tabsetGenerales",
              tabPanel(
                title = "General",
                icon = icon("industry"),
                fluidRow(
                  column(12, uiOutput("dynamicTitleFechaCaja"))
                ),
                fluidRow(
                  
                  column(4, infoBoxOutput("Boletas_emitidas_grupo", width = 12)),
                  column(4, infoBoxOutput("Cantidades_producto_grupo", width = 12)),
                  column(4, infoBoxOutput("Recaudacion_grupo", width = 12))
                  ),
                  fluidRow(
                    column(12, 
                           tabBox(
                             title = "Evolución costos",
                             selected = "Costos (G)",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Costos (G)",
                               width = 12,
                               plotOutput("plotts_costos_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla resumen",
                               width = 12,
                               downloadButton("downloadExceltabla_ts_costos_grupo", "Descargar Excel"),
                               dataTableOutput('tabla_ts_costos_grupo')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
                ),
                div(
                  fluidRow(
                    column(6, 
                           tabBox(
                             title = "Área",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_familia_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_familia_grupo", "Descargar Excel"),
                               dataTableOutput('tab_familia_grupo')
                             )
                           )),
                    column(6, 
                           tabBox(
                             title = "Sub Area",
                             selected = "Grafico",
                             status = "primary",
                             solidHeader = FALSE,
                             maximizable = TRUE,
                             width = 12,
                             type = "tabs",
                             tabPanel(
                               title = "Grafico",
                               width = 12,
                               plotOutput("plot_subfamilia_grupo", height = "600px")
                             ),
                             tabPanel(
                               title = "Tabla",
                               width = 12,
                               downloadButton("downloadExcel_subfamilia_grupo", "Descargar Excel"),
                               dataTableOutput('tab_subfamilia_grupo')
                             )
                           ))
                  ),
                  style = "margin-top: 20px;"
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
  
  output$dynamicTitleFechaCaja <- renderUI({
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
  
  output$Boletas_emitidas_grupo <- renderInfoBox({
    
    filtered <- filtered_data_GRUPO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    cant_boletas <- nrow(boletas)
    
    infoBox(
      paste0("Boletas: "),
      cant_boletas,
      paste0("Realizadas"),
      icon = icon("file"),
      color = "info",
      width = NULL
    )
  })
  
  output$Cantidades_producto_grupo <- renderInfoBox({
    
    filtered <- filtered_data_GRUPO()
    
    cantidades <- sum(filtered$cantidad_prod)
    
    infoBox(
      paste0("Cantidades: "),
      cantidades,
      paste0("Productos/servicios solicitados"),
      icon = icon("bag-shopping"),
      color = "info",
      width = NULL
    )
    
  })
  
  output$Recaudacion_grupo <- renderInfoBox({
    
    filtered <- filtered_data_GRUPO()
    
    montos <- sum(filtered$cantidad_pu)
    
    infoBox(
      paste0("Recaudacion: "),
      scales::dollar(montos, prefix = "S/. "),
      paste0("En Total"),
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
  
  output$plotts_costos_grupo <- renderPlot({
    
    filtered <- filtered_data_GRUPO()
    
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
      summarise(Monto = sum(cantidad_pu))
    
    recaudacion_por_mes$Mes <- as.Date(paste0(recaudacion_por_mes$Mes, "-01"))
    recaudacion_por_mes$Año <- format(recaudacion_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(recaudacion_por_mes$Año)) != 0)
    
    max_cantidad <- max(recaudacion_por_mes$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(recaudacion_por_mes, aes(x = Mes, y = Monto)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Cantidad", title = "Montos totales por mes") +
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
      ) +
      scale_y_continuous(
        breaks = seq(0, max_redondeado, by = 100000),
        labels = seq(0, max_redondeado, by = 100000)
      )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::               TABLAS                 :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExceltabla_ts_costos_grupo <- downloadHandler(
    filename = function() {
      paste("Costos_por_mes", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_GRUPO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      costos <- filtered %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Recaudacion = sum(cantidad_pu))
      boletas_mes <- boletas %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Boletas = n())
      
      unir <- merge(x = costos, y = boletas_mes, by = "Mes", all.x = TRUE)
  
      write.xlsx(unir, file, sheetName = "Costos_por_mes", row.names = FALSE)
    }
  )
  
  output$tabla_ts_costos_grupo <- renderDataTable({
    
    filtered <- filtered_data_GRUPO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    costos <- filtered %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Recaudacion = scales::dollar(sum(cantidad_pu), prefix = "S/. "))
    boletas_mes <- boletas %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Boletas = n())
    
    unir <- merge(x = costos, y = boletas_mes, by = "Mes", all.x = TRUE)
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::       REALIZAR GRAFICOS GRUPOS       :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_familia_grupo <- renderPlot({
    
    filtered <- filtered_data_GRUPO()
    areas <- filtered %>% group_by(FAMILIA) %>% 
      summarise(Monto = sum(cantidad_pu)) %>% arrange(desc(Monto))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(areas) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(areas$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = areas, aes(x = reorder(FAMILIA, Monto), y = Monto)) + 
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
      labs(y = "Montos (S/.)", title = "Areas") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 50000))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_familia_grupo <- downloadHandler(
    filename = function() {
      paste("Areas_montos", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_GRUPO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      areas <- filtered %>% group_by(FAMILIA) %>% summarise(Monto = sum(cantidad_pu))
      
      boletas_areas <- boletas %>% group_by(FAMILIA) %>% summarise(Boletas = n())
      
      unir <- merge(x = areas, y = boletas_areas, by = "FAMILIA", all.x = TRUE)
      unir <- unir %>% arrange(desc(Monto))
      
      write.xlsx(unir, file, sheetName = "Areas_montos", row.names = FALSE)
    }
  )
  
  output$tab_familia_grupo <- renderDataTable({
    
    filtered <- filtered_data_GRUPO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    areas <- filtered %>% group_by(FAMILIA) %>% summarise(Monto = sum(cantidad_pu))
    
    boletas_areas <- boletas %>% group_by(FAMILIA) %>% summarise(Boletas = n())
    
    unir <- merge(x = areas, y = boletas_areas, by = "FAMILIA", all.x = TRUE)
    unir <- unir %>% arrange(desc(Monto))
    unir$Monto <- scales::dollar(unir$Monto, prefix = "S/. ")
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_subfamilia_grupo <- renderPlot({
    
    filtered <- filtered_data_GRUPO()
    
    subarea <- filtered %>% group_by(SUBFAMILIA) %>% summarise(Monto = sum(cantidad_pu))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(subarea) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(subarea$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = subarea, aes(x = reorder(SUBFAMILIA, Monto), y = Monto)) + 
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
      labs(y = "Monto (S/)", title = "Sub Área") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 50000))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_subfamilia_grupo <- downloadHandler(
    filename = function() {
      paste("Subarea_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_GRUPO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      subarea <- filtered %>% group_by(SUBFAMILIA) %>%
        summarise(Monto = sum(cantidad_pu))
      boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
        summarise(Boletas = n()) 
      
      unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
      unir <- unir %>% arrange(desc(Monto))
      
      write.xlsx(unir, file, sheetName = "Subarea_monto", row.names = FALSE)
    }
  )
  
  output$tab_subfamilia_grupo <- renderDataTable({
    
    filtered <- filtered_data_GRUPO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    subarea <- filtered %>% group_by(SUBFAMILIA) %>%
      summarise(Monto = sum(cantidad_pu))
    boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
      summarise(Boletas = n()) 
    
    unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
    unir <- unir %>% arrange(desc(Monto))
    unir$Monto <- scales::dollar(unir$Monto, prefix = "S/. ")
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::            ANALISIS POR GRUPOS            :::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$dynamicTitleFechaCaja2 <- renderUI({
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
  
  output$Boletas_emitidas_grupo2 <- renderInfoBox({
    
    filtered <- filtered_data_PRODUCTO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    cant_boletas <- nrow(boletas)
    
    infoBox(
      paste0("Boletas: "),
      cant_boletas,
      paste0("Realizadas"),
      icon = icon("file"),
      color = "info",
      width = NULL
    )
  })
  
  output$Cantidades_producto_grupo2 <- renderInfoBox({
    
    filtered <- filtered_data_PRODUCTO()
    
    cantidades <- sum(filtered$cantidad_prod)
    
    infoBox(
      paste0("Cantidades: "),
      cantidades,
      paste0("Productos/servicios solicitados"),
      icon = icon("bag-shopping"),
      color = "info",
      width = NULL
    )
    
  })
  
  output$Recaudacion_grupo2 <- renderInfoBox({
    
    filtered <- filtered_data_PRODUCTO()
    
    montos <- sum(filtered$cantidad_pu)
    
    infoBox(
      paste0("Recaudacion: "),
      scales::dollar(montos, prefix = "S/. "),
      paste0("En Total"),
      icon = icon("fas fa-hand-holding-dollar"),
      color = "success",
      width = NULL
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #:::::::::::::::::::::::             GRAFICO              ::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plotts_costos_producto <- renderPlot({
    
    filtered <- filtered_data_PRODUCTO()
    
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
      summarise(Monto = sum(cantidad_pu))
    
    recaudacion_por_mes$Mes <- as.Date(paste0(recaudacion_por_mes$Mes, "-01"))
    recaudacion_por_mes$Año <- format(recaudacion_por_mes$Mes, "%Y")
    cambios_de_año <- which(diff(as.numeric(recaudacion_por_mes$Año)) != 0)
    
    max_cantidad <- max(recaudacion_por_mes$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(recaudacion_por_mes, aes(x = Mes, y = Monto)) + 
      geom_line(group = 1, color = line_color, linetype = "solid") + 
      geom_point(size = 1, shape = 21, stroke = 2, fill = "#00bcf5", color = "#00bcf5") +
      theme_minimal_hgrid() +
      labs(x = "Fecha", y = "Cantidad", title = "Montos totales por mes") +
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
      ) +
      scale_y_continuous(
        breaks = seq(0, max_redondeado, by = 100000),
        labels = seq(0, max_redondeado, by = 100000)
      )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::               TABLAS                 :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExceltabla_ts_costos_producto <- downloadHandler(
    filename = function() {
      paste("Costos_por_mes", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_PRODUCTO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      costos <- filtered %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Recaudacion = sum(cantidad_pu))
      boletas_mes <- boletas %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Boletas = n())
      
      unir <- merge(x = costos, y = boletas_mes, by = "Mes", all.x = TRUE)
      
      write.xlsx(unir, file, sheetName = "Costos_por_mes", row.names = FALSE)
    }
  )
  
  output$tabla_ts_costos_producto <- renderDataTable({
    
    filtered <- filtered_data_PRODUCTO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    costos <- filtered %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Recaudacion = scales::dollar(sum(cantidad_pu), prefix = "S/. "))
    boletas_mes <- boletas %>% group_by(Mes = format(fecha, "%Y-%m")) %>% summarise(Boletas = n())
    
    unir <- merge(x = costos, y = boletas_mes, by = "Mes", all.x = TRUE)
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_subfamilia_producto <- renderPlot({
    
    filtered <- filtered_data_PRODUCTO()
    
    subarea <- filtered %>% group_by(SUBFAMILIA) %>% summarise(Monto = sum(cantidad_pu))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(subarea) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(subarea$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = subarea, aes(x = reorder(SUBFAMILIA, Monto), y = Monto)) + 
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
      labs(y = "Monto (S/)", title = "Sub Área") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 50000))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_subfamilia_producto <- downloadHandler(
    filename = function() {
      paste("Subarea_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_PRODUCTO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      subarea <- filtered %>% group_by(SUBFAMILIA) %>%
        summarise(Monto = sum(cantidad_pu))
      boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
        summarise(Boletas = n()) 
      
      unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
      unir <- unir %>% arrange(desc(Monto))
      
      write.xlsx(unir, file, sheetName = "Subarea_monto", row.names = FALSE)
    }
  )
  
  output$tab_subfamilia_producto <- renderDataTable({
    
    filtered <- filtered_data_PRODUCTO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    subarea <- filtered %>% group_by(SUBFAMILIA) %>%
      summarise(Monto = sum(cantidad_pu))
    boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
      summarise(Boletas = n()) 
    
    unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
    unir <- unir %>% arrange(desc(Monto))
    unir$Monto <- scales::dollar(unir$Monto, prefix = "S/. ")
    
    df <- unir
    
    datatable(
      df
    )
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::              GRAFICOS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$plot_subfamilia_producto <- renderPlot({
    
    filtered <- filtered_data_PRODUCTO()
    
    subarea <- filtered %>% group_by(SUBFAMILIA) %>% summarise(Monto = sum(cantidad_pu))
    
    # Detectar si está en modo oscuro o claro con input$dark_mode
    is_dark_mode <- input$dark_mode  # TRUE si está activado, FALSE si no
    
    bg_color <- ifelse(is_dark_mode, "#343a40", "white") 
    text_color <- ifelse(is_dark_mode, "white", "black")
    line_color <- ifelse(is_dark_mode, "#17e0ff", "#38a0b0")
    
    if (nrow(subarea) == 0) {
      return(ggplot() +
               geom_text(aes(x = 1, y = 1, label = "No hay datos disponibles\npara los filtros seleccionados"), 
                         size = 6, fontface = "bold", color = line_color) +
               theme_minimal_hgrid() +
               theme(
                 plot.background = element_rect(fill = bg_color)
               ))
    }
    
    max_cantidad <- max(subarea$Monto, na.rm = TRUE)
    max_redondeado <- ceiling(max_cantidad / 10) * 10  
    
    ggplot(data = subarea, aes(x = reorder(SUBFAMILIA, Monto), y = Monto)) + 
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
      labs(y = "Monto (S/)", title = "Sub Área") + 
      geom_label(aes(label = scales::dollar(Monto, prefix = "S/. ")), 
                 colour = text_color, fill = bg_color,  # Usar la columna calculada
                 fontface = "bold.italic", hjust = 0.2, size = 4) +
      scale_y_continuous(labels = etiquetas, limits = c(0, max_redondeado), breaks = seq(0, max_redondeado, 50000))
    
  })
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::                TABLAS                :::::::::::::::::::::
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$downloadExcel_subfamilia_producto <- downloadHandler(
    filename = function() {
      paste("Subarea_monto", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      
      filtered <- filtered_data_PRODUCTO()
      
      boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
      subarea <- filtered %>% group_by(SUBFAMILIA) %>%
        summarise(Monto = sum(cantidad_pu))
      boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
        summarise(Boletas = n()) 
      
      unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
      unir <- unir %>% arrange(desc(Monto))
      
      write.xlsx(unir, file, sheetName = "Subarea_monto", row.names = FALSE)
    }
  )
  
  output$tab_subfamilia_producto <- renderDataTable({
    
    filtered <- filtered_data_PRODUCTO()
    
    boletas <- filtered %>% distinct(ID, .keep_all = TRUE)
    subarea <- filtered %>% group_by(SUBFAMILIA) %>%
      summarise(Monto = sum(cantidad_pu))
    boletas_subarea <- boletas %>% group_by(SUBFAMILIA) %>%
      summarise(Boletas = n()) 
    
    unir <- merge(x = subarea, y = boletas_subarea, by = "SUBFAMILIA", all.x = TRUE)
    unir <- unir %>% arrange(desc(Monto))
    unir$Monto <- scales::dollar(unir$Monto, prefix = "S/. ")
    
    df <- unir
    
    datatable(
      df
    )
  })
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)


