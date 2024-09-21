library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(bsicons)
library(RPostgres)
library(DT)
library(plotly)
library(shinyjs)

#### Thema ###
thema <- bslib::bs_theme(
  version = 5,
  bg = "#F6F6F5",
  fg = "#000000",
  base_font = font_google("Roboto Condensed"),
  code_font = font_google("Roboto Condensed"))

ui <- page_navbar(
  useShinyjs(),
  includeScript("javaScript.js"),
  
  title = "Rieten Daken Dashboard", 
  
  ## Stylings ##
  bg = "#1789FC",
  theme = thema,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css", version = "1.0")
  ),
  
#### Introductie pagina ####
  nav_panel(
    title = "Introductie",
    icon = icon("map", class = "far"), 
    layout_columns(
      
      # Introductie tekst
      card(
        card_header("Introductie"),
        uiOutput("intro_text")),
      
      # Sensorlocaties kaart
      card(
        card_header("Sensor locaties"),
        leafletOutput("leaflet_kaart"),
        full_screen = TRUE
      )
    ) 
  ),
  
#### Monitoring pagina ####
  nav_panel(
    title = "Monitoring",
    icon = icon("laptop", class = "far"), 
    layout_sidebar(
      
      ## Sidebar met filters ##
      sidebar = sidebar(
        open = "always",
        title = "Filters",
        bg = "#FFFFFF",
        dateRangeInput("datums_monitoring",
                       label = "Selecteer datums", 
                       start = Sys.Date() - 7,
                       end = Sys.time(),
                       format = "dd-mm-yy",
                       language = "nl",
                       weekstart = 1,
                       separator = " tot "
        ),

        actionButton("resetFilters", label = "Alle filters resetten", class = "reset-filters-button")
      ),
      
      ## Fieldlab filters ##
      layout_columns(
        fill = FALSE,
        actionButton("m_alle_fieldlabs", label = "Alle fieldlabs", class = "custom-action-button"),
        actionButton("m_havelte", label = "Havelte", class = "custom-action-button"),
        actionButton("m_rotterdam", label = "Rotterdam", class = "custom-action-button"),
        actionButton("m_schoorl", label = "Schoorl", class = "custom-action-button"),
        actionButton("m_sprundel", label = "Sprundel", class = "custom-action-button"),
        actionButton("m_wamel", label = "Wamel", class = "custom-action-button")
      ),

      layout_columns(
        col_widths = c(6,6),
        max_height="46%", 
        
        # Gemiddelde temperatuursverloop
        uiOutput("dynamic_card"),  # Renderen van dynamische kaart die gedefineerd is in server.R
        
        # Fieldlab locaties kaart
        card(
          card_header("Fieldlab locaties"),
          leafletOutput("fieldlab_locaties"),
          full_screen = TRUE
        )
      ),
      
      layout_columns(
        col_widths = c(6,6),
        max_height="46%",
        
        # Gemiddelde luchtvochtigheidsverloop
        uiOutput("dynamic_card2"),
        
        #Offline sensoren tabel
        card(
          card_header(
            div(
            class = "d-flex justify-content-between align-items-center",
            div( "Offline sensoren",
                 tooltip(
                   id = "info-icon",
                   bs_icon("info-circle"),
                   "Een sensor wordt als offline gerekend als deze geen data doorstuurd of afwijkende waardes doorstuurt voor de temperatuur of luchtvochtigheid van >100.")),
            div(
              class = "sensoren-offline",
              textOutput("aantal_sensoren_offline")
            )
          )),
          uiOutput("sensorstatus_table"),
          full_screen = TRUE
        ),
      ) 
    )
  ),
  
#### Analyse pagina ####
  nav_panel(
    title = "Analyse",
    icon = icon("magnifying-glass-chart", class = "far"), 
    layout_sidebar(
      
      ## Sidebar met filters ##
      sidebar = sidebar(
        open = "always",
        title = "Filters",
        icon = icon("circle-info", class = "far"), 
        bg = "#FFFFFF",
        dateRangeInput("datums_analyse",
                       label = "Selecteer datums", 
                       start = Sys.Date() - 7,
                       end = Sys.Date(),
                       format = "dd-mm-yy",
                       language = "nl",
                       weekstart = 1,
                       separator = " tot "
        ),

        checkboxGroupInput("dakconstructie", label = ("Dakconstructie"), 
                           choices = list("Open constructie" = "open", "Gesloten constructie" = "gesloten"),
                           selected = NULL),
        checkboxGroupInput("dakhoogte", label = ("Hoogte in het dak"), 
                           choices = list("Boven" = "boven", "Midden" = "midden", "Onder" = "onder"),

                           selected = NULL),
        checkboxGroupInput("dakorientatie", label = ("Oriëntatie"), 
                           choices = list("Zonkant" = "zonkant", "Schaduwkant" = "schaduwkant"),
                           selected = NULL),
        actionButton("resetFilters", label = "Alle filters resetten", class = "reset-filters-button")
      ),
      
      ## Fieldlab filters ##
      layout_columns(
        fill = FALSE,
        actionButton("a_alle_fieldlabs", label = "Alle fieldlabs", class = "custom-action-button"),
        actionButton("a_havelte", label = "Havelte", class = "custom-action-button"),
        actionButton("a_rotterdam", label = "Rotterdam", class = "custom-action-button"),
        actionButton("a_schoorl", label = "Schoorl", class = "custom-action-button"),
        actionButton("a_sprundel", label = "Sprundel", class = "custom-action-button"),
        actionButton("a_wamel", label = "Wamel", class = "custom-action-button")
      ),

      layout_columns(
        col_widths = c(6,3,3),
        max_height="46%",
        
        # Gemiddelde temperatuur in en buiten het dak
        uiOutput("dynamic_card3"),  # Renderen van dynamische kaart die gedefineerd is in server.R
        
        # Omstandigheden voor schimmel
        card(
          card_header(
            "Omstandigheden voor schimmel",
            tooltip(
                id = "info-icon",
                bs_icon("info-circle"),
                HTML("Optimaal: Deze grafiek toont het aantal dagen per maand waarop de gemiddelde dagtemperatuur zich
                     tussen de 10 en 25 °C bevond, en waarbij tegelijkertijd de gemiddelde luchtvochtigheid van
                     die dag boven de 70% lag.<br><br>
                     Kritiek: Deze grafiek toont het aantal dagen per maand waarop de temperatuur minstens één keer boven
                     de 35 °C uitkwam of waarop de luchtvochtigheid minstens één keer onder de 18% zakte."))
          ),
          div(
            id = "omstandigheden-grafiek",
            class = "d-flex justify-content-around",
            actionButton("grafiek_optimaal", "optimaal", class="action-button-graph-o"),
            actionButton("grafiek_kritiek", "kritiek", class="action-button-graph-o"),
          ),
          full_screen = TRUE,
          uiOutput("omstandigheden_grafiek")
        ),

        
        # Gemiddelde temperatuur/luchtvochtigheid bij verschillende...
        card(
          id = "staafgrafiek-card",
          card_header(
            id = "gemiddelden_staafgrafiek",
            textOutput("titel_staafgrafiek"),
            popover(
              bs_icon("sliders"),
              title = "Kies parameter",
              radioButtons("parameter_staafgrafiek", label = "",
                           choices = list("Temperatuur" = 1, "Luchtvochtigheid" = 2), 
                           selected = 1)
            ),
            class = "d-flex justify-content-between"
          ),
          div(
            class = "d-flex justify-content-around",
            actionButton("grafiek_hoogtes", "hoogtes", class="action-button-graph-g"),
            actionButton("grafiek_constructies", "constructies", class="action-button-graph-g"),
            actionButton("grafiek_orientaties", "oriëntaties", class="action-button-graph-g")
          ),
          full_screen = TRUE,
          uiOutput("geselecteerde_staafgrafiek")
        )
      ),
      
      layout_columns(
        col_widths = c(6,6),
        max_height="46%",
        
        # Gemiddelde luchtvochtigheid in en buiten het dak
        uiOutput("dynamic_card4"),  # Renderen van dynamische kaart die gedefineerd is in server.R
        
        # Bioclim variabelen
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                "Bioclim variabelen",
                tooltip(
                  id = "info-icon",
                  bs_icon("info-circle"),
                  HTML("Bioklimatische variabelen zijn afgeleid van maandelijkse temperatuur- en luchtvochtigheidswaarden om biologisch betekenisvollere variabelen te genereren.
                    Ze vertegenwoordigen jaarlijkse trends, seizoensgebondenheid en extreme of beperkende omgevingsfactoren.
                    Voor meer over de bioclim variabelen, ga naar: <a href='https://chelsa-climate.org/bioclim/' target='_blank'>www.chelsa-climate.org/bioclim</a>.")
                ),
                class = "d-inline-flex align-items-center"
              ),
              div(
                downloadButton("downloadBioclim", label = "", class = "custom-download-button")
              )
            )
          ),
          dataTableOutput("bioclim_table"),
          full_screen = TRUE
        )
      )
    )
  ),
  
#### Informatie pagina ####
  nav_panel(
    title = "Informatie",
    icon = icon("circle-info", class = "far"), 
    layout_columns(
      fill = FALSE,
          downloadButton("downloadData1", label = "  Metadataformulier Havelte", class = "download-metadata"),
          downloadButton("downloadData2", label = "  Metadataformulier Rotterdam", class = "download-metadata"),
          downloadButton("downloadData3", label = "  Metadataformulier Schoorl", class = "download-metadata"),
          downloadButton("downloadData4", label = "  Metadataformulier Sprundel", class = "download-metadata"),
          downloadButton("downloadData5", label = "  Metadataformulier Wamel", class = "download-metadata")
      ),
    layout_columns(
      fill = FALSE,
      card(
        DTOutput("informatie_tabel"),
      )
    ),
    layout_columns(
      fill = FALSE,
      div(class = "opslaan-container",
          actionButton("wijzigingen_opslaan", label = "Wijzigingen opslaan", class = "wijzigingen-opslaan-button")
      )
    )
  )
)
