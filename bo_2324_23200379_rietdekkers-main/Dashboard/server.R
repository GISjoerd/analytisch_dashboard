library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(bsicons)
library(RPostgres)
library(shinyjs)
library(sf)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(DBI)

source("introductie.R", local = TRUE)
source("monitoring.R", local = TRUE)
source("analyse.R", local = TRUE)
source("informatie.R", local = TRUE)

server <- function(input, output, session) {
  
  introductie_server(input, output, session)
  monitoring_server(input,output,session)
  analyse_server(input, output, session)
  informatie_server(input, output, session)
  
#### Reset filters ####
  observeEvent(input$resetFilters, {
    reset("datums_monitoring")
    reset("dakconstructie")
    reset("dakhoogte")
    reset("dakorientatie")
  })
  
#### Sync filters ####
  ## Datums ##
  observeEvent(input$datums_monitoring, {
    updateDateRangeInput(session, "datums_analyse",
                         start = input$datums_monitoring[1],
                         end = input$datums_monitoring[2]
    )
  })
  
  observeEvent(input$datums_analyse, {
    updateDateRangeInput(session, "datums_monitoring",
                         start = input$datums_analyse[1],
                         end = input$datums_analyse[2]
    )
  })
  
### Dynamische titel staafgrafiek ###
  output$titel_staafgrafiek <- renderText({
    if (input$parameter_staafgrafiek == 1) {
      return("Gemiddelde temperatuur bij verschillende...")
    } else {
      return("Gemiddelde luchtvochtigheid bij verschillende...")
    }

  })


### Dynamisch "card" verandering wanneer er een fieldlab wordt geselecteerd ###
  
  # Set the default state to show Card 1
  output$dynamic_card <- renderUI({
    navset_card_underline(
      nav_panel(title = "Gemiddelde temperatuursverloop per fieldlab", plotlyOutput("verloopgrafiek_temperatuur")),
      nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
      nav_spacer(),
      nav_item(
        downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
      ),
      full_screen = TRUE)
  })
  
  observeEvent(input$m_alle_fieldlabs, {
    cat("alle_fieldlabs clicked, showing Card 1\n")
    output$dynamic_card <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuursverloop per fieldlab", plotlyOutput("verloopgrafiek_temperatuur")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$m_havelte, {
    cat("havelte clicked, showing Card 2\n")
      output$dynamic_card <- renderUI({
        navset_card_underline(
          nav_panel(title = "Gemiddelde temperatuursverloop Havelte", plotlyOutput("verloopgrafiek_fieldlab_havelte_t")),
          nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
          nav_spacer(),
          nav_item(
            downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
          ),
          full_screen = TRUE)
      })
    })
  
  observeEvent(input$m_rotterdam, {
    cat("rotterdam clicked, showing Card 3\n")
    output$dynamic_card <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuursverloop Rotterdam", plotlyOutput("verloopgrafiek_fieldlab_rotterdam_t")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$m_schoorl, {
    cat("schoorl clicked, showing Card 4\n")
    output$dynamic_card <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuursverloop Schoorl", plotlyOutput("verloopgrafiek_fieldlab_schoorl_t")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$m_sprundel, {
    cat("sprundel clicked, showing Card 4\n")
    output$dynamic_card <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuursverloop Sprundel", plotlyOutput("verloopgrafiek_fieldlab_sprundel_t")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$m_wamel, {
    cat("wamel clicked, showing Card 5\n")
    output$dynamic_card <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuursverloop Wamel", plotlyOutput("verloopgrafiek_fieldlab_wamel_t")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })





### Dynamisch "card2 luchtvochtigheid monitoring" verandering wanneer er een fieldlab wordt geselecteerd ###


 #   # Set the default state to show Card 2
 output$dynamic_card2 <- renderUI({
   navset_card_underline(
     nav_panel(title = "Gemiddelde luchtvochtigheidsverloop per fieldlab", plotlyOutput("verloopgrafiek_luchtvochtigheid")),
    nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
    nav_spacer(),
     nav_item(
       downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
     ),
    full_screen = TRUE
    )
 })


  observeEvent(input$m_alle_fieldlabs, {
    cat("alle_fieldlabs clicked, showing Card 1\n")
    output$dynamic_card2 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheidsverloop per fieldlab", plotlyOutput("verloopgrafiek_luchtvochtigheid")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })

  observeEvent(input$m_havelte, {
    cat("havelte clicked, showing Card 2\n")
      output$dynamic_card2 <- renderUI({
        navset_card_underline(
          nav_panel(title = "Gemiddelde luchtvochtigheidsverloop Havelte", plotlyOutput("verloopgrafiek_fieldlab_havelte_l")),
          nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
          nav_spacer(),
          nav_item(
            downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
          ),
          full_screen = TRUE)
      })
    })
   
  observeEvent(input$m_rotterdam, {
    cat("rotterdam clicked, showing Card 3\n")
    output$dynamic_card2 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheidsverloop Rotterdam", plotlyOutput("verloopgrafiek_fieldlab_rotterdam_l")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
   observeEvent(input$m_schoorl, {
    cat("schoorl clicked, showing Card 4\n")
     output$dynamic_card2 <- renderUI({
       navset_card_underline(
         nav_panel(title = "Gemiddelde luchtvochtigheidsverloop Schrool", plotlyOutput("verloopgrafiek_fieldlab_schoorl_l")),
         nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
         nav_spacer(),
         nav_item(
           downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
         ),
         full_screen = TRUE)
     })
   })

  observeEvent(input$m_sprundel, {
    cat("sprundel clicked, showing Card 4\n")
    output$dynamic_card2 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheidsverloop Sprundel", plotlyOutput("verloopgrafiek_fieldlab_sprundel_l")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })

  observeEvent(input$m_wamel, {
    cat("wamel clicked, showing Card 5\n")
    output$dynamic_card2 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheidsverloop Wamel", plotlyOutput("verloopgrafiek_fieldlab_wamel_l")),
        nav_panel(title = "Data", dataTableOutput("tabel_verloop_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadVerloopgrafiekLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  ### Dynamisch analyse "card3" luchtvochtigheid verandering wanneer er een fieldlab wordt geselecteerd ###
  
  # Set the default state to show Card 3
  output$dynamic_card3 <- renderUI({
    navset_card_underline(
      id = "optimalegrafiek",
      nav_panel(title = "Gemiddelde temperatuur in en buiten het dak per fieldlab", plotlyOutput("gemiddelde_grafiek_temperatuur")),
      nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
      nav_spacer(),
      nav_item(
        downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button")
      ),full_screen = TRUE)
   })
  
  observeEvent(input$a_alle_fieldlabs, {
    cat("alle_fieldlabs clicked, showing Card 1\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        id = "optimalegrafiek",
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak per fieldlab", plotlyOutput("gemiddelde_grafiek_temperatuur")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button")
        ),full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_havelte, {
    cat("havelte clicked, showing Card 2\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak Havelte", plotlyOutput("gemiddelde_grafiek_temperatuur_havelte")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_rotterdam, {
    cat("rotterdam clicked, showing Card 3\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak Rotterdam", plotlyOutput("gemiddelde_grafiek_temperatuur_rotterdam")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_schoorl, {
    cat("schoorl clicked, showing Card 4\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak Schoorl", plotlyOutput("gemiddelde_grafiek_temperatuur_schoorl")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_sprundel, {
    cat("sprundel clicked, showing Card 4\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak Sprundel", plotlyOutput("gemiddelde_grafiek_temperatuur_Sprundel")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_wamel, {
    cat("wamel clicked, showing Card 5\n")
    output$dynamic_card3 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde temperatuur in en buiten het dak Wamel", plotlyOutput("gemiddelde_grafiek_temperatuur_Wamel")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_temperatuur")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenTEMP", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  ### Dynamisch analyse "card4" luchtvochtigheid verandering wanneer er een fieldlab wordt geselecteerd ###
  
  # Set the default state to show Card 4
  output$dynamic_card4 <- renderUI({
    navset_card_underline(
      nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak per fieldlab", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid")),
      nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
      nav_spacer(),
      nav_item(
        downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button")
      ),
      full_screen = TRUE)
  })
  
  observeEvent(input$a_alle_fieldlabs, {
    cat("alle_fieldlabs clicked, showing Card 1\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak per fieldlab", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button")
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_havelte, {
    cat("havelte clicked, showing Card 2\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak Havelte", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid_havelte")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_rotterdam, {
    cat("rotterdam clicked, showing Card 3\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak Rotterdam", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid_rotterdam")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_schoorl, {
    cat("schoorl clicked, showing Card 4\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak Schoorl", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid_schoorl")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_sprundel, {
    cat("sprundel clicked, showing Card 4\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak Sprundel", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid_Sprundel")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
  observeEvent(input$a_wamel, {
    cat("wamel clicked, showing Card 5\n")
    output$dynamic_card4 <- renderUI({
      navset_card_underline(
        nav_panel(title = "Gemiddelde luchtvochtigheid in en buiten het dak Wamel", plotlyOutput("gemiddelde_grafiek_luchtvochtigheid_Wamel")),
        nav_panel(title = "Data", dataTableOutput("gemiddelde_tabel_luchtvochtigheid")),
        nav_spacer(),
        nav_item(
          downloadButton("downloadBinnenBuitenLV", label = "", class = "custom-download-button"),
        ),
        full_screen = TRUE)
    })
  })
  
}
  










