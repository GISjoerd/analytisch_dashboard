source("elements.R", local = TRUE)

# Establish the connection with SSL options
con <- dbConnect(
  Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password,
  sslmode = 'require'
)

# Check if connection is successful
if (dbIsValid(con)) {
  print("Connection successful.")
} else {
  print("Connection failed.")
}



informatie_server <- function(input, output, session) {

  
  # Data ophalen
  get_informatie_data <- function() {
    query <- "SELECT *
            FROM informatie_view"
    
    data <- dbGetQuery(con, query, stringsAsFactors = FALSE)
    
    return(data)
  }
  
  # Fetch the data from the database initially and set it to `local$data`
    data <- get_informatie_data()

  # Data als tabel renderen    
  output$informatie_tabel <- renderDT({
  
    # Tabel instellingen
    datatable(
      data,
      editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5))),
      escape = FALSE, 
      rownames = FALSE,
      colnames = c(
        "Fieldlab", 
        "Beschrijving", 
        "Dakconstructie", 
        "Schimmel", 
        "Bouwjaar", 
        "Installatiedatum",
        '<span>Opmerkingen <i class="fa-solid fa-pen-to-square"></i></span>'
      ),
      options = list(
        dom = 't',  
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE
      )
    ) %>%
      formatStyle(
        'beschrijving', 
        width = '20em'
      )
  })
  
  # Dataframe bewerkte data aanmaken
  bewerkte_data <- data
  
  # Observeren van celbewerkingen in de tabel 
  observeEvent(input$informatie_tabel_cell_edit, {
    
    info <- input$informatie_tabel_cell_edit
    i <- info$row
    j <- match("opmerkingen", colnames(bewerkte_data))  
    v <- info$value
    
    # Bewerkingen wijzigen in dataframe
    bewerkte_data[i, j] <- v  
    
    # Bewerkingen updaten in database bij druk op knop
    observeEvent(input$wijzigingen_opslaan, {
      
      for (i in 1:nrow(bewerkte_data)) {
        fieldlab <- bewerkte_data$fieldlab[i]
        opmerkingen <- bewerkte_data$opmerkingen[i]
        
        query <- paste0("UPDATE informatie_view SET opmerkingen = '", opmerkingen, 
                        "' WHERE fieldlab = '", fieldlab, "'")
        
        dbExecute(con, query)
        
        print(paste("Opmerkingen", fieldlab, "geupdated"))
      }
    })
  })
  
  # Download buttons
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "Metadataformulier_Havelte.pdf"
    },
    content = function(file) {
      file.copy("www/Metadataformulier_Havelte.pdf", file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "Metadataformulier_Rotterdam.pdf"
    },
    content = function(file) {
      file.copy("www/Metadataformulier_Rotterdam.pdf", file)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "Metadataformulier_Schoorl.pdf"
    },
    content = function(file) {
      file.copy("www/Metadataformulier_Schoorl.pdf", file)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "Metadataformulier_Sprundel.pdf"
    },
    content = function(file) {
      file.copy("www/Metadataformulier_Sprundel.pdf", file)
    }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "Metadataformulier_Wamel.pdf"
    },
    content = function(file) {
      file.copy("www/Metadataformulier_Wamel.pdf", file)
    }
  )

  
  
  
}
