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

iconMarker <- makeIcon("icon_location.png",
                       iconWidth = 25,
                       iconHeight = 35, 
                       iconAnchorX = 12,
                       iconAnchorY = 41)

introductie_server <- function(input, output, session) {
  ##### Leaflet kaart ######
  output$leaflet_kaart <- renderLeaflet({
    # Fetch geometry data and fieldlab names from the database
    query <- "SELECT ST_AsText(location) AS geom_text, fieldlab_name 
              FROM fieldlab
              WHERE fieldlab_name NOT LIKE 'HAS test';"
    fieldlab_data <- dbGetQuery(con, query)
    
    # Convert WKT geometry data to spatial objects
    fieldlab_sf <- st_as_sf(fieldlab_data, wkt = "geom_text")
    
    # Extract coordinates
    fieldlab_coords <- st_coordinates(fieldlab_sf)
    
    # Create Leaflet map
    leaflet(width="100%", height="100%", options=leafletOptions(minZoom = 5, maxZoom = 18)) %>%
      
      # Add OpenStreetMap tile layer
      addTiles() %>%
      
      # Set view to display the entire country of the Netherlands
      setView(5.2913, 52.1326, zoom = 7) %>%
      
      # Add markers for each fieldlab location with popup
      addMarkers(lng = fieldlab_coords[, "X"],
                 lat = fieldlab_coords[, "Y"],
                 popup = fieldlab_data$fieldlab_name,
                 icon = iconMarker,
                 popupOptions = popupOptions(offset = c(0, -20)))  
  })

  
  #introductie tekst
  output$intro_text <- renderUI({
    HTML(
      paste(
        "Welkom bij het Rieten Daken Dashboard! In dit dashboard is de data van 48 sensoren te zien, die in 5
        verschillende rieten daken verdeeld door heel Nederland zijn geplaatst. Er zijn sensoren geplaatst in
        een dak in Havelte, Sprundel, Schoorl, Rotterdam en Wamel, te zien op de kaart hiernaast. Deze locaties
        worden ook wel fieldabs genoemd.<br><br>",
        
        "Op de monitoring pagina zijn een aantal algemene visualisaties te zien, om zo te kunnen monitoren welke data binnenkomt en of deze correct binnenkomt.",
        "Op de analyse pagina zijn een aantal inzichtelijkere visualisaties te zien, waarin verschillende factoren vergeleken kunnen worden.",
        "Op de informatie pagina is wat algemene informatie over de fieldlabs te zien en kan er een metadataformulier van elk fieldlab gedownload worden.<br><br>",
        
        "Voor meer informatie over hoe het dashboard werkt kan de Dashboard Handleiding geraadpleegd worden.",
        as.character(downloadLink("download_handleiding", "Download Dashboard Handleiding"))
      )
    )
  })
  
  output$download_handleiding <- downloadHandler(
    filename = function() {
      "dashboard_handleiding.pdf"
    },
    content = function(file) {
      file.copy("www/dashboard_handleiding.pdf", file)
    }
  )
  
  
}
  