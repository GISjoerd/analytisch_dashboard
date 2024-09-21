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

monitoring_server <- function(input, output, session) {
  
#### Gemiddelde temperatuurs-/luchtvochtigheidsverloop alle fieldlabs ####
  verloopgrafiek <- function(parameter, start_date, end_date) {
    # Determine the time interval for aggregation based on the time range
    hour_timestamp <- ifelse(difftime(end_date, start_date, units = "days") > 8, "day", "hour")
    
    # Adjust end_date to include the entire day
    end_date <- as.POSIXct(paste0(end_date, " 23:59:59"))
    
    # Construct the SQL query
    query <- paste0("SELECT DATE_TRUNC('", hour_timestamp, "', time_stamp) AS hour_timestamp, 
                    AVG(value) AS avg_value,
                    fieldlab_name AS fieldlab_name
                    FROM view_graphs
                    WHERE parameter_description = '", parameter, "' ",
                    " AND time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "'",
                    " GROUP BY hour_timestamp, fieldlab_name")
    
    # Execute the SQL query to fetch data from the database
    data <- dbGetQuery(con, query)
    
    # Convert hour_timestamp to POSIXct
    data$hour_timestamp <- as.POSIXct(data$hour_timestamp)
    
    # Get unique field lab names
    fieldlabs <- unique(data$fieldlab_name)
    
    # Assign colors to each field lab
    fieldlab_colors <- setNames(line_colors[seq_along(fieldlabs)], fieldlabs)
    
    # Create a ggplot object
    p <- ggplot(data, aes(x = hour_timestamp, y = avg_value, color = fieldlab_name, group = fieldlab_name, text = paste("Datum:", hour_timestamp, "<br>", parameter, round(avg_value, 1)))) +
      geom_line() +
      labs(
        x = "Datum",
        y = ifelse(parameter == "Temperatuur", paste(parameter, "(°C)"), 
                   ifelse(parameter == "Luchtvochtigheid", paste(parameter, "(%)"), parameter)),
        color = "Field Lab"
      ) +
      scale_color_manual(values = fieldlab_colors) +
      custom_theme()
    
    
    # Convert ggplot object to plotly
    plotly_obj <- ggplotly(p, tooltip = "text", dynamicTicks = TRUE) %>%
      config(
        displaylogo = FALSE) %>%
      layout(
        legend = list(
          orientation = "h",
          y = 1.3,
          x = 0,
          traceorder = "reversed",  
          title = list(text = ""),  
          font = list(size = 12)
        )
      ) 
    
    return(plotly_obj)
  }
  
  # Temperatuur grafiek
  output$verloopgrafiek_temperatuur <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]

    verloopgrafiek("Temperatuur", start_date, end_date)
  })

  # Luchtvochtigheid grafiek
  output$verloopgrafiek_luchtvochtigheid <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]

    verloopgrafiek("Luchtvochtigheid", start_date, end_date)
  })
  
#### Gemiddelde temperatuurs-/luchtvochtigheidsverloop per fieldlab ####
  
  verloopgrafiek_fieldlab <- function(parameter, fieldlab_name, start_date, end_date) {
    # Determine the time interval for aggregation based on the time range
    hour_timestamp <- ifelse(difftime(end_date, start_date, units = "days") > 8, "day", "hour")
    # Adjust end_date to include the entire day
    end_date <- as.POSIXct(paste0(end_date, " 23:59:59"))
    
    # Construct the SQL query
    query <- paste0("SELECT DATE_TRUNC('", hour_timestamp, "', time_stamp) AS hour_timestamp, 
                   AVG(value) AS avg_value,
                   tag AS sensor_name,
                   schimmel as schimmel
                   FROM view_graphs
                   WHERE parameter_description = '", parameter, "' 
                   AND fieldlab_name = '", fieldlab_name, "'")
    
    query <- paste0(query, " AND time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "'")
    
    # Group by the appropriate time interval
    query <- paste0(query, " GROUP BY hour_timestamp, sensor_name, schimmel")
    
    # Execute the SQL query to fetch data from the database
    data <- dbGetQuery(con, query)
    
    # Convert hour_timestamp to POSIXct
    data$hour_timestamp <- as.POSIXct(data$hour_timestamp)
    
    # Convert schimmel to a factor with appropriate labels
    data$schimmel <- factor(data$schimmel, levels = c(0, 1), labels = c("Geen Schimmel", "Schimmel"))
    
    # Create a ggplot object with less smooth lines
    p <- ggplot(data, aes(x = hour_timestamp, y = avg_value, color = schimmel, group = sensor_name, text = paste(" Sensor:", sensor_name, "<br> Datum:", hour_timestamp, "<br>", parameter, round(avg_value, 1)))) +
      geom_line() +
      labs(x = "Datum", y = ifelse(parameter == "Temperatuur", paste(parameter, "(°C)"), ifelse(parameter == "Luchtvochtigheid", paste(parameter, "(%)"), parameter)), color = "Schimmel") +
      scale_color_manual(values = c("Schimmel" = "#FF8C29", "Geen Schimmel" = "#1789FC")) +
      custom_theme()
    
    # Convert ggplot object to plotly
    plotly_obj <- ggplotly(p, tooltip = "text", dynamicTicks = TRUE) %>%
      config(
        displaylogo = FALSE) %>%
      layout(
        legend = list(
          orientation = "h",
          y = 1.3,
          x = 0,
          traceorder = "reversed",  
          title = list(text = ""),  
          font = list(size = 12)
        )
      ) 
    
    return(plotly_obj)
  }
  
  #plot verloopgrafiek voor havelte
  output$verloopgrafiek_fieldlab_havelte_t <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Temperatuur", "Havelte", start_date, end_date)
  })
  

  output$verloopgrafiek_fieldlab_havelte_l <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Luchtvochtigheid", "Havelte", start_date, end_date)
  })
  
  #plot verloopgrafiek voor rotterdam
  output$verloopgrafiek_fieldlab_rotterdam_t <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Temperatuur", "Rotterdam", start_date, end_date)
  })
  
  output$verloopgrafiek_fieldlab_rotterdam_l <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Luchtvochtigheid", "Rotterdam", start_date, end_date)
  })
  
  #plot verloopgrafiek voor schoorl
  output$verloopgrafiek_fieldlab_schoorl_t <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Temperatuur", "Schoorl", start_date, end_date)
  })
  
  output$verloopgrafiek_fieldlab_schoorl_l <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Luchtvochtigheid", "Schoorl", start_date, end_date)
  })
  
  #plot verloopgrafiek voor wamel
  output$verloopgrafiek_fieldlab_wamel_t <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Temperatuur", "Wamel", start_date, end_date)
  })
  
  output$verloopgrafiek_fieldlab_wamel_l <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Luchtvochtigheid", "Wamel", start_date, end_date)
  })
  
  #plot verloopgrafiek voor sprundel
  output$verloopgrafiek_fieldlab_sprundel_t <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Temperatuur", "Sprundel", start_date, end_date)
  })
  
  output$verloopgrafiek_fieldlab_sprundel_l <- renderPlotly({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]
    
    verloopgrafiek_fieldlab("Luchtvochtigheid", "Sprundel", start_date, end_date)
  })
  
  
#### Tabel gemiddelde temperatuurs-/luchtvochtigheidsverloop ####
  tabelVerloopgrafiek <- function(parameter, start_date, end_date, fieldlab) {

    # Format start_date and end_date in 'YYYY-MM-DD' format
    start_date <- format(start_date, "%Y-%m-%d")
    end_date <- format(end_date, "%Y-%m-%d")
    
    # Query data from the database to obtain the average value for each hour for each field lab
    query <- paste0("SELECT TO_CHAR(DATE_TRUNC('hour', time_stamp), 'YYYY-MM-DD HH24:MI') AS datum, 
                  ROUND(AVG(CAST(value AS NUMERIC)), 1) AS avg_value,
                  fieldlab_name,
                  schimmel, 
                  tag as sensor
                  FROM view_graphs
                  WHERE parameter_description = '", parameter, "'")
    
    if (fieldlab != "Alle fieldlabs") {
      query <- paste0(query, " AND fieldlab_name = '", fieldlab, "'")
    }
    
    # Add conditions for date range if start_date and end_date are not empty
    if (!is.na(start_date) && !is.na(end_date)) {
      query <- paste0(query, " AND time_stamp >= '", start_date, " 00:00:00' AND time_stamp <= '", end_date, " 23:59:59'")
    }
    
    query <- paste0(query, " GROUP BY DATE_TRUNC('hour', time_stamp), fieldlab_name, schimmel, tag")
    
    # Execute the SQL query to fetch data from the database
    data <- dbGetQuery(con, query)
    
    # Transform schimmel values to "Ja" or "Nee"
    data$schimmel <- ifelse(data$schimmel == 1, "Ja", "Nee")
    
    return(data)
  }
  
  # Tabel luchtvochtigheid
  output$tabel_verloop_luchtvochtigheid <- renderDataTable({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]

    data <- tabelVerloopgrafiek("Luchtvochtigheid", start_date, end_date, fieldlab())

    colnames(data) <- c("Datum", "Luchtvochtigheid (%)", "Fieldlab locatie", "Schimmel", "Sensor")

    # Set the options for the table, including the dom option to customize DOM elements
    datatable(
      data,
      options = list(
        dom = 't', # Only display the table element
        paging = FALSE, # Disable pagination
        searching = FALSE, # Disable search box
        info = FALSE # Disable information summary
      ), rownames = FALSE)
  })
  
  # Tabel temperatuur
  output$tabel_verloop_temperatuur <- renderDataTable({
    start_date <- input$datums_monitoring[1]
    end_date <- input$datums_monitoring[2]

    data <- tabelVerloopgrafiek("Temperatuur", start_date, end_date, fieldlab())


    colnames(data) <- c("Datum", "Temperatuur (°C)", "Fieldlab locatie", "Schimmel", "Sensor")

    # Set the options for the table, including the dom option to customize DOM elements
    datatable(
      data,
      options = list(
        dom = 't', # Only display the table element
        paging = FALSE, # Disable pagination

        searching = FALSE, # Disable search box
        info = FALSE # Disable information summary
      ),
      rownames = FALSE)
  })
  
  ## Download verloopgrafiek luchtvochtigheid
  
  output$downloadVerloopgrafiekLV <- downloadHandler(
    filename = "luchvochtigheidsverloop.csv",
    content = function(file) {
      start_date <- input$datums_monitoring[1]
      end_date <- input$datums_monitoring[2]
      
      data <- tabelVerloopgrafiek("Luchtvochtigheid", start_date, end_date, fieldlab())
      
      colnames(data) <- c("Datum", "Gemiddelde luchtvochtigheid (%)", "Fieldlab locatie", "Schimmel", "Sensor")
      
      write.csv(data, file)
    }
  )
  
  ## Download verloopgrafiek temperatuur
  output$downloadVerloopgrafiekTEMP <- downloadHandler(
    filename = "temperatuursverloop.csv",
    content = function(file) {
      start_date <- input$datums_monitoring[1]
      end_date <- input$datums_monitoring[2]
      
      data <- tabelVerloopgrafiek("Temperatuur", start_date, end_date, fieldlab())
      
      colnames(data) <- c("Datum", "Gemiddelde temperatuur (°C)", "Fieldlab locatie", "Schimmel", "Sensor")
      
      write.csv(data, file)
    }
  )
  
#### Fieldlab locaties kaart ####
  
  # Reactive value to store selected fieldlab
  selected_fieldlab <- reactiveVal(NULL)
  
  # Observe button clicks and update the selected fieldlab
  observeEvent(input$m_alle_fieldlabs, { selected_fieldlab(NULL) })
  observeEvent(input$m_havelte, { selected_fieldlab("Havelte") })
  observeEvent(input$m_rotterdam, { selected_fieldlab("Rotterdam") })
  observeEvent(input$m_schoorl, { selected_fieldlab("Schoorl") })
  observeEvent(input$m_sprundel, { selected_fieldlab("Sprundel") })
  observeEvent(input$m_wamel, { selected_fieldlab("Wamel") })
  
  output$fieldlab_locaties <- renderLeaflet({
    fieldlab <- selected_fieldlab()
    
    query <- if (is.null(fieldlab)) {
      "SELECT DISTINCT ST_AsText(sl.geom) AS geom_text, vg.tag, vg.schimmel
       FROM sensor_locations sl
       JOIN view_graphs vg ON sl.tag = vg.tag
       ORDER BY vg.schimmel DESC"
      
    } else {
      sprintf("SELECT DISTINCT ST_AsText(sl.geom) AS geom_text, vg.tag, vg.schimmel
               FROM sensor_locations sl
               JOIN view_graphs vg ON sl.tag = vg.tag
               WHERE vg.fieldlab_name = '%s'", fieldlab)
    }
    
    fieldlab_data <- dbGetQuery(con, query)

    
    fieldlab_sf <- st_as_sf(fieldlab_data, wkt = "geom_text")
    fieldlab_coords <- st_coordinates(fieldlab_sf)
    
    # Create Leaflet map
    leaflet(width="100%", height="100%", options=leafletOptions(minZoom = 5, maxZoom = 21)) %>%
      addCircleMarkers(
        lng = fieldlab_coords[, "X"],
        lat = fieldlab_coords[, "Y"],
        popup = paste(
          "Tag:", fieldlab_data$tag, "<br>",
          "Schimmel:", ifelse(fieldlab_data$schimmel == 1, "aanwezig", "niet aanwezig")
        ),
        radius = 5,
        fillOpacity = 0.8,
        fillColor = sapply(fieldlab_data$schimmel, function(schimmel) {
          if (schimmel == 1) "#FF8C29" else "#1789FC"
        }), 
        stroke = TRUE,
        color = "#FFF",
        weight = 1,
        opacity = 1
      ) %>%
      
      addWMSTiles(
        baseUrl = "https://service.pdok.nl/hwh/luchtfotorgb/wmts/v1_0",
        layers = "Actueel_orthoHR",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, maxZoom = 21),
        attribution = NULL
      ) %>%
      fitBounds(
        lng1 = min(fieldlab_coords[, "X"]),
        lat1 = min(fieldlab_coords[, "Y"]),
        lng2 = max(fieldlab_coords[, "X"]),
        lat2 = max(fieldlab_coords[, "Y"])
      ) %>%
      addControl(
        html = '<div id="noordpijl"><img src="noordpijl.png" width="21" height="40" style="background-color: #f0f0f0;"></div>',
        position = "bottomleft" 
      )
  })
  

#### Offline sensoren tabel ####
  
  # Define a function to fetch and process observation data
  get_sensorstatus_data <- function(fieldlab) {
    if (fieldlab == "Alle fieldlabs") {
      query <- "
    SELECT DISTINCT ON (vg.tag)
           vg.tag, 
           vg.fieldlab_name,
           TO_CHAR(MAX(DATE_TRUNC('hour', vg.time_stamp)), 'YYYY-MM-DD') AS last_observation_date
    FROM view_graphs vg
    JOIN fieldlab_device fd ON vg.tag = fd.tag
    WHERE fd.end_date IS NULL
    GROUP BY vg.tag, vg.fieldlab_name
    ORDER BY vg.tag, MAX(vg.time_stamp) DESC"
    } else {
      query <- paste0("
    SELECT DISTINCT ON (vg.tag)
           vg.tag, 
           vg.fieldlab_name,
           TO_CHAR(MAX(DATE_TRUNC('hour', vg.time_stamp)), 'YYYY-MM-DD') AS last_observation_date
    FROM view_graphs vg
    JOIN fieldlab_device fd ON vg.tag = fd.tag
    WHERE vg.fieldlab_name = '", fieldlab, "'
    AND fd.end_date IS NULL
    GROUP BY vg.tag, vg.fieldlab_name
    ORDER BY vg.tag, MAX(vg.time_stamp) DESC")
    }
    
    sensorstatusdata <- dbGetQuery(con, query)
    
    # Calculate the number of days since the last observation and round it to full days
    sensorstatusdata$days_since_last_observation <- floor(as.numeric(difftime(Sys.time(), as.Date(sensorstatusdata$last_observation_date), units = "days")))
    
    # Filter rows where days_since_last_observation > 0
    sensorstatusdata <- sensorstatusdata[sensorstatusdata$days_since_last_observation > 0, ]
    
    # Sort by last_observation_date (newest first)
    sensorstatusdata <- sensorstatusdata[order(sensorstatusdata$last_observation_date), ]
    
    return(sensorstatusdata)
  }
  
  # Initial value for fieldlab
  fieldlab <- reactiveVal("Alle fieldlabs")
  
  # Update fieldlab based on button clicks
  observeEvent(input$m_alle_fieldlabs, {
    fieldlab("Alle fieldlabs")
  })
  observeEvent(input$m_havelte, {
    fieldlab("Havelte")
  })
  observeEvent(input$m_rotterdam, {
    fieldlab("Rotterdam")
  })
  observeEvent(input$m_schoorl, {
    fieldlab("Schoorl")
  })
  observeEvent(input$m_sprundel, {
    fieldlab("Sprundel")
  })
  observeEvent(input$m_wamel, {
    fieldlab("Wamel")
  })
  
  # Reactieve expressie om sensor status gegevens op te halen
  sensorstatus <- reactive({
    get_sensorstatus_data(fieldlab())
  })
  
  # Render the DataTable or message based on data availability
  output$sensorstatus_table <- renderUI({
    df <- sensorstatus()
    
    if (nrow(df) == 0) {
      # No data available, render message
      p("Geen sensoren offline bij dit fieldlab")
    } else {
      # Data available, render DataTable
      
      df <- df[, c("tag", "fieldlab_name", "days_since_last_observation", "last_observation_date")]
      colnames(df) <- c("Sensor", "Fieldlab", "Dagen sinds laatste waarneming", "Datum")
      
      renderDataTable({
        datatable(df, options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        ), rownames = FALSE)
      })
    }
  })
  
  # Count the number of sensors offline
  offline_sensor_count <- reactive({
    df <- sensorstatus()
    count_offline <- sum(df$days_since_last_observation > 0)
    return(count_offline)
  })
  
  # Display the number of offline sensors as text output
  output$aantal_sensoren_offline <- renderText({
    count <- offline_sensor_count()
    paste("Aantal sensoren offline:", count)
  })
  
}
