library(RPostgres)
library(stringr)

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

analyse_server <- function(input, output, session) {
  
  ### Filters ###
  selected_constructie <- reactive({
    input$dakconstructie
  })
  
  selected_hoogte <- reactive({
    input$dakhoogte
  })
  
  selected_orientatie <- reactive({
    input$dakorientatie
  })
  
#### Gemiddelde temperatuur in en buiten het dak alle fieldlabs ####
  
  plotGemiddelde <- function(parameter, start_date, end_date, constructie, hoogte, orientatie) {
    # Determine the time interval for aggregation based on the time range
    hour_timestamp <- ifelse(difftime(end_date, start_date, units = "days") > 8, "day", "hour")
    # Adjust end_date to include the entire day
    end_date <- as.POSIXct(paste0(end_date, " 23:59:59"))
   
    query_avg_data <- paste0("
      SELECT 
        DATE_TRUNC('hour', time_stamp) AS hour_timestamp, 
        AVG(value) AS avg_value,
        fieldlab_name AS fieldlab_name,
        MAX(CASE WHEN parameter_description = 'Schimmel' THEN value ELSE NULL END) AS schimmel_value
      FROM view_graphs
      WHERE parameter_description = '", parameter, "'
        AND time_stamp >= '", start_date, "' 
        AND time_stamp <= '", end_date, "'")
    
    # Voeg constructie IN clausule alleen toe als constructie niet leeg is
    if (length(constructie) > 0) {
      query_avg_data <- paste0(query_avg_data, " AND constructie IN ('", paste(constructie, collapse = "', '"), "')")
    }
    
    # Voeg hoogte IN clausule alleen toe als hoogte niet leeg is
    if (length(hoogte) > 0) {
      query_avg_data <- paste0(query_avg_data, " AND hoogte IN ('", paste(hoogte, collapse = "', '"), "')")
    }
    
    # Voeg orientatie IN clausule alleen toe als orientatie niet leeg is
    if (length(orientatie) > 0) {
      query_avg_data <- paste0(query_avg_data, " AND orientatie IN ('", paste(orientatie, collapse = "', '"), "')")
    }
    
    query_avg_data <- paste0(query_avg_data, "
      GROUP BY hour_timestamp, fieldlab_name")

    
    # Query data from the database to obtain the actual temperature and humidity for each time interval

    query_weather_data <- paste0("
    SELECT 
      DATE_TRUNC('hour', mg.time_stamp) AS hour_timestamp, 
      AVG(mg.act_temp) AS average_temperature,
      AVG(mg.lucht_vocht) AS average_humidity
    FROM meteo_gegevens mg
    WHERE EXISTS (
      SELECT 1
      FROM (", query_avg_data, ") AS avg_subquery
      WHERE DATE_TRUNC('hour', mg.time_stamp) = avg_subquery.hour_timestamp
      AND time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "'
    )
    GROUP BY DATE_TRUNC('hour', mg.time_stamp)")
    
    # Execute the SQL queries to fetch data from the database
    data_avg <- dbGetQuery(con, query_avg_data)
    data_weather <- dbGetQuery(con, query_weather_data)
    
    # Extract hour_timestamp from query_actual_data
    data_weather$hour_timestamp <- as.POSIXct(data_weather$hour_timestamp)
    
    # Get unique field lab names
    fieldlabs <- unique(data_avg$fieldlab_name)
    
    # Assign colors to each field lab
    fieldlab_colors <- setNames(line_colors[seq_along(fieldlabs)], fieldlabs)
    
    # Add colors for temperature and humidity lines
    all_colors <- c(fieldlab_colors, "Buiten temperatuur" = "black", "Buiten luchtvochigheid" = "black")
    
    # Bereken de y-aslimieten voor temperatuur
    y_min_temp <- min(c(data_avg$avg_value, data_weather$average_temperature), na.rm = TRUE)
    y_max_temp <- max(c(data_avg$avg_value, data_weather$average_temperature), na.rm = TRUE)
    
    # Bereken de y-aslimieten voor luchtvochtigheid
    y_min_humidity <- min(c(data_avg$avg_value, data_weather$average_humidity), na.rm = TRUE)
    y_max_humidity <- max(c(data_avg$avg_value, data_weather$average_humidity), na.rm = TRUE)
    
    # Map binary values to labels
    data_avg$schimmel_label <- ifelse(data_avg$schimmel_value == 1, "Schimmel", "Geen Schimmel")
    
    # Create a ggplot object using the fetched data
    p <- ggplot()
    
    # Achtergrond kleuren en buiten weerdata lijn
    if (parameter == "Temperatuur") {
      p <- p +
        geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 10, ymax = 25, fill = "Optimaal"), alpha = 0.1) +
        geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 35, ymax = 40, fill = "Kritiek"), alpha = 0.1) +
        geom_line(data = data_weather, aes(x = hour_timestamp, y = average_temperature, color = "Temperatuur buiten NL",), alpha = 1, linetype = "dashed") +
        scale_fill_manual(values = c("Optimaal" = "#49F566", "Kritiek" = "#F54966", "Temperatuur buiten NL" = '#000')) +
        coord_cartesian(ylim = c(y_min_temp, y_max_temp))  # Correcte y-limieten voor temperatuur
    } else if (parameter == "Luchtvochtigheid") {
      p <- p +
        geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 70, ymax = 100, fill = "Optimaal"), alpha = 0.1) +
        geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 0, ymax = 18, fill = "Kritiek"), alpha = 0.1) +
        geom_line(data = data_weather, aes(x = hour_timestamp, y = average_humidity, color = "Luchtvochtigheid buiten NL"), alpha = 1, linetype = "dashed") +
        scale_fill_manual(values = c("Optimaal" = "#49F566", "Kritiek" = "#F54966", "Luchtvochtigheid buiten NL" = '#000')) +
        coord_cartesian(ylim = c(y_min_humidity, y_max_humidity))  # Correcte y-limieten voor luchtvochtigheid
    }
    
    # Voeg de sensor/fieldlab lijnen toe
    p <- p +
      geom_line(data = data_avg, aes(x = hour_timestamp, y = avg_value, color = fieldlab_name, group = fieldlab_name, text = paste( "Fieldlab:", fieldlab_name,"<br> Datum:", hour_timestamp, "<br>", parameter, round(avg_value, 1)))) +
      labs(x = "Datum", y = parameter, color = "Fieldlab", fill = "Conditie") +
      custom_theme() +
      scale_color_manual(values = all_colors)
    
    # Convert ggplot to plotly and add tooltips
    plotly_obj <- ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE) %>%
      layout(
        legend = list(
          orientation = "h",
          y = 4,
          x = 0,
          traceorder = "reversed",  # Display legend items in the order they appear in the data
          title = list(text = ""),  # Remove legend title
          font = list(size = 12),  # Set font size for legend items
          tracegroupgap = 0,  # Adjust gap between legend groups
          groupclick = "toggleitem"  # Allow toggling items individually
        ),
        showlegend = TRUE
      )
    
    # Functie om onderdelen juist in legende weer te geven
    for (i in 1:length(plotly_obj$x$data)) {
      if (!is.null(plotly_obj$x$data[[i]]$name)) {
        plotly_obj$x$data[[i]]$name <- gsub("\\(", "", str_split(plotly_obj$x$data[[i]]$name, ",")[[1]][1])
      }
    }
    
    return(plotly_obj)
  }
  
  # In de server
  output$gemiddelde_grafiek_temperatuur <- renderPlotly({
    # Functie om de grafiek voor temperatuur te maken
    plotGemiddelde("Temperatuur", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
  })
  
   output$gemiddelde_grafiek_luchtvochtigheid <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde("Luchtvochtigheid", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
#### Gemiddelde temperatuur in en buiten het dak per fieldlab ####

   plotGemiddelde_per_fieldlab <- function(parameter, fieldlab_name, start_date, end_date, constructie, hoogte, orientatie) {
     
     # Determine the time interval for aggregation based on the time range
     hour_timestamp <- ifelse(difftime(end_date, start_date, units = "days") > 8, "day", "hour")
     # Adjust end_date to include the entire day
     end_date <- as.POSIXct(paste0(end_date, " 23:59:59"))
     
     query_avg_data <- paste0("SELECT 
                DATE_TRUNC('hour', time_stamp) AS hour_timestamp, 
                AVG(value) AS avg_value,
                tag AS tag,
                schimmel
            FROM view_graphs
            WHERE parameter_description = '", parameter, "'
            AND fieldlab_name = '", fieldlab_name, "'
            AND time_stamp >= '", start_date, "'
            AND time_stamp <= '", end_date, "'")
     
     # Voeg constructie IN clausule alleen toe als constructie niet leeg is
     if (length(constructie) > 0) {
       query_avg_data <- paste0(query_avg_data, " AND constructie IN ('", paste(constructie, collapse = "', '"), "')")
     }
     
     # Voeg hoogte IN clausule alleen toe als hoogte niet leeg is
     if (length(hoogte) > 0) {
       query_avg_data <- paste0(query_avg_data, " AND hoogte IN ('", paste(hoogte, collapse = "', '"), "')")
     }
     
     # Voeg orientatie IN clausule alleen toe als orientatie niet leeg is
     if (length(orientatie) > 0) {
       query_avg_data <- paste0(query_avg_data, " AND orientatie IN ('", paste(orientatie, collapse = "', '"), "')")
     }
     
     query_avg_data <- paste0(query_avg_data, "
                              GROUP BY tag, hour_timestamp, schimmel")
     
     # Query data from the database to obtain the actual temperature and humidity for each hour
     query_weather_data <- paste0("
      SELECT 
        DATE_TRUNC('hour', mg.time_stamp) AS hour_timestamp, 
        AVG(mg.act_temp) AS average_temperature,
        AVG(mg.lucht_vocht) AS average_humidity
      FROM meteo_gegevens mg
      WHERE EXISTS (
        SELECT 1
        FROM (", query_avg_data, ") AS avg_subquery
        WHERE DATE_TRUNC('hour', mg.time_stamp) = avg_subquery.hour_timestamp
        AND time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "'
      )
      GROUP BY DATE_TRUNC('hour', mg.time_stamp)
      ")
     
     # Execute the SQL queries to fetch data from the database
     data_avg <- dbGetQuery(con, query_avg_data)
     data_weather <- dbGetQuery(con, query_weather_data)
     
     # Ensure hour_timestamp is correctly formatted
     data_avg$hour_timestamp <- as.POSIXct(data_avg$hour_timestamp)
     data_weather$hour_timestamp <- as.POSIXct(data_weather$hour_timestamp)
     
     # Check for NA values and remove them
     data_avg <- na.omit(data_avg)
     data_weather <- na.omit(data_weather)
     

     y_min_temp <- min(c(data_avg$avg_value, data_weather$average_temperature), na.rm = TRUE)
     y_max_temp <- max(c(data_avg$avg_value, data_weather$average_temperature), na.rm = TRUE)
     
     # Bereken de y-aslimieten voor luchtvochtigheid
     y_min_humidity <- min(c(data_avg$avg_value, data_weather$average_humidity), na.rm = TRUE)
     y_max_humidity <- max(c(data_avg$avg_value, data_weather$average_humidity), na.rm = TRUE)
     
     # Map binary values to labels
     data_avg$schimmel_label <- ifelse(data_avg$schimmel == 1, "Schimmel", "Geen Schimmel")
     
  # Create a ggplot object using the fetched data
  p <- ggplot()
  # Achtergrond kleuren en buiten weerdata lijn
  if (parameter == "Temperatuur") {
    p <- p + 
      geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 10, ymax = 25, fill = "Optimaal"), alpha = 0.2) +  
      geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 35, ymax = 40, fill = "Kritiek"), alpha = 0.2) +  
      geom_line(data = data_weather, aes(x = hour_timestamp, y = average_temperature, color = "Temperatuur buiten"), alpha = 1, linetype = "dashed") + 
      scale_fill_manual(values = c("Optimaal" = "#49F566", "Kritiek" = "#F54966", "Temperatuur buiten" = "#000")) +
      coord_cartesian(ylim = c(y_min_temp, y_max_temp))  # Correcte y-limieten voor temperatuur
  }
  # If the parameter is "Luchtvochtigheid", add humidity line
  else if (parameter == "Luchtvochtigheid") {
    p <- p + 
      geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 70, ymax = 100, fill = "Optimaal"), alpha = 0.2) +  
      geom_ribbon(data = data_avg, aes(x = hour_timestamp, ymin = 0, ymax = 18, fill = "Kritiek"), alpha = 0.2) +
      geom_line(data = data_weather, aes(x = hour_timestamp, y = average_humidity, color = "Luchtvochtigheid buiten"), alpha = 1, linetype = "dashed") +
      scale_fill_manual(values = c("Optimaal" = "#49F566", "Kritiek" = "#F54966","Luchtvochtigheid buiten" = "#000")) +
      coord_cartesian(ylim = c(y_min_humidity, y_max_humidity))  # Correcte y-limieten voor luchtvochtigheid
    
  }
  p <- p +
    geom_line(data = data_avg, aes(x = hour_timestamp, y = avg_value, color = schimmel_label, group = tag, text = paste(" Sensor:", tag, "<br> Datum:", hour_timestamp, "<br>", parameter, round(avg_value, 1)))) +
    labs(x = "Datum", y = parameter, color = "Schimmel") +
    custom_theme()+
    scale_color_manual(values = c("Schimmel" = "#FF8C29", "Geen Schimmel" = "#1789FC")) 
  
  
  
  # Convert ggplot to a plotly object and set layout
  plotly_obj <- ggplotly(p, tooltip = "text") %>%
    config(
      displaylogo = FALSE) %>%
    layout(
      legend = list(
        orientation = "h",
        y = 1.3,
        x = 0,
        traceorder = "reversed",  # Display legend items in the order they appear in the data
        title = list(text = ""),  # Remove legend title
        font = list(size = 12),  # Set font size for legend items
        tracegroupgap = 0  # Adjust gap between legend groups
      )
    )
  
  # Functie om onderdelen juist in legende weer te geven
  for (i in 1:length(plotly_obj$x$data)) {
    if (!is.null(plotly_obj$x$data[[i]]$name)) {
      plotly_obj$x$data[[i]]$name <- gsub("\\(", "", str_split(plotly_obj$x$data[[i]]$name, ",")[[1]][1])
    }
  }
  
  return(plotly_obj)
}
   
   # In de server
   output$gemiddelde_grafiek_temperatuur_havelte <- renderPlotly({
     # Functie om de grafiek voor temperatuur te maken
     plotGemiddelde_per_fieldlab("Temperatuur", "Havelte", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   output$gemiddelde_grafiek_luchtvochtigheid_havelte <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde_per_fieldlab("Luchtvochtigheid", "Havelte", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   # In de server
   output$gemiddelde_grafiek_temperatuur_rotterdam <- renderPlotly({
     # Functie om de grafiek voor temperatuur te maken
     plotGemiddelde_per_fieldlab("Temperatuur", "Rotterdam", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   output$gemiddelde_grafiek_luchtvochtigheid_rotterdam <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde_per_fieldlab("Luchtvochtigheid", "Rotterdam", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   # In de server
   output$gemiddelde_grafiek_temperatuur_schoorl <- renderPlotly({
     # Functie om de grafiek voor temperatuur te maken
     plotGemiddelde_per_fieldlab("Temperatuur", "Schoorl", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   output$gemiddelde_grafiek_luchtvochtigheid_schoorl <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde_per_fieldlab("Luchtvochtigheid", "Schoorl", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   # In de server
   output$gemiddelde_grafiek_temperatuur_Sprundel <- renderPlotly({
     # Functie om de grafiek voor temperatuur te maken
     plotGemiddelde_per_fieldlab("Temperatuur", "Sprundel", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   output$gemiddelde_grafiek_luchtvochtigheid_Sprundel <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde_per_fieldlab("Luchtvochtigheid", "Sprundel", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   # In de server
   output$gemiddelde_grafiek_temperatuur_Wamel <- renderPlotly({
     # Functie om de grafiek voor temperatuur te maken
     plotGemiddelde_per_fieldlab("Temperatuur", "Wamel", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   
   output$gemiddelde_grafiek_luchtvochtigheid_Wamel <- renderPlotly({
     # Functie om de grafiek voor luchtvochtigheid te maken
     plotGemiddelde_per_fieldlab("Luchtvochtigheid", "Wamel", input$datums_monitoring[1], input$datums_monitoring[2], selected_constructie(), selected_hoogte(), selected_orientatie())
   })
   

#### Tabel temperatuur/luchtvochtigheid in en buiten het dak ####
  
  getDataGemiddeldeTabel <- function(start_date, end_date, fieldlab) {
    # Determine the time interval for aggregation based on the time range
    
    # Query to fetch average values for temperature and humidity
    query_avg_data <- paste0("
    SELECT 
      DATE_TRUNC('hour', vg.time_stamp) AS hour_timestamp, 
      ROUND(AVG(CASE WHEN vg.parameter_description = 'Temperatuur' THEN CAST(vg.value AS NUMERIC) END)::numeric, 1) AS avg_temperature,
      ROUND(AVG(CASE WHEN vg.parameter_description = 'Luchtvochtigheid' THEN CAST(vg.value AS NUMERIC) END)::numeric, 1) AS avg_humidity,
      vg.fieldlab_name,
      vg.schimmel as schimmel, 
      vg.tag as sensor
    FROM view_graphs vg
    WHERE vg.time_stamp >= '", start_date, "' AND vg.time_stamp <= '", end_date, "'")
    
    # Voeg voorwaarde voor fieldlab_name toe als fieldlab niet gelijk is aan "Alle fieldlabs"
    if (fieldlab != "Alle fieldlabs") {
      query_avg_data <- paste0(query_avg_data, " AND vg.fieldlab_name = '", fieldlab, "'")
    }
    
    # Completeren van de GROUP BY clausule
    query_avg_data <- paste0(query_avg_data, "
    GROUP BY hour_timestamp, vg.fieldlab_name, vg.schimmel, vg.tag")
    
    # Query to fetch average outside temperature and humidity from meteo_gegevens
    query_weather_data <- paste0("
    SELECT 
      DATE_TRUNC('hour', mg.time_stamp) AS hour_timestamp, 
      ROUND(AVG(mg.act_temp)::numeric, 1) AS avg_outside_temperature,
      ROUND(AVG(mg.lucht_vocht)::numeric, 1) AS avg_outside_humidity
    FROM meteo_gegevens mg
    WHERE EXISTS (
      SELECT 1
      FROM (", query_avg_data, ") AS avg_subquery
      WHERE DATE_TRUNC('hour', mg.time_stamp) = avg_subquery.hour_timestamp
      AND mg.time_stamp >= '", start_date, "' AND mg.time_stamp <= '", end_date, "'
    )
    GROUP BY DATE_TRUNC('hour', mg.time_stamp)
    ")
    
    # Execute the SQL queries to fetch data from the database
    data_avg <- dbGetQuery(con, query_avg_data)
    data_weather <- dbGetQuery(con, query_weather_data)
    
    # Merge data_avg with data_weather based on hour_timestamp
    data <- merge(data_avg, data_weather, by = "hour_timestamp", all.x = TRUE)
    
    # Transform schimmel values to "Ja" or "Nee"
    data$schimmel <- ifelse(data$schimmel == 1, "Ja", "Nee")
    
    # Convert hour_timestamp to POSIXct
    data$hour_timestamp <- as.POSIXct(data$hour_timestamp, format = "%Y-%m-%d %H:%M")
    
    # Format hour_timestamp
    data$hour_timestamp <- format(data$hour_timestamp, "%Y-%m-%d %H:%M")

    # Return the dataset
    return(data)
  }
  
  output$gemiddelde_tabel_temperatuur <- renderDataTable({
    
    # Retrieve data
    data <- getDataGemiddeldeTabel(input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
    
    # Select columns for temperature table
    data_temperature <- data[, c("hour_timestamp", "avg_temperature", "avg_outside_temperature", "fieldlab_name", "schimmel", "sensor")]
    
    # Rename columns
    colnames(data_temperature) <- c("Datum", "Gemiddelde temperatuur dak (°C)", "Gemiddelde temperatuur buiten (°C)", "Fieldlab locatie", "Schimmel", "Sensor")
    # Set the options for the table, including the dom option to customize DOM elements
    datatable(
      data_temperature,
      options = list(
        dom = 't', # Only display the table element
        paging = FALSE, # Disable pagination
        searching = FALSE, # Disable search box
        info = FALSE # Disable information summary
      ), rownames=FALSE
    )
  })
  
  
  ## Download binnen/buiten temperatuur
  output$downloadBinnenBuitenTEMP <- downloadHandler(
    filename = "dak_en_buiten_temp.csv",
    content = function(file) {
      data <- getDataGemiddeldeTabel(input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
      
      # Select columns for temperature CSV
      data_temperature <- data[, c("hour_timestamp", "avg_temperature", "avg_outside_temperature", "fieldlab_name", "schimmel", "sensor")]
      
      # Rename columns
      colnames(data_temperature) <- c("Datum", "Gemiddelde temperatuur dak(°C)", "Gemiddelde temperatuur buiten (°C)", "Fieldlab locatie", "Schimmel", "Sensor")
      
      # Write CSV
      write.csv(data_temperature, file, row.names = FALSE)
    }
  )

  output$gemiddelde_tabel_luchtvochtigheid <- renderDataTable({
    # Retrieve data
    data <- getDataGemiddeldeTabel(input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
    
    # Select columns for humidity table
    data_humidity <- data[, c("hour_timestamp", "avg_humidity", "avg_outside_humidity", "fieldlab_name", "schimmel", "sensor")]
    
    # Rename columns
    colnames(data_humidity) <- c("Datum", "Gemiddelde luchtvochtigheid dak (%)", "Gemiddelde luchtvochtigheid buiten (%)", "Fieldlab locatie", "Schimmel", "Sensor")
    
    # Set the options for the table, including the dom option to customize DOM elements
    datatable(
      data_humidity,
      options = list(
        dom = 't', # Only display the table element
        paging = FALSE, # Disable pagination
        searching = FALSE, # Disable search box
        info = FALSE # Disable information summary
      ), rownames=FALSE
    )
  })
  
  ## Download binnen/buiten luchtvochtigheid
  output$downloadBinnenBuitenLV <- downloadHandler(
    filename = "dak_en_buiten_luchtv.csv",
    content = function(file) {
      data <- getDataGemiddeldeTabel(input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
      
      # Select columns for humidity CSV
      data_humidity <- data[, c("hour_timestamp", "avg_humidity", "avg_outside_humidity", "fieldlab_name", "schimmel", "sensor")]
      
      # Rename columns
      colnames(data_humidity) <- c("Datum", "Gemiddelde luchtvochtigheid dak (%)", "Gemiddelde luchtvochtigheid buiten (%)", "Fieldlab locatie", "Schimmel", "Sensor")
      
      # Write CSV
      write.csv(data_humidity, file, row.names = FALSE)
    }
  )
  
#### Omstandigheden voor schimmel: optimaal ####
  
  getOmstandighedenData <- function(fieldlab) {
    query <- paste0("
  WITH daily_avg AS (
    SELECT 
        DATE_TRUNC('day', time_stamp) AS dag,
        AVG(CASE WHEN parameter_description = 'Temperatuur' THEN value END) AS gemiddelde_temperatuur,
        AVG(CASE WHEN parameter_description = 'Luchtvochtigheid' THEN value END) AS gemiddelde_luchtvochtigheid
    FROM 
        view_graphs 
    WHERE 
        parameter_description IN ('Temperatuur', 'Luchtvochtigheid') AND
        time_stamp >= current_date - interval '1 year' AND 
        time_stamp <= current_date");
    
    # Voeg de WHERE-clausule alleen toe als fieldlab niet 'Alle fieldlabs' is
    if (fieldlab != "Alle fieldlabs") {
      query <- paste0(query, " AND fieldlab_name ='", fieldlab, "'");
    }
    
    query <- paste0(query, "
    GROUP BY 
        DATE_TRUNC('day', time_stamp)
  )
  SELECT 
      CASE 
          WHEN TO_CHAR(dag, 'MM') = '01' THEN 'jan'
          WHEN TO_CHAR(dag, 'MM') = '02' THEN 'feb'
          WHEN TO_CHAR(dag, 'MM') = '03' THEN 'mrt'
          WHEN TO_CHAR(dag, 'MM') = '04' THEN 'apr'
          WHEN TO_CHAR(dag, 'MM') = '05' THEN 'mei'
          WHEN TO_CHAR(dag, 'MM') = '06' THEN 'jun'
          WHEN TO_CHAR(dag, 'MM') = '07' THEN 'jul'
          WHEN TO_CHAR(dag, 'MM') = '08' THEN 'aug'
          WHEN TO_CHAR(dag, 'MM') = '09' THEN 'sep'
          WHEN TO_CHAR(dag, 'MM') = '10' THEN 'okt'
          WHEN TO_CHAR(dag, 'MM') = '11' THEN 'nov'
          WHEN TO_CHAR(dag, 'MM') = '12' THEN 'dec'
      END AS maand_naam,
      to_char(dag, 'YY') as jaar,
      to_char(dag, 'MM') as maand_nummer,
      COUNT(*) AS aantal_dagen
  FROM 
      daily_avg
  WHERE 
      gemiddelde_temperatuur > 10 AND gemiddelde_temperatuur < 25 AND gemiddelde_luchtvochtigheid > 70
  GROUP BY 
      jaar, maand_nummer, maand_naam
  ORDER BY 
      jaar, maand_nummer;
")
    
    # Voer de query uit en haal de resultaten op
    result <- dbGetQuery(con, query)
    
    return(result)
  }
  
  # Render de plot
  output$optimaleOmstandigheden <- renderPlotly({
    
    # Haal de data op
    data <- getOmstandighedenData(fieldlab())
    
    data$maand_naam_jaar <- paste0(data$maand_naam, " '", data$jaar)
    
    # Maak een staafdiagram met ggplot2 en gebruik reorder om op maand_nummer te sorteren
    bar_plot <- ggplot(data, aes(x = reorder(maand_naam_jaar, as.numeric(maand_nummer)), y = aantal_dagen,
                                 text = paste("Maand:", maand_naam_jaar, "<br>Aantal dagen:", aantal_dagen))) +
      geom_bar(stat = "identity", fill = "#1789FC") + 
      labs(x = "Maand", y = "Aantal dagen") +
      custom_theme()
    
    # Render het staafdiagram met plotly
    ggplotly(bar_plot, tooltip = "text") %>%
      config(
        displaylogo = FALSE, 
        modeBarButtonsToRemove = list(
          "sendDataToCloud", "zoom2d", "pan2d",
          "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "lasso2d", "select2d", "toggleSpikelines",
          "resetViews", "toggleHover", "resetViewMap"
        )
      )
  })
  
  
#### Omstandigheden voor schimmel: kritiek ####
  
  getOmstandighedenData2 <- function(fieldlab) {
    
    query <- paste0("
      WITH daily_avg AS (
        SELECT 
            DATE_TRUNC('day', time_stamp) AS dag,
            MAX(CASE WHEN parameter_description = 'Temperatuur' AND value > 35 THEN 1
                     WHEN parameter_description = 'Luchtvochtigheid' AND value < 18 THEN 1
                     ELSE 0 
                END) AS kritieke_omstandigheden
        FROM 
            view_graphs 
        WHERE 
            (parameter_description = 'Temperatuur' OR parameter_description = 'Luchtvochtigheid') AND
            time_stamp >= current_date - interval '1 year' AND 
            time_stamp <= current_date");
    
    # Voeg de WHERE-clausule alleen toe als fieldlab niet 'Alle fieldlabs' is
    if (fieldlab != "Alle fieldlabs") {
      query <- paste0(query, " AND fieldlab_name ='", fieldlab, "'");
    }
    
    query <- paste0(query, "
        GROUP BY 
            DATE_TRUNC('day', time_stamp)
      )
      SELECT 
          CASE 
              WHEN TO_CHAR(dag, 'MM') = '01' THEN 'jan'
              WHEN TO_CHAR(dag, 'MM') = '02' THEN 'feb'
              WHEN TO_CHAR(dag, 'MM') = '03' THEN 'mrt'
              WHEN TO_CHAR(dag, 'MM') = '04' THEN 'apr'
              WHEN TO_CHAR(dag, 'MM') = '05' THEN 'mei'
              WHEN TO_CHAR(dag, 'MM') = '06' THEN 'jun'
              WHEN TO_CHAR(dag, 'MM') = '07' THEN 'jul'
              WHEN TO_CHAR(dag, 'MM') = '08' THEN 'aug'
              WHEN TO_CHAR(dag, 'MM') = '09' THEN 'sep'
              WHEN TO_CHAR(dag, 'MM') = '10' THEN 'okt'
              WHEN TO_CHAR(dag, 'MM') = '11' THEN 'nov'
              WHEN TO_CHAR(dag, 'MM') = '12' THEN 'dec'
          END AS maand_naam,
          to_char(dag, 'YY') as jaar,
          to_char(dag, 'MM') as maand_nummer,
          SUM(kritieke_omstandigheden) AS aantal_dagen
      FROM 
          daily_avg
      GROUP BY 
          jaar, maand_nummer, maand_naam
      ORDER BY 
          jaar, maand_nummer;
    ")
    
    # Voer de query uit en haal de resultaten op
    result2 <- dbGetQuery(con, query)
    
    return(result2)
  }
  
  # Render de plot
  output$kritiekeOmstandigheden <- renderPlotly({
    
    # Haal de data op
    data <- getOmstandighedenData2(fieldlab())
    
    if (nrow(data) == 0) {
      # Maak een leeg dataframe voor de plot
      data <- data.frame(maand_naam_jaar = character(), aantal_dagen = 0)
    } else {
      # Voeg een kolom toe voor de tooltip
      data$maand_naam_jaar <- paste0(data$maand_naam, " '", data$jaar)
    }
    
    # Maak een staafdiagram met ggplot2 en gebruik reorder om op maand_nummer te sorteren
    bar_plot <- ggplot(data, aes(x = reorder(maand_naam_jaar, as.numeric(maand_nummer)), y = aantal_dagen,
                                 text = paste("Maand:", maand_naam_jaar, "<br>Aantal dagen:", aantal_dagen))) +
      geom_bar(stat = "identity", fill = "#1789FC") + 
      labs(x = "Maand", y = "Aantal dagen") +
      custom_theme()
    
    # Render het staafdiagram met plotly
    ggplotly(bar_plot, tooltip = "text") %>%
      config(
        displaylogo = FALSE, 
        modeBarButtonsToRemove = list(
          "sendDataToCloud", "zoom2d", "pan2d",
          "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "lasso2d", "select2d", "toggleSpikelines",
          "resetViews", "toggleHover", "resetViewMap"
        )
      )
  })
  
  # Functie om de juiste staafgrafiek te genereren
  action_button_omstandigheden <- reactiveVal("grafiek_optimaal")
  
  output$omstandigheden_grafiek <- renderUI({
    switch(action_button_omstandigheden(),
           "grafiek_optimaal" = plotlyOutput("optimaleOmstandigheden"),
           "grafiek_kritiek" = plotlyOutput("kritiekeOmstandigheden")
    )
  })
  
  observeEvent(input$grafiek_optimaal, {
    action_button_omstandigheden("grafiek_optimaal")
  })
  
  observeEvent(input$grafiek_kritiek, {
    action_button_omstandigheden("grafiek_kritiek")
  })
  
#### Gemiddelde temperatuur/luchtvochtigheid bij verschillende...####
  
  staafgrafiek <- function(parameter, variabele, x_levels, x_as_titel, start_date, end_date, fieldlab){
    # Definieer de query in een aparte variabele
    if (fieldlab == "Alle fieldlabs") {
      query <- paste0(
        "SELECT ", variabele, ", AVG(value) AS value ",
        "FROM view_graphs ",
        "WHERE parameter_description = '", parameter, "' AND ",
        "time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "' ",
        "GROUP BY ", variabele
      )
    } else {
      query <- paste0(
        "SELECT ", variabele, ", AVG(value) AS value ",
        "FROM view_graphs ",
        "WHERE parameter_description = '", parameter, "' AND ",
        "time_stamp >= '", start_date, "' AND time_stamp <= '", end_date, "' AND ",
        "fieldlab_name = '", fieldlab, "' ",
        "GROUP BY ", variabele
      )
    }

    # Query uitvoeren om de gegevens op te halen uit de view
    data <- dbGetQuery(con, query)
    
    # Converteer variabele naar factor met opgegeven levels
    data[[variabele]] <- factor(data[[variabele]], levels = x_levels)
    
    # Staafdiagram maken met ggplot2
    p <- ggplot(data, aes(x = .data[[variabele]], y = value, text = paste(parameter, ":", round(value, 1)))) +
      geom_bar(stat = "identity", fill = "#1789FC") +
      labs(x = x_as_titel, y = parameter) +
      custom_theme()
    
    # Convert ggplot object to plotly
    plotly_obj <- ggplotly(p, tooltip = "text") %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "sendDataToCloud", "zoom2d", "pan2d",
          "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "lasso2d", "select2d", "toggleSpikelines",
          "resetViews", "toggleHover", "resetViewMap"
        )
      )
    
    return(plotly_obj)
  }
    
    ## Staafgrafiek hoogte ##
    # Reactive functie voor luchtvochtigheid of temperatuur grafiek
    staafgrafiek_hoogte <- reactive({
      
      x_levels <- c("boven", "midden", "onder", "onbekend")
      
      if (input$parameter_staafgrafiek == 1) {
        staafgrafiek("Temperatuur", "hoogte", x_levels, "Dakhoogtes", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
        
      } else if (input$parameter_staafgrafiek == 2) {
        staafgrafiek("Luchtvochtigheid", "hoogte", x_levels, "Dakhoogtes", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
      }
    })
    
    # Render staafgrafiek met gekozen parameter
    output$staafgrafiek_hoogte <- renderPlotly({
      staafgrafiek_hoogte()
    })
    
    ## Staafgrafiek constructie ##
    # Reactive functie voor luchtvochtigheid of temperatuur grafiek
    staafgrafiek_constructie <- reactive({
      
      x_levels <- c("open", "gesloten", "onbekend")
      
      if (input$parameter_staafgrafiek == 1) {
        staafgrafiek("Temperatuur", "constructie", x_levels, "Dakcontstructies", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
        
      } else if (input$parameter_staafgrafiek == 2) {
        staafgrafiek("Luchtvochtigheid", "constructie", x_levels, "Dakconstructies", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
      }
    })
    
    # Render staafgrafiek met gekozen parameter
    output$staafgrafiek_constructie <- renderPlotly({
      staafgrafiek_constructie()
    })
    
    ## Staafgrafiek oriëntatie ##
    # Reactive functie voor luchtvochtigheid of temperatuur grafiek
    staafgrafiek_orientatie <- reactive({
      
      x_levels <- c("schaduwkant", "zonkant", "onbekend")
      
      if (input$parameter_staafgrafiek == 1) {
        staafgrafiek("Temperatuur", "orientatie", x_levels, "Dakoriëntaties", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
        
      } else if (input$parameter_staafgrafiek == 2) {
        staafgrafiek("Luchtvochtigheid", "orientatie", x_levels, "Dakoriëntaties", input$datums_monitoring[1], input$datums_monitoring[2], fieldlab())
      }
    })
    
    # Render staafgrafiek met gekozen parameter
    output$staafgrafiek_orientatie<- renderPlotly({
      staafgrafiek_orientatie()
    })
    
    # Functie om juiste fieldlab te selecteren
    fieldlab <- reactiveVal("Alle fieldlabs")
    
    observeEvent(input$a_alle_fieldlabs, {
      fieldlab("Alle fieldlabs")
    })
    observeEvent(input$a_havelte, {
      fieldlab("Havelte")
    })
    observeEvent(input$a_rotterdam, {
      fieldlab("Rotterdam")
    })
    observeEvent(input$a_schoorl, {
      fieldlab("Schoorl")
    })
    observeEvent(input$a_sprundel, {
      fieldlab("Sprundel")
    })
    observeEvent(input$a_wamel, {
      fieldlab("Wamel")
    })
    
    # Functie om de juiste staafgrafiek te genereren
    action_button_staafgrafiek <- reactiveVal("grafiek_hoogtes")
    
    output$geselecteerde_staafgrafiek <- renderUI({
      switch(action_button_staafgrafiek(),
             "grafiek_hoogtes" = plotlyOutput("staafgrafiek_hoogte"),
             "grafiek_constructies" = plotlyOutput("staafgrafiek_constructie"),
             "grafiek_orientaties" = plotlyOutput("staafgrafiek_orientatie"),
      )
    })
    
    observeEvent(input$grafiek_hoogtes, {
      action_button_staafgrafiek("grafiek_hoogtes")
    })
    
    observeEvent(input$grafiek_constructies, {
      action_button_staafgrafiek("grafiek_constructies")
    })
    
    observeEvent(input$grafiek_orientaties, {
      action_button_staafgrafiek("grafiek_orientaties")
    })
    
#### Bioclim variablelen ####
    
    bioclim <- function(){
      # Fetch data from the database using the view
      query <- "SELECT fieldlab_name,
                   AVG(bio1_t) AS bio1_t_mean,
                   AVG(bio2_t) AS bio2_t_mean,
                   AVG(bio4_t) AS bio4_t_mean,
                   AVG(bio8_t) AS bio8_t_mean,
                   AVG(bio9_t) AS bio9_t_mean,
                   AVG(bio10_t) AS bio10_t_mean,
                   AVG(bio11_t) AS bio11_t_mean,
                   AVG(bio1_h) AS bio1_h_mean,
                   AVG(bio2_h) AS bio2_h_mean,
                   AVG(bio4_h) AS bio4_h_mean,
                   AVG(bio8_h) AS bio8_h_mean,
                   AVG(bio9_h) AS bio9_h_mean,
                   AVG(bio10_h) AS bio10_h_mean,
                   AVG(bio11_h) AS bio11_h_mean
            FROM bioclim_values
            GROUP BY fieldlab_name;"
      bioclim_data <- dbGetQuery(con, query)
      
      # Round the numeric values to one decimal place
      numeric_columns <- names(bioclim_data)[-1]  # Exclude the first column (fieldlab_name)
      bioclim_data[numeric_columns] <- lapply(bioclim_data[numeric_columns], function(x) round(x, 1))
      
      # Save the new variable names
      new_variable_names <- c(
        "Field Lab Name",
        "Gemiddelde temperatuur",
        "Temperatuur bereik (Max temp - min temp)",
        "Gemiddelde afwijking temperatuur (standaardafwijking ×100)",
        "Gemiddelde temperatuur van natste kwartaal (herfst)",
        "Gemiddelde temperatuur van droogste kwartaal (lente)",
        "Gemiddelde temperatuur van warmste kwartaal (zomer)",
        "Gemiddelde temperatuur van koudste kwartaal (winter)",
        "Gemiddelde luchtvochigheid",
        "Luchtvochigheid bereik (Max % - min %)",
        "Gemiddelde afwijking luchtvochigheid (standaardafwijking ×100)",
        "Gemiddelde luchtvochigheid van natste kwartaal (herfst)",
        "Gemiddelde luchtvochigheid van droogste kwartaal (lente)",
        "Gemiddelde luchtvochigheid van warmste kwartaal (zomer)",
        "Gemiddelde luchtvochigheid van koudste kwartaal (winter)"
      )
      
      # Transpose the data
      transposed_data <- t(bioclim_data)
      
      # Convert transposed matrix back to data frame
      transposed_df <- as.data.frame(transposed_data, stringsAsFactors = FALSE)
      
      # Set the column names to the values of the first row (which was fieldlab_name)
      colnames(transposed_df) <- transposed_df[1, ]
      
      # Remove the first row
      transposed_df <- transposed_df[-1, ]
      
      # Add the new variable names as a new column
      transposed_df <- cbind(Fieldlab = new_variable_names[-1], transposed_df)
      
      # Return the transposed data frame with the new variable names included
      return(transposed_df)
    }
    

    output$bioclim_table <- renderDataTable({
      data <- bioclim()
      datatable(
        data, 
        options = list(
          dom = 't', # Only display the table element
          paging = FALSE, # Disable pagination
          searching = FALSE, # Disable search box
          info = FALSE # Disable information summary
        ), rownames=FALSE
      )
    })
     
    
    ### Download bioclim data ##
    output$downloadBioclim <- downloadHandler(
      filename = "bioclim.csv",
      content = function(file) {
        
        data <- bioclim()
        
        write.csv(data, file)
      }
    )
   }
    
 
    
    
    
    
    


  

  


