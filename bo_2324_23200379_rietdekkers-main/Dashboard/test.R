library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Table with Download Buttons"),
  DTOutput("table")
)

# Define server logic
server <- function(input, output, session) {
  # Create a sample data frame
  data <- data.frame(
    Name = c("File 1", "File 2", "File 3"),
    stringsAsFactors = FALSE
  )
  
  # Add download button HTML to the data frame
  data$Download <- paste0(
    '<a href="', c("www/metadataformulier_schoorl.pdf", "www/metadataformulier_sprundel.pdf", "www/metadataformulier_wamel.pdf"),
    '" download><button>Download</button></a>'
  )
  
  # Render the DataTable
  output$table <- renderDT({
    datatable(data, escape = FALSE, options = list(
      columnDefs = list(list(className = 'dt-center', targets = 1))
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
