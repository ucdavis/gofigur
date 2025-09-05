import_multiUI <- function(id) {
  htmltools::tagList(
    fluidRow(
      "Import Multiple Figures",
      column(width = 1),
      column(
        width = 4,
        fileInput(
          NS(id, "upload"), 
          multiple = TRUE,
          NULL, 
          accept = c(
            ".rds"
          )
        ),
        actionButton(
          inputId = NS(id, "go"),
          "Upload!",
          icon = shiny::icon("file")
        )
      )
    )
  )
}

import_multiServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # import data using appropriate function
    eventReactive(input$go, {
      shiny::req(input$upload)
      # Store files
      plot_files <- input$upload
      
      # Import each file
      import_list <- lapply(
        plot_files$datapath,
        function(x) readRDS(x)
      )
      
      import_list
    })
  })
}
