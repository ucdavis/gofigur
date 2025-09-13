import_multiUI <- function(id) {
  htmltools::tagList(
    sidebarPanel(
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
}

import_multiServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # import data using appropriate function
    eventReactive(input$go, {
      if (!is.null(input$upload)) {
        # Store files
        plot_files <- input$upload
        
        # Import each file
        import_list <- lapply(
          plot_files$datapath,
          function(x) readRDS(x)
        )
        names(import_list) <- plot_files$name
        import_list
      } else {
        NULL
      }
    },
    ignoreNULL = FALSE)
  })
}
