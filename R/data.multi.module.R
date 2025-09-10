data_multiUI <- function(id) {
  htmltools::tagList(
    mainPanel(
      fluidRow(
        column(
          11,
          p(textOutput(NS(id, "dataInfo")))
        )
      ),
      fluidRow(
        column(
          11,
          DT::dataTableOutput("plot_names")
        )
      )
    )
  )
}

data_multiServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Display message to import data file or summary of data file (nrow, ncol)
    data_info <- reactive({
      if (is.null(data())) {
        text <- paste0(
          "Import your figure files.
          Only R (rds) files are accepted."
        )
      } else if ((!is.null(data())) & any(sapply(data(), function(x) class(x) == "gg"))){
        text <- paste0("The import contains ", length(data()), " {ggplot2} figure object(s).")
      }
      text
    })
    
    output$dataInfo <- renderText({data_info()})
    
    # get path of each figure
    reactive({
      req(data())
      
      data.frame(
        "Figure" = seq(1, length(data()), 1),
        "Names" = names(data())
      )
    })
  })
}
