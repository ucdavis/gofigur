dataUI <- function(id) {
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
          DT::dataTableOutput("info")
        )
      )
    )
  )
}

dataServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Display message to import data file or summary of data file (nrow, ncol)
    data_info <- reactive({
      if (is.null(data())) {
        text <- paste0(
          "Import your data file.
          Accepted file types are xls, xlsx",
          ", csv, R (rds), SAS (sas7bdat), Stata (dta), or SPSS (sav)."
        )
      } else if ((!is.null(data())) & any(class(data()) == "gg")){
        text <- paste0("The import is a {ggplot2} figure object.")
      } else if ((!is.null(data())) & nrow(data()) == 0 & all(class(data()) != "gg")) {
        text <- paste0(
          "The imported file is empty. Please choose another file/sheet."
        )
      } else if ((!is.null(data())) & all(class(data()) != "gg")) {
          text <- paste0(
            "The data has ", nrow(data()), " rows and ", ncol(data()),
            " columns. The variable types are displayed below.",
            " Please review each variable and check that its class",
            " (numeric or character) is as expected."
          )
      }
      
      text
    })
    
    output$dataInfo <- renderText({data_info()})
    
    # get class of each variable
    reactive({
      req(data())
      
      if ((!is.null(data())) & any(class(data()) == "gg")) {
        data.frame(
          "Variable" = "import",
          "Class" = "ggplot object"
        )
      } else if ((!is.null(data())) & all(class(data()) != "gg") & nrow(data()) > 0) {
        data.frame(
          "Variable" = colnames(data()),
          "Class" = sapply(1:ncol(data()), function(x){class(data()[[x]])[1]})
        )
      }
    })
  })
}
