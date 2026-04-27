importUI <- function(id) {
  htmltools::tagList(
    sidebarPanel(
      fileInput(
        NS(id, "upload"), 
        NULL, 
        accept = c(
          ".csv", ".xlsx", ".xls", ".sas7bdat", ".sav", ".dta", ".rds"
        )
      ),
      selectInput(NS(id, 'sheet'), "Choose Sheet",  NULL),
      actionButton(
        inputId = NS(id, "go"),
        "Upload!",
        icon = shiny::icon("file")
      )
    )
  )
}

importServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # get sheet names
    sheetNames <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      if(ext == 'xls' | ext == "xlsx"){
        readxl::excel_sheets(input$upload$datapath)
      } else {
        "No Sheets"
      }
    })
    
    # update dropdown menu
    observe({
      updateSelectInput(
        session, "sheet", choices = sheetNames()
      )
    })
    
    
    # import data using appropriate function
    eventReactive(input$go, {
      if((!is.null(input$upload)) && (input$sheet != "")){
        ext <- tools::file_ext(input$upload$name)
        # import data using appropriate function based on file type
        switch(
          ext,
          csv = read.csv(input$upload$datapath),
          xls = readxl::read_xls(input$upload$datapath, sheet = input$sheet),
          xlsx = readxl::read_xlsx(input$upload$datapath, sheet = input$sheet), 
          sas7bdat = haven::read_sas(input$upload$datapath),
          sav = haven::read_spss(input$upload$datapath),
          dta = haven::read_dta(input$upload$datapath),
          rds = readRDS(input$upload$datapath),
          validate(paste0("Invalid file; Please upload a file of the following",
                          " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta, .rds"))
        )
      } else if ((!is.null(input$upload))) {
        ext <- tools::file_ext(input$upload$name)
        # import data using appropriate function based on file type
        switch(
          ext,
          csv = read.csv(input$upload$datapath),
          xls = readxl::read_xls(
            input$upload$datapath, 
            sheet = readxl::excel_sheets(input$upload$datapath)[1]
          ),
          xlsx = openxlsx::read.xlsx(
            input$upload$datapath, 
            sheet = readxl::excel_sheets(input$upload$datapath)[1]
          ),
          sas7bdat = haven::read_sas(input$upload$datapath),
          sav = haven::read_spss(input$upload$datapath),
          dta = haven::read_dta(input$upload$datapath),
          rds = readRDS(input$upload$datapath),
          validate(paste0("Invalid file; Please upload a file of the following",
                          " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta, .rds"))
        )
      } else {
        NULL
      }
    },
    ignoreNULL = FALSE)
  })
}
