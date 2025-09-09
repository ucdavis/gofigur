multiplotUI <- function(id) {
  tabPanel(
    "Multi-panel Plot",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          NS(id, "theme"),
          "Plot Theme",
          choices = c(
            "Default",
            "Black and White",
            "Classic",
            "Half Open",
            "Horizontal Grid",
            "Vertical Grid"
          ),
          selected = "Default"
        ),
        numericInput(
          NS(id, "n_rows"),
          "Number of Rows",
          value = 1,
          min = 1
        ),
        numericInput(
          NS(id, "n_cols"),
          "Number of Columns",
          value = 1,
          min = 1
        ),
        textInput(
          NS(id, "panel_labels"),
          label = "Panel labels ('AUTO', 'auto', or user defined separate by '|')",
          value = "AUTO"
        )
      ),
      mainPanel(
        plotOutput(NS(id, "mplot")),
        downloadUI("mplot")
      )
    )
  )
}

multiplotServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # update n_rows and n_cols to have max = length(data)
    observeEvent(data(), {
      updateNumericInput(
        session,
        "n_rows",
        value = ceiling(length(data()) / 2),
        max = length(data())
      )
    })
    
    observeEvent(data(), {
      updateNumericInput(
        session,
        "n_cols",
        value = ceiling(length(data()) / 2),
        max = length(data())
      )
    })
    
    # process theme
    user_theme <- reactive({
      switch(
        input$theme,
        "Default" = ggplot2::theme(),
        "Black and White" = ggplot2::theme_bw(),
        "Classic" = ggplot2::theme_classic(),
        "Half Open" = cowplot::theme_half_open(),
        "Horizontal Grid" = cowplot::theme_minimal_hgrid(),
        "Vertical Grid" = cowplot::theme_minimal_vgrid()
      )
    })
    
    # process panel_labels
    labels <- reactive({
      ifelse(
        grepl("\\|", input$panel_labels),
        stringr::str_split(input$panels, "\\|"),
        input$panel_labels
      )
    })
    
    # apply theme
    plot_data <- reactive({
      lapply(data(), function(x) x + user_theme())
    })
    
    # plot
    plot <- reactive({
      cowplot::plot_grid(
        plotlist = plot_data(),
        nrow = input$n_rows,
        ncol = input$n_cols,
        labels = labels(),
        align = "hv"
      )
    })
    
    output$mplot <- renderPlot({plot()})
    
    # download handler
    opts <- reactive({
      if(input$plot_device == "rds") {
        list(
          textInput(
            inputId = NS(id, "plot_name"),
            label = "Figure File Name:",
            value = "figure-name"
          )
        )
      } else {
        list(
          textInput(
            inputId = NS(id, "plot_name"),
            label = "Figure File Name:",
            value = "figure-name"
          ),
          numericInput(
            inputId = NS(id, "plot_height"),
            label = "Figure Height:",
            value = 6,
            min = 1
          ),
          numericInput(
            inputId = NS(id, "plot_width"),
            label = "Figure Width:",
            value = 6,
            min = 1
          ),
          numericInput(
            inputId = NS(id, "plot_dpi"),
            label = "Figure Dots per Inch (DPI):",
            value = 300,
            min = 1,
            max = 600
          )
        )
      }
    })
    
    output$downloadOpts <- renderUI(opts())
    
    output$downloadPlot <- downloadHandler(
      filename = function(file) {
        paste(input$plot_name, input$plot_device, sep = ".")
      },
      content = function(file) {
        if (input$plot_device == "rds") {
          saveRDS(object = plot(), file)
        } else {
          cowplot::ggsave2(
            file,
            ,
            plot = plot(),
            width = input$plot_width,
            height = input$plot_height,
            dpi = input$plot_dpi,
            device = input$plot_device
          )
        }
      }
    )
  })
}

