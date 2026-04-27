providedUI <- function(id) {
  tabPanel(
    "Provided",
    sidebarLayout(
      sidebarPanel(
        uiOutput(NS(id, "label_ui")),
        fonts("provided", by = TRUE),
        selectInput(
          NS(id, "theme"),
          label = "Plot Theme",
          choices = c("Default", "Black and White", "Classic"),
          selected = NULL
        )
      ),
      mainPanel(
        plotOutput(NS(id, "provided")),
        downloadUI("provided")
      )
    )
  )
}

providedServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # extract labels to generate appropriate 'Label' UI fields
    map_labels <- reactive({
      shiny::req(data())
      
      if (any(class(data()) == "S7_object")) {
        data()@mapping
      } else {
        data()$labels
      }
    })
    
    map_labels_names <- reactive({
      shiny::req(map_labels())
      
      map_labels() |> names()
    })
    
    # generate UI
    output$label_ui <- renderUI({
      purrr::map(
        map_labels_names(),
        function(x) {
          textInput(
            inputId = NS(id, x),
            label = paste(Hmisc::capitalize(x), "Label", sep = " "),
            value = ifelse(
              any(class(data()) == "S7_object"),
              map_labels()[[x]] |> rlang::quo_get_expr(),
              map_labels()[[x]]
            )
          )
        }
      )
    })
    
    # extract new Labels
    new_labels <- reactive({
      tmp <- purrr::map(
        map_labels_names(),
        function(x) input[[x]]
      )

      names(tmp) <- map_labels_names()

      tmp
    })
    
    # update the 'labels' list in the plot data
    plot_data <- reactive({
      shiny::req(data())
      
      tmp_data <- data()
      
      if (!(any(class(data()) == "S7_object"))) {
        tmp_data$labels <- new_labels()
      }
      
      tmp_data
    })
    
    # process theme
    user_theme <- reactive({
      if (input$theme == "Default") {
        ggplot2::theme()
      } else if (input$theme == "Black and White") {
        ggplot2::theme_bw()
      } else if (input$theme == "Classic") {
        ggplot2::theme_classic()
      }
    })
    
    # plot
    plot <- reactive({
      if (any(class(data()) == "gg")) {
        p <- plot_data() +
          user_theme() +
          ggplot2::theme(
            # x-axis
            axis.title.x = ggtext::element_markdown(
              size = input$x_lab_size
            ),
            axis.text.x = ggtext::element_markdown(
              size = input$x_text_size
            ),
            # y-axis
            axis.title.y = ggtext::element_markdown(
              size = input$y_lab_size
            ),
            axis.text.y = ggtext::element_markdown(
              size = input$y_text_size
            ),
            # legend
            legend.title = ggtext::element_markdown(
              size = input$by_lab_size
            ),
            legend.text = ggtext::element_markdown(
              size = input$by_text_size
            )
          )
        if (any(class(data()) == "S7_object")) {
          # update labels
          ggplot2::update_labels(p, labels = new_labels())
        }
        
      } else {
        ggplot2::ggplot() +
          ggplot2::aes(
            x = 1,
            y = 1,
            label = "Upload a saved {ggplot2} figure to utilize this panel"
          ) +
          ggplot2::geom_text(size = 6) +
          ggplot2::theme_void()
      }
    })

    output$provided <- renderPlot({print(plot())})

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
          ggplot2::ggsave(
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