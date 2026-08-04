scatterUI <- function(id) {
  tabPanel(
    "Scatterplot",
    sidebarLayout(
      sidebarPanel(
        select_x.var.input(NS(id, "x.var")),
        select_y.var.input(NS(id, "y.var")),
        select_by.var.input(NS(id, "by.var")),
        labels_and_fonts("scatter", by = TRUE)
      ),
      mainPanel(
        plotOutput(NS(id, "scatter")),
        downloadUI("scatter")
      )
    )
  )
}

scatterServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select variables
    x_var <- select_x.var.server(
      "x.var", data_class, c("numeric", "Date", "POSIXct", "factor")
    )
    y_var <- select_y.var.server("y.var", data_class, "numeric")
    by_var <- select_by.var.server("by.var", data_class)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(x_var()), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", paste(y_var()), input$y_lab)
    })
    by_label <- reactive({
      ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    })
    
    # create tmp data for plot
    plot_data <- reactive({
      shiny::req(data())
      shiny::req(nrow(data()) > 0)
      shiny::req(all(class(data()) != "gg"))
      
      data() |> 
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::any_of(c(by_var())),
            ~ .x |> as.factor()
          )
        )
    })
    
    # conditional aes
    gg_aes <- reactive({
      if (by_var() != "No group") {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]],
          color = .data[[by_var()]]
        )
      } else {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]]
        )
      }
    })

    
    # plot
    plot <- reactive({
      plot_data() |> 
        ggplot2::ggplot() +
        gg_aes() +
        ggplot2::geom_point() +
        ggplot2::labs(
          x = x_label(),
          y = y_label(),
          color = by_label()
        ) +
        ggplot2::theme_bw() +
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
    })
    
    output$scatter <- renderPlot({plot()})
    
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

