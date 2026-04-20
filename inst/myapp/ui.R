# ui #
library(shiny)
library(dplyr)
library(tibble)
library(readxl)
library(shinythemes)
library(ggplot2)
library(ggtext)

colorblind_palette <- c(
  "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
  "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
)

options(
  ggplot2.discrete.fill = colorblind_palette,
  ggplot2.discrete.colour = colorblind_palette
)

ui <- navbarPage(
  # set language
  lang = "en",
  # replace title with logo on navbar
  title = div(
    tags$img(
      src="Go FiguR.png",
      alt = "GoFiguR Logo",
      width = "90px", height = "60px"
    ),
    alt = "GoFiguR Logo"
  ),
  # title in browser tab
  windowTitle = "GoFiguR",
  # adjust dimensions of navbar to accommodate logo
  header = tagList(
    # add favicon to browser tab
    tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
    fresh::use_theme(
      fresh::create_theme(
        # theme = "cerulean",
        fresh::bs_vars_navbar(
          height = "120px",
          margin_bottom = "15px",
          padding_vertical = "15px",
          default_bg = "#022851",
          default_color = "#FFFFFF",
          default_link_active_bg = "#022851",
          default_link_active_color = "#FFBF00",
          default_link_color = "#FFFFFF",
          default_link_hover_color = "#FFDC00"
        ),
        fresh::bs_vars_font(
          size_base = "20px",
          size_h1 = "28px",
          size_h3 = "28px"
        ),
        fresh::bs_vars_tabs(
          active_link_hover_bg = "#022851",
          active_link_hover_color = "#FFFFFF",
          link_hover_border_color = "#008EAA"
        ),
        fresh::bs_vars_color(
          brand_primary = "#022851"
        )
      )
    )
  ),
  # theme = shinythemes::shinytheme(theme = "cyborg"),
  
  # landing page
  tabPanel(
    "Intro",
    fluidPage(
      fluidRow(
        h1("Purpose"),
        
        p("The purpose of GoFiguR is to assist in the creation and",
          " modification of figures without requiring coding knowledge."),
        
        tags$ul(
          tags$li(
            "Create figures such a histograms, boxplots, or Kaplan-Meier curves",
            " from uploaded data."
          ),
          tags$li(
            "Modify an existing {ggplot2} figure by uploading a RDS file."
          )
        ),
        
        p("Final figures can be exported to:"),
        
        tags$ul(
          tags$li("jpeg/png/pdf/tiff for use in manuscripts or posters, or"),
          tags$li("RDS for additional modification in R.")
        ),
        
        h1("Instructions"),
        column(
          width = 6,
          tags$ul(
            tags$li("To import your data go to the ", tags$b("Data Import"),
                    " tab and click ", tags$b("Browse"),
                    " to select your data file. If your data is stored",
                    " in an Excel file you can select the sheet to import from",
                    " the ", tags$b("Sheet")," dropdown."),
            tags$li("Click the ", tags$b('Upload!'),
                    " button after you have selected the",
                    " file/sheet that you want to import."),
            tags$li("Once your data is imported, a summary sentence and table ",
                    "will appear. The sentence summarizes the number of records",
                    " and variables in the data file. The table shows each ",
                    "variable in the data file and their class (",
                    "how the data are stored). Check that the variables are stored",
                    " as expected."),
            tags$li("Select the ", tags$b("Figure"), "tab and then select the", 
                    " tab corresponding to the type  of figure you'd like to ",
                    "make."),
            tags$li("Once you have selected the figure type, select the ", 
                    "appropriate variables from the drop down menus and adjust",
                    " the axis titles and font sizes as necessary."),
            tags$li("When you are happy with the figure's appearance specify",
                    " the filename, file type, set the dimensions, ",
                    "specify the resolution (dpi), and hit download!"),
            tags$li(tags$b("Note: this may be an iterative process!"))
          )
        )
      )
    )
  ),
  
  # data import
  tabPanel(
    "Data Import",
    gofigur:::importUI("import"),
    gofigur:::dataUI("data")
  ),
  # Figure tabset panel for each supported figure type
  tabPanel(
    "Figure",
    tabsetPanel(
      id = "Figure Type",
      gofigur:::histogramUI("hist"),
      gofigur:::scatterUI("scatter"),
      gofigur:::boxUI("box"),
      gofigur:::tntUI("tnt"),
      gofigur:::barUI("bar"),
      gofigur:::kmUI("km"),
      gofigur:::providedUI("provided")
    )
  )
)