library(shiny)
library(shinyjs)
library(ggplot2)
library(tibble)
library(png)
library(grid)
library(glue)
library(dplyr)

generate_plot <- function(data) {
  img_grob <- readRDS("score_sheet_grob.RDS")
  labels_raw <- c("N", "E", "O", "U", "S")
  labels <- c(labels_raw, paste0(rep(labels_raw, each = 6), rep(1:6, 5)))

  group_x_offset <- c(213, 680, 1240, 1800, 2360, 2920) # X positions of the first traits in each group (general, N, E, O, U, S)
  y_offset <- 2480 - 2329 # Y position of (1,1)
  x_scale <- 93 # Difference of 1 in X
  y_scale <- 152 # Difference of 1 in Y

  df <- data %>%
    mutate(
      scale = factor(scale, levels = labels)
    ) %>%
    arrange(scale) %>%
    mutate(
      scale = c(1:5, rep(1:6, 5)),
      group = c(rep(1, 5), rep(2:6, each = 6)),
      scale = group_x_offset[group] + (scale - 1) * x_scale,
      sten = y_offset + (sten - 1) * y_scale,
      group = factor(c(rep("general", 5), rep(labels_raw, each = 6)), levels = c("general", labels_raw), ordered = TRUE)
    )

  ggplot(df, aes(x = scale, y = sten, colour = group)) +
    annotation_custom(img_grob, xmin = 0, xmax = 3508, ymin = 0, ymax = 2480) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.5) +
    scale_x_continuous(limits = c(0, 3508), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 2480), expand = c(0, 0)) +
    scale_colour_manual(
      values = c("#ef476f", "#f78c6b", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"),
      guide = "none"
    ) +
    coord_fixed() +
    theme_void()
}

# Helper function to create a row of narrow numeric inputs.
createInputRow <- function(labels) {
  fluidRow(
    column(
      12,
      lapply(labels, function(label) {
        div(
          style = "display: inline-block; margin-right: 10px;",
          numericInput(
            inputId = label,
            label = label,
            value = NULL,
            min = 1,
            max = 10,
            width = "75px" # Input field width changed to 75px
          )
        )
      })
    )
  )
}

# Define the input labels for each row.
row1 <- c("N", "E", "O", "U", "S")
row2 <- paste0("N", 1:6)
row3 <- paste0("E", 1:6)
row4 <- paste0("O", 1:6)
row5 <- paste0("U", 1:6)
row6 <- paste0("S", 1:6)
all_labels <- c(row1, row2, row3, row4, row5, row6)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Profile NEO-PI-R"),
    tags$meta(name = "description", content = "Generator profili osobowości NEO-PI-R z wyników stenowych polskiej wersji testu."),
    tags$meta(property = "og:title", content = "Profile osobowości z testu NEO-PI-R"),
    tags$meta(property = "og:description", content = "Generator profili osobowości NEO-PI-R z wyników stenowych."),
    tags$meta(property = "og:url", content = "https://shiny.jakubjedrusiak.pl/neo-pi-r/"),
    tags$meta(property = "og:image", content = "https://shiny.jakubjedrusiak.pl/neo-pi-r/wykres.png")
  ),
  titlePanel("Generator profili osobowości NEO-PI-R"),
  HTML('<p>
  <a
    href="https://github.com/jakub-jedrusiak/NEO-PI-R"
    title="Go to GitHub repo"
    ><img
      src="https://img.shields.io/static/v1?label=jakub-jedrusiak&amp;message=NEO-PI-R&amp;color=blue&amp;logo=github"
      alt="jakub-jedrusiak - NEO-PI-R"
  /></a>
  <a href="https://creativecommons.org/publicdomain/zero/1.0/"
    ><img src="https://img.shields.io/badge/License-CC0-blue" alt="License"
  /></a>
  <a href="https://github.com/jakub-jedrusiak/NEO-PI-R"
    ><img
      src="https://img.shields.io/github/stars/jakub-jedrusiak/NEO-PI-R?style=social"
      alt="stars - NEO-PI-R"
  /></a>
  <a href="https://github.com/jakub-jedrusiak/NEO-PI-R"
    ><img
      src="https://img.shields.io/github/forks/jakub-jedrusiak/NEO-PI-R?style=social"
      alt="forks - NEO-PI-R"
  /></a>
</p>'),

  # Center the input section in a fixed-width container.
  div(
    style = "width:550px; margin:auto; text-align:center;",
    p("Wprowadź wyniki stenowe:", style = "margin-top: 2em; margin-bottom: 1em"),
    createInputRow(row1),
    createInputRow(row2),
    createInputRow(row3),
    createInputRow(row4),
    createInputRow(row5),
    createInputRow(row6),
    br(),
    # Generate plot button, same width as the input container.
    actionButton("plotButton", "Generuj wykres", width = "100%"),
    br(), br(),
    # Download buttons arranged side-by-side.
    div(
      style = "display:inline-block; width:45%; margin-right:5%;",
      downloadButton("downloadPNG", "Pobierz PNG", style = "width:100%;")
    ),
    div(
      style = "display:inline-block; width:45%;",
      downloadButton("downloadPDF", "Pobierz PDF", style = "width:100%;")
    )
  ),
  br(),
  fluidRow(
    column(
      12,
      div(
        style = "text-align:center;",
        uiOutput("plotContainer")
      )
    )
  ),
  div(
    style = "width:550px; margin:auto; text-align:center;",
    div(
      style = "display:inline-block; width:45%; margin-right:5%;",
      downloadButton("downloadPNGEmpty", "Pobierz pusty PNG", style = "width:100%;")
    ),
    div(
      style = "display:inline-block; width:45%;",
      downloadButton("downloadPDFEmpty", "Pobierz pusty PDF", style = "width:100%;")
    )
  ),
  tags$footer(includeHTML("footer.html"))
)

server <- function(input, output, session) {
  disable("plotButton")

  observe({
    all_filled <- all(sapply(all_labels, function(label) {
      val <- input[[label]]
      !is.null(val) && !is.na(val) && val != ""
    }))

    if (isTRUE(all_filled)) { # This now safely checks for TRUE
      enable("plotButton")
    } else {
      disable("plotButton")
    }
  })


  # Disable download buttons until the plot is generated.
  disable("downloadPNG")
  disable("downloadPDF")

  # Gather input values when "Generuj wykres" is clicked.
  plot_data <- eventReactive(input$plotButton, {
    values <- sapply(all_labels, function(x) input[[x]])
    req(all(!sapply(values, is.null)))
    tibble(scale = all_labels, sten = as.numeric(values))
  })

  output$plotContainer <- renderUI({
    if (input$plotButton == 0) {
      # Before any plot is generated, show the default image from the www folder.
      tags$img(
        src = "wykres.png",
        style = "max-width: 100%; height: auto; max-height: 600px;",
        alt = "Wykres domyślny"
      )
    } else {
      # Once a plot is generated, display the generated plot.
      tags$img(
        src = base64enc::dataURI(file = plotFile(), mime = "image/png"),
        style = "max-width: 100%; height: auto; max-height: 600px;",
        alt = "Wygenerowany wykres"
      )
    }
  })


  # Create the PNG file with fixed resolution (3508x2480) when the plot is generated.
  plotFile <- eventReactive(input$plotButton, {
    req(plot_data())
    tmpFile <- tempfile(fileext = ".png")
    ggsave(
      filename = tmpFile,
      plot = generate_plot(plot_data()),
      width = 3508, height = 2480, units = "px"
    )
    tmpFile
  })

  # Enable download buttons once the plot is generated.
  observeEvent(plotFile(), {
    enable("downloadPNG")
    enable("downloadPDF")
  })

  # Render the image while preserving its aspect ratio.
  output$plot <- renderImage(
    {
      req(plotFile())
      clientWidth <- session$clientData$output_plot_width
      if (is.null(clientWidth)) clientWidth <- 800 # Fallback value
      naturalHeight <- clientWidth * (2480 / 3508)
      if (naturalHeight > 600) {
        imgHeight <- 600
        imgWidth <- 600 / (2480 / 3508)
      } else {
        imgHeight <- naturalHeight
        imgWidth <- clientWidth
      }
      list(
        src = plotFile(),
        contentType = "image/png",
        width = imgWidth,
        height = imgHeight,
        alt = "Wykres"
      )
    },
    deleteFile = FALSE
  )

  # Download handler for PNG export.
  output$downloadPNG <- downloadHandler(
    filename = function() {
      "wykres.png"
    },
    content = function(file) {
      req(plotFile())
      file.copy(plotFile(), file)
    }
  )

  # Download handler for PDF export.
  output$downloadPDF <- downloadHandler(
    filename = function() {
      "wykres.pdf"
    },
    content = function(file) {
      req(plot_data())
      pdf_file <- tempfile(fileext = ".pdf")
      ggsave(pdf_file,
        plot = generate_plot(plot_data()),
        width = 3508 / 300, height = 2480 / 300, dpi = 300
      )
      file.copy(pdf_file, file)
    }
  )

  output$downloadPNGEmpty <- downloadHandler(
    filename = function() {
      "score_sheet.png"
    },
    content = function(file) {
      file.copy("www/score_sheet.png", file)
    },
    contentType = "image/png"
  )

  output$downloadPDFEmpty <- downloadHandler(
    filename = function() {
      "score_sheet.pdf"
    },
    content = function(file) {
      file.copy("www/score_sheet.pdf", file)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
