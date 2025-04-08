#-------------------------#
# Libraries               #
#-------------------------#
# Load libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(colorspace)
library(shiny)
library(shinyjs)



#-----------------------------#
# Color Palette List          #
#-----------------------------#

# Define color palettes for different branches
ecampus_palette <- list(
  "USG-eCore" = list(
    "Official Colors" = c("eCore_Green" = "#7BA640", "eCore_Teal" = "#00A9A6", "eCore_Callout_Green" = "#C9D57F", "eCore_Callout_Teal" = "#B4E8E5"),
    "Secondary Colors" = c("eCore_Class_is_Greener" = "#7DAA45", "eCore_OER_Orange" = "#F6931E", "eCore_Professor_Plum" = "#8F3776", "eCore_Transferable_Teal" = "#03A59D", "eCore_Accredited_Gray" = "#555755", "eCore_Profread_Red" = "#CA2027", "eCore_Success_Team_Turquoise" = "#4CC2C5","eCore_Banner_Blue" = "#21387D","eCore_Deep_Thinking_Teal" = "#0F7F86"),
    "Neutrals" = c("eCore_Grateful_Gray" = "#848484", "eCore_Balanced_Blue" = "#DDF1EB", "eCore_Confident_Gray" = "#CFCFCF", "eCore_Growth_Green" = "#E7F1BD")
  ),
  "USG-eMajor" = list(
    "Official Colors" = c("eMajor_Navy" = "#0D4860", "eMajor_Callout_Navy" = "#122550", "eMajor_Gray" = "#54554C", "eMajor_Teal" = "#38A3B1"),
    "Secondary Colors" = c("eMajor_Transcript_Review_Teal" = "#0F7F86", "eMajor_Ready_for_Graduation" = "#AA1E23", "eMajor_Pedagogy_Purple" = "#8F3776", "eMajor_Leadership_Lavender" = "#776780", "eMajor_Collaborative_Teal" = "#5EAFB4"),
    "Neutrals" = c("eMajor_Grateful_Gray" = "#848484", "eCore_Balanced_Blue" = "#DDF1EB", "eMajor_Oats_of_Opportunity" = "#C4B5AB")
  ),
  "USG-freeCampus" = list(
    "Official Colors" = c("FreeCampus_Teal" = "#0F7F86", "FreeCampus_Red" = "#A22422", "FreeCampus_Dark_Teal" = "#004F51"),
    "Neutrals" = c("FreeCampus_Grateful_Gray" = "#848484", "FreeCampus_Balanced_Blue" = "#DDF1EB", "FreeCampus_Oats_of_Opportunity" = "#C4B5AB")
  ),
  "USG-eCampus" = list(
    "Official Colors" = c("eCampus_Teal" = "#6DC4B5", "eCampus_Red" = "#CA202A", "eCampus_Callout_Teal" = "#0F7F86", "eCampus_Callout_Red" = "#A22422"),
    "Secondary Colors" = c("eCampus_Bright_Future_Yellow" = "#EBC647", "eCampus_Online-Learning_Orange" = "#E87600", "eCampus_Study_Group_Gray" = "#898989", "eCampus_Pedadogy_Purple" = "#662362"),
    "Neutrals" = c("eCampus_Grateful_Gray" = "#848484", "eCampus_Balanced_Blue" = "#DDF1EB", "eCampus_Oats_of_Opportunity" = "#C4B5AB")
  )
)



#---------------------------------------#
# Shiny Body                            #
#---------------------------------------#
# UI
ui <- fluidPage(
  titlePanel("eCampus Enhanced Gradient Generator"),
  useShinyjs(),

  sidebarLayout(
    sidebarPanel(
      selectInput("palette", "Choose a Palette:", choices = names(ecampus_palette)),
      uiOutput("category_ui"),
      uiOutput("colors_ui"),

      selectInput("gradient_type", "Gradient Type:", choices = c("Linear", "Radial", "Conic", "Linear-Repeating", "Radial-Repeating", "Conic-Repeating")),

      sliderInput("angle", "Gradient Angle:", min = 0, max = 360, value = 0, step = 5),
      selectInput("direction", "Interpolation Direction:", choices = c("To Right", "To Left", "To Top", "To Bottom")),
      sliderInput("precision", "Gradient Precision:", min = 10, max = 200, value = 100),

      selectInput("interpolation_type", "Interpolation Type:", choices = c("RGB Blend", "Perceptual HCL", "Smooth Interpolation")),
      selectInput("ease_function", "Ease Function:", choices = c("Linear", "Ease-In", "Ease-Out")),

      downloadButton("download_gradient", "Export Gradient as CSS"),
      actionButton("copy_css", "Copy CSS"),
      actionButton("copy_url", "Copy URL Gradient"),
      downloadButton("download_svg", "Export as SVG")
    ),

    mainPanel(
      div(id = "preview_element", style = "width:100%;height:300px;"),
      textOutput("gradient_css"),
      plotlyOutput("gradient_plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  output$category_ui <- renderUI({
    req(input$palette)
    selectInput("category", "Choose a Category:", choices = names(ecampus_palette[[input$palette]]))
  })

  output$colors_ui <- renderUI({
    req(input$palette, input$category)
    selectizeInput("selected_colors", "Select Colors:",
                   choices = ecampus_palette[[input$palette]][[input$category]],
                   multiple = TRUE, options = list(maxItems = 5))
  })

  gradient_css <- reactiveVal("")

  observe({
    req(input$selected_colors)

    colors <- paste(input$selected_colors, collapse = ", ")
    direction <- switch(input$direction,
                        "To Right" = "to right",
                        "To Left" = "to left",
                        "To Top" = "to top",
                        "To Bottom" = "to bottom")

    angle <- paste0(input$angle, "deg")

    css_gradient <- switch(input$gradient_type,
                           "Linear" = paste0("linear-gradient(", angle, ", ", colors, ")"),
                           "Radial" = paste0("radial-gradient(circle, ", colors, ")"),
                           "Conic" = paste0("conic-gradient(", colors, ")"),
                           "Linear-Repeating" = paste0("repeating-linear-gradient(", angle, ", ", colors, ")"),
                           "Radial-Repeating" = paste0("repeating-radial-gradient(circle, ", colors, ")"),
                           "Conic-Repeating" = paste0("repeating-conic-gradient(", colors, ")"))

    gradient_css(css_gradient)
  })

  observeEvent(gradient_css(), {
    shinyjs::runjs(sprintf(
      'document.getElementById("preview_element").style.backgroundImage = "%s";',
      gradient_css()
    ))
  })

  output$gradient_css <- renderText({
    paste("CSS Gradient:", gradient_css())
  })

  output$download_gradient <- downloadHandler(
    filename = function() {
      paste("gradient-", Sys.Date(), ".css", sep = "")
    },

    content = function(file) {
      req(input$selected_colors)
      css_gradient <- gradient_css()
      writeLines(css_gradient, file)
    }
  )

  observeEvent(input$copy_css, {
    runjs(sprintf(
      "navigator.clipboard.writeText(`%s`).then(() => {
       Shiny.setInputValue('copy_success', 'css', {priority: 'event'});
     }).catch(err => {
       Shiny.setInputValue('copy_failure', 'css', {priority: 'event'});
     });",
      gradient_css()
    ))
  })


  observeEvent(input$copy_url, {
    req(input$selected_colors)
    url_gradient <- paste0(
      "https://example.com/gradient?colors=",
      URLencode(paste(input$selected_colors, collapse = ","))
    )

    runjs(sprintf(
      "navigator.clipboard.writeText(`%s`).then(() => {
       Shiny.setInputValue('copy_success', 'url', {priority: 'event'});
     }).catch(err => {
       Shiny.setInputValue('copy_failure', 'url', {priority: 'event'});
     });",
      url_gradient
    ))
  })


  output$download_svg <- downloadHandler(
    filename = "gradient.svg",
    content = function(file) {
      req(input$selected_colors)
      svg_content <- paste(
        '<svg width="100%" height="100" xmlns="http://www.w3.org/2000/svg">',
        '<defs>',
        '<linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">',
        paste0('<stop offset="0%" style="stop-color:', input$selected_colors[1], ';stop-opacity:1" />'),
        paste0('<stop offset="100%" style="stop-color:', input$selected_colors[length(input$selected_colors)], ';stop-opacity:1" />'),
        '</linearGradient>',
        '</defs>',
        '<rect width="100%" height="100" fill="url(#grad1)" />',
        '</svg>'
      )
      writeLines(svg_content, file)
    }
  )

  output$gradient_plot <- renderPlotly({
    req(input$selected_colors)

    colors <- input$selected_colors
    if (length(colors) < 2) {
      showNotification("Select at least 2 colors to generate a gradient.", type = "error")
      return(NULL)
    }

    precision <- input$precision
    gradient_colors <- switch(input$interpolation_type,
                              "RGB Blend" = colorRampPalette(colors, space = "rgb")(precision),
                              "Perceptual HCL" = {
                                rgb_colors <- col2rgb(colors) / 255
                                hcl_colors <- as(colorspace::RGB(t(rgb_colors)), "polarLUV")
                                sequential_hcl(
                                  n = precision,
                                  h = mean(hcl_colors@coords[, "H"], na.rm = TRUE),
                                  c = mean(hcl_colors@coords[, "C"], na.rm = TRUE),
                                  l = seq(min(hcl_colors@coords[, "L"], na.rm = TRUE), max(hcl_colors@coords[, "L"], na.rm = TRUE), length.out = precision)
                                )
                              },
                              "Smooth Interpolation" = {
                                smooth_palette <- colorRampPalette(colors)
                                smooth_palette(precision)
                              }
    )

    df <- data.frame(x = 1:precision, y = rep(1, precision), color = gradient_colors)

    p <- ggplot(df, aes(x = x, y = y, fill = color, text = paste("Color: ", color))) +
      geom_tile(color = NA) +
      scale_fill_identity() +
      theme_void() +
      theme(panel.grid = element_blank(),
            legend.position = "none",
            plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.border = element_blank()) +
      labs(title = paste("Gradient Method:", input$gradient_type))

    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
}

# Run the app
shinyApp(ui, server)
