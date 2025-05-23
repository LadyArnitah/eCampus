eCampus Colors Guide
================
Enita Omuvwie
2025-04-08

<!-- README.md is generated from README.Rmd. Please edit that file -->

# eCampus <img src="https://raw.githubusercontent.com/LadyArnitah/eCampus/master/man/figures/ecampus.png" align="right" height="300"/>

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Made with
R](https://img.shields.io/badge/Made%20with-R-1f425f.svg?logo=R)](https://www.r-project.org/)

The eCampus package provides a collection of color palettes and tools
for working with the eCampus brand colors.

It demonstrates how to access the color palettes, visualize them, and
generate gradients using the eCampus colors.

The eCampus package contains color palettes for R inspired by the
[eCampus](https://ecampus.usg.edu/about/style-guides.php).

- [Installation](#installation)
- [Palette Table](#palette-table)
- [Palettes](#palettes)
- [Package Functions](#package-functions)
- [USG Institutions](#usg-institutions)
- [eCampus Hues](#ecampus-hues)
- [eCampus Enhanced Gradient Generator](#ecampus-enhanced-gradient-generator)
- [Notes](#notes)

## Installation

You can install the development version of ecampus from
[GitHub](https://github.com/) with:

``` r
# If you don't have devtools installed, uncomment the following line to install it:
# install.packages("devtools")

# Install the eCampus package from GitHub
devtools::install_github("LadyArnitah/eCampus")
```

## Palette Table

The ecampus package includes color palettes for different branches of
the eCampus brand. The palettes are organized into four main categories.
Each category contains a list of colors with their corresponding hex
codes. You can view the interactive eCampus Color Guide  **[View Interactive Plot](https://LadyArnitah.github.io/eCampus/color_table.html)**

<img src="https://raw.githubusercontent.com/LadyArnitah/eCampus/master/man/figures/final_USG-merged.png"/>

The code to reproduce the interactive table is below:

``` r
# Load libraries
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(odbc)
library(maps)
library(colorspace)
library(shiny)
library(htmlwidgets)
library(shinyjs)
```

``` r
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
```

``` r
# Function to generate palette table data
get_palette_data <- function() {
  palette_list <- lapply(names(ecampus_palette), function(branch) {
    lapply(names(ecampus_palette[[branch]]), function(type) {
      colors <- ecampus_palette[[branch]][[type]]
      data.frame(
        Branch = branch,
        Type = type,
        Color = names(colors),
        Hex = unname(colors),
        stringsAsFactors = FALSE
      )
    }) 
  })
  
  do.call(rbind, unlist(palette_list, recursive = FALSE))
}

# Combine the data for all branches and types
palette_data <- get_palette_data()
```

``` r
# Add Swatch column for color visualization
palette_data <- palette_data %>%
  mutate(Swatch = paste0('<div style="width: 50px; height: 20px; background-color:', Hex, ';"></div>'))

# Display using DT
datatable(
  palette_data, 
  escape = FALSE, 
  rownames = FALSE,
  options = list(
    dom = 't', 
    pageLength = -1  # Set to -1 to display all rows
  )
)

# save as html
#htmlwidgets::saveWidget(color_table, file="color_table.html", selfcontained = TRUE)
```

## Palettes

To further interact with the color palettes, we can visualize them in a
stacked bar plot with hover text showing the color name and hex code.You
can view the interactive eCampus Colors **[View Interactive Palette Plot](https://ladyarnitah.github.io/eCampus/ref_plt.html)**

<img src="https://raw.githubusercontent.com/LadyArnitah/eCampus/master/man/figures/ref_plt.png"/>

The code to reproduce the palette is below:

``` r
# Create a stacked bar plot with custom hover text
stacked_plot <- ggplot(palette_data, aes(x = Branch, fill = Color)) +
  geom_bar(position = "stack", aes(text = paste("Branch: ", Branch, "<br>Color: ", Color, "<br>Hex: ", Hex))) +
  scale_fill_manual(values = setNames(palette_data$Hex, palette_data$Color)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust=0),
        legend.position = "none") +
  labs(title = "Color Palette for eCampus")

# Convert to an interactive plot with plotly, specifying the hover text
interactive_plot <- ggplotly(stacked_plot, tooltip = "text")%>%
  layout(
    xaxis = list(title = ""),
    yaxis= list(showticklabels = FALSE,title=""))

# Print the interactive plot
interactive_plot

# Save as html
#htmlwidgets::saveWidget(interactive_plot, "ref_plt.html", selfcontained = TRUE)
```

## Package Functions

The eCampus package includes several functions to work with color
palettes. The full function definitions can be found in the R folder of
the package. Below are examples of how to use these functions:

1.  **get_ecampus_colors()**: Retrieves color values for given color
    names.

``` r
# Example usage
library(eCampus)
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = setNames(
    get_ecampus_colors(c("eCore_Green", "eCore_Teal", "eMajor_Navy")),
    c("4", "6", "8")
  )) +
  theme_minimal()

# Example using a single color
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = get_ecampus_colors("eMajor_Navy")) +
  theme_minimal() +
  labs(title = "Bar Plot with eCore_Green", x = "Cylinders", y = "Count")
```

2.  **eCampus_theme()**: Applies a custom theme to ggplot2 plots.

``` r
# Example usage
library(eCampus)
library(ggplot2)
ggplot(data, aes(x, y)) +
  geom_point() +
  eCampus_theme()
```

3.  **scale_color_usg()**: Creates a color scale for ggplot2 based on
    eCampus palettes.

``` r
# Example usage
library(eCampus)
library(ggplot2)
ggplot(data, aes(x, y, color = category)) +
  geom_point() +
  scale_color_usg(branch = "USG-eCore", type = "Official Colors")
```

4.  **scale_fill_usg()**: Creates a fill scale for ggplot2 based on
    eCampus palettes.

``` r
# Example usage
library(eCampus)
library(ggplot2)
ggplot(data, aes(x, fill = category)) +
  geom_bar() +
  scale_fill_usg(branch = "USG-eCore", type = "Official Colors")
```

## USG Institutions

USG Institutions Map shows the USG institutions in Georgia colored by
system and the institutions plotted as points. The map uses a random
categorical variable to color the counties and a manual color palette
for the five systems. The institutions are colored based on their system
affiliation. The map is interactive, allowing users to hover over points
for more information. **[View USG Institutions Map](https://ladyarnitah.github.io/eCampus/usg_map.html)**

<img src="https://raw.githubusercontent.com/LadyArnitah/eCampus/master/man/figures/usg_map.png"/>

The code to reproduce the map is below:

``` r
# Load county map data for Georgia
ga_counties <- map_data("county") %>% filter(region == "georgia")

# Create a random categorical variable for coloring
set.seed(123)  # Ensures consistent random colors
unique_counties <- unique(ga_counties$subregion)
color_categories <- sample(c("A", "B", "C", "D", "E"), length(unique_counties), replace = TRUE)
county_colors <- data.frame(subregion = unique_counties, category = color_categories)

# Merge color categories with map data
ga_counties <- ga_counties %>%
  left_join(county_colors, by = "subregion")

# Connect to the database and fetch institution data
con <- DBI::dbConnect(odbc::odbc(), "sqlserver-ecampus")
sql_query <- "SELECT Institution, IntitutionLatitude, InstitutionLongitude, System FROM [eCampus].[dbo].[ref.Institutions]"
institutions <- DBI::dbGetQuery(con, sql_query)
DBI::dbDisconnect(con)

# Ensure latitude and longitude are numeric
institutions <- institutions %>%
  mutate(
    IntitutionLatitude = as.numeric(IntitutionLatitude),
    InstitutionLongitude = as.numeric(InstitutionLongitude)
  )

# Remove rows with missing latitude or longitude
institutions <- institutions %>%
  filter(!is.na(IntitutionLatitude) & !is.na(InstitutionLongitude))

# Define color palette for the five systems
color_palette <- c(
  "USG" = "#0D4860",       # Dark Blue
  "USG-CE" = "#F6931E",    # Orange
  "TCSG" = "#8F3776",      # Purple
  "TCSG-CE" = "#03A59D",   # Teal
  "Private" = "#EBC647"    # Yellow
)

# Create static map with counties colored and institutions as points
create_static_map <- function(county_data, institution_data, color_palette) {
  ggplot() +
    # Color counties by the random categorical variable
    geom_polygon(data = county_data, aes(x = long, y = lat, group = group, fill = category), color = "white", size = 0.2) +
    scale_fill_manual(values = c("A" = "#0D4860", "B" = "#F6931E", "C" = "#8F3776", "D" = "#03A59D", "E" = "#EBC647")) +
    
    # Plot the institution points on top of the counties
    geom_point(data = institution_data, aes(x = InstitutionLongitude, y = IntitutionLatitude, color = System, text = paste("Institution: ", Institution, "<br>System: ", System)), size = 2) +
    scale_color_manual(values = color_palette) +
    
    # Customize the map appearance
    coord_map() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = "Georgia Institutions by System with County")
}

# Generate the static map with counties and institutions
usg_map <- create_static_map(ga_counties, institutions, color_palette)

# Make the map interactive and show tooltips
ggplotly(usg_map, tooltip = "text")

# Save as html
#htmlwidgets::saveWidget(ggplotly(usg_map, tooltip = "text"), "usg_map.html", selfcontained = TRUE)
```

## eCampus Hues

The eCampus Hues are a collection of colors that can be used to create
visually appealing and harmonious color schemes. This is a range of hues
using the eCampus palette colors  **[View Interactive Hues Plot](https://ladyarnitah.github.io/eCampus/ecampus_hues.html)**

<img src="https://raw.githubusercontent.com/LadyArnitah/eCampus/master/man/figures/ecampus_hues.png"/>

The code to reproduce the palette range of hues is below:

``` r
# Function to extract all colors from ecampus_palettes
extract_colors <- function(palettes) {
  colors <- unlist(lapply(palettes, function(branch) {
    unlist(lapply(branch, function(type) {
      unname(type)
    }))
  }))
  return(unique(colors))  # Return only unique colors
}

# Extract colors
all_colors <- extract_colors(ecampus_palette)

# Manual color grouping based on the image
manual_color_groups <- list(
  "Blue" = c("#0D4860","#21387D","#0F7F86", "#38A3B1","#4CC2C5"),
  "Cyan" = c("#5EAFB4", "#03A59D", "#00A9A6","#6DC4B5", "#B4E8E5", "#DDF1EB"),
  "Gray" = c("#54554C", "#848484", "#CFCFCF","#C4B5AB"),
  "Green" = c("#004F51","#7BA640", "#7DAA45","#C9D57F"),
  "Magenta" = c( "#662362","#8F3776", "#776780"),
  "Red" = c("#AA1E23", "#A22422", "#CA2027","#CA202A"),
  "Yellow" = c("#F6931E", "#E87600", "#EBC647","#E7F1BD")
)

# Get luminance of each color
get_luminance <- function(hex) {
  rgb <- col2rgb(hex) / 255
  0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
}

# Rebuild dataframe with luminance and stack rank
data <- do.call(rbind, lapply(names(manual_color_groups), function(group) {
  colors <- manual_color_groups[[group]]
  luminance <- get_luminance(colors)
  df <- data.frame(
    Group = group,
    Color = colors,
    Luminance = luminance,
    stringsAsFactors = FALSE
  )
  df <- df[order(df$Luminance), ]  # darkest (low luminance) first
  df$Rank <- rev(seq_len(nrow(df)))  # stack from bottom up
  df
}))

data$Group <- factor(data$Group, levels = c("Blue", "Cyan", "Gray", "Green", "Magenta", "Red", "Yellow"))

# Plot with geom_col and y = 1, position = "stack"
p <- ggplot(data, aes(x = Group, y = 1, fill = Color, text = paste("Color:", Color))) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.9, color = NA) +
  scale_fill_identity() +
  theme_minimal() +
  labs(title = "Palette Colors from Dark to Light (Bottom to Top)",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

# Convert to interactive plotly plot
pp <- plotly::ggplotly(p, tooltip = "text")


# save as html
#htmlwidgets::saveWidget(pp, "ecampus_hues.html", selfcontained = TRUE)
```

## eCampus Enhanced Gradient Generator

The eCampus Enhanced Gradient Generator is a Shiny application that
allows users to create and visualize gradients from the eCampus color
palette.

The app allows you to: - Generate gradients using eCampus color
palettes. - Export gradients as CSS, URL, or SVG. - Visualize color
combinations interactively.

**[![Launch Shiny
App](https://img.shields.io/badge/Shiny%20App-Launch-blue?logo=rstudio)](https://ladyarnitah.shinyapps.io/eCampusGradient/)**

Here are examples of the eCampus palette with credits from \[Learn UI
Design\] (<https://www.learnui.design/tools/gradient-generator.html>).

To run this app locally, use the following command:

``` r
# install shiny and shinyjs if not already installed
# install.packages("shiny")
# install.packages("shinyjs")
# Run the app
shiny::runGitHub("eCampusGradient", "LadyArnitah")
```
Here is the code to reproduce the eCampus Enhanced Gradient Generator:

``` r
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
```

## Notes

- The eCampus package is designed to be user-friendly and provides a
  variety of functions to work with color palettes.
- The package is open-source and contributions are welcome.
- Package plots were inspired by \[Learn UI
Design\] (<https://www.learnui.design/tools/data-color-picker.html>).
