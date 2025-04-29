# Load libraries
library(ggplot2)
library(dplyr)

# Define color palettes for different branches
ecampus_palette <- list(
  "USG-eCore" = list(
    "Official Colors" = c("eCore_Green" = "#7BA640", "eCore_Teal" = "#00A9A6", "eCore_Callout_Green" = "#C9D57F", "eCore_Callout_Teal" = "#B4E8E5"),
    "Secondary Colors" = c("eCore_Class_is_Greener" = "#7DAA45", "eCore_OER_Orange" = "#F6931E", "eCore_Professor_Plum" = "#8F3776", "eCore_Transferable_Teal" = "#03A59D", "eCore_Accredited_Gray" = "#555755", "eCore_Proofread_Red" = "#CA2027", "eCore_Success_Team_Turquoise" = "#4CC2C5","eCore_Banner_Blue" = "#21387D","eCore_Deep_Thinking_Teal" = "#0F7F86"),
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

# Function to generate palette table data
get_palette_data <- function() {
  palette_list <- lapply(names(ecampus_palettes), function(branch) {
    lapply(names(ecampus_palettes[[branch]]), function(type) {
      colors <- ecampus_palettes[[branch]][[type]]
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


# Function to calculate luminance
get_luminance <- function(hex) {
  rgb <- col2rgb(hex) / 255  # Convert hex to normalized RGB
  luminance <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  return(luminance)
}

# Function to plot each branch's palette as bar charts and save as PNG
plot_palettes_save_png <- function(palette_data) {
  # Loop through the branches in the data
  branches <- unique(palette_data$Branch)

  for (branch in branches) {
    branch_data <- palette_data[palette_data$Branch == branch, ]

    # Create the plot for the branch's color palette
    p <- ggplot(branch_data, aes(x = Color, y = 1, fill = Hex)) +
      geom_bar(stat = "identity", show.legend = FALSE, width = 1) +  # Set width to 1 to remove space between bars
      scale_fill_identity() +
      labs(title = paste("Color Palette for", branch)) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),       # Remove axis titles
        axis.text = element_blank(),        # Remove axis text for both x and y axes
        panel.grid = element_blank(),       # Remove grid lines
        axis.ticks = element_blank(),       # Remove axis ticks
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Bold and centered title
        plot.margin = margin(1, 1, 1, 1, "cm")  # Add margins
      ) +
      geom_text(aes(label = Color),
                position = position_stack(vjust = 0.5), # Position labels in the middle of the bars
                color = ifelse(get_luminance(branch_data$Hex) > 0.5, "black", "white"), # Adjust text color based on luminance
                size = 2, fontface = "bold", angle = 90) # Slant the labels and make them bold

    # Save the plot as a PNG file
    filename <- paste0("color_palette_", branch, ".png")
    ggsave(filename = filename, plot = p, width = 8, height = 4, dpi = 300)
    message(paste("Saved plot for", branch, "as", filename))
  }
}

# Run the function to plot and save the PNG files
plot_palettes_save_png(palette_data)


