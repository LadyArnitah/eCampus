# Load libraries
library(magick)
library(grid)
library(gridExtra)
library(png)
library(grid)
library(cowplot)

# Function to merge logo and branch plot and save as PNG
merge_logo_and_plot <- function(logo_paths, plot_paths, output_dir = "merged_plots") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Loop through the logos and plots
  for (i in 1:length(logo_paths)) {
    # Ensure paths are correct (use relative paths if in the same working directory or absolute paths)
    if (!file.exists(logo_paths[i])) {
      stop(paste("Logo file does not exist:", logo_paths[i]))
    }
    if (!file.exists(plot_paths[i])) {
      stop(paste("Plot file does not exist:", plot_paths[i]))
    }
    
    # Load the logo
    logo <- readPNG(logo_paths[i])
    logo_grob <- rasterGrob(logo, interpolate = TRUE)
    
    # Load the branch plot
    plot <- readPNG(plot_paths[i])  # Assuming the plots are already saved as PNG
    plot_grob <- rasterGrob(plot, interpolate = TRUE)
    
    # Combine the logo and plot (logo on top, plot below)
    final_plot <- plot_grid(
      ggdraw() + draw_grob(logo_grob),   # Add logo
      ggdraw() + draw_grob(plot_grob),   # Add plot
      ncol = 1,                           # Arrange vertically
      rel_heights = c(0.2, 1)             # Adjust space between logo and plot
    )
    
    # Get the branch name (assuming filenames are named after the branch)
    branch_name <- gsub(".png", "", basename(logo_paths[i]))
    
    # Save the combined plot as a PNG file
    output_file <- file.path(output_dir, paste0("final_", branch_name, ".png"))
    ggsave(output_file, plot = final_plot, width = 8, height = 6, dpi = 300)
    message(paste("Saved combined plot for", branch_name, "as", output_file))
  }
}

# Example usage (assuming logo_paths and plot_paths are defined)
# Replace with actual logo and plot paths, either relative or absolute
logo_paths <- c("USG-FreeCampus.png","USGeCampus.png", "USGecore.png", "USGemajor.png")  # Update the file name if needed
plot_paths <- c("color_palette_USG-freeCampus.png", "color_palette_USG-eCampus.png", "color_palette_USG-eCore.png", "color_palette_USG-eMajor.png")  # Update the file name if needed


# Call the function to merge and save the images
merge_logo_and_plot(logo_paths, plot_paths)
