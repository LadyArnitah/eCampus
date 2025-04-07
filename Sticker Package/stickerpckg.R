#install.packages("hexSticker")

library(hexSticker)
library(showtext)

# Add Google fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# Create the sticker
sticker(
  subplot = "./ecampus-logo.png", # Path to your image
  package = "eCampus",
  p_size = 20, # Package name font size
  p_color = "#FFFFFF", # Package name color
  s_x = 1, s_y = 0.8, s_width = 0.6, # Image position and size
  h_fill = "#00a9a6", # Background color
  h_color = "#c9d57f", # Border color
  p_family = "roboto", # Font family
  filename = "ecampus.png" # Save path
)
