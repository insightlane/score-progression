install.packages("magick")
install.packages("here")
install.packages("magrittr")

library(ggplot2)
library(magick)
library(here) # For making the script run without a wd
library(magrittr) # For piping the logo


# Call back the plot
plot <- image_read(paste0(here("/"), "Average ScoreWorms by club.png"))


logo_raw <- image_read("C:/Local documents/InsightLane/Plain logo.png") 

logo <- logo_raw %>%
        image_scale("250") %>% 
        image_background("white", flatten = TRUE) %>%
        image_border("white", "600x10") 
        #image_annotate("Trial", color = "black", size = 30, location = "+10+50", gravity = "northeast")

final_plot <- image_append(image_scale(c(plot, logo)), stack = TRUE)

final_plot