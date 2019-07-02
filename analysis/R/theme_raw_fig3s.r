#davidson.theme
#maay2019
#publication 1 template theme

# ? theme_set
# theme_get

# This is currently NOT WORKING
# theme mods
# generating new theme
theme_trends <- function(base_size = 22,
                      base_family = "Times",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 300) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      strip.text = element_text(face = "bold", colour = "black"),
      strip.text.y = element_blank(),

      # strip.background = element_blank(),
      
      plot.title = element_text(hjust = 0, color = "black"),
      plot.subtitle = element_text(face = "italic", color = "black"),
      
      legend.position = "right",
      legend.key = element_blank(),
      # legend.background = element_rect(fill = "grey90"),
      # legend.key.size = unit(1, "cm"),
      legend.text = element_text(colour = "black", 
                                 size = 10),
      legend.title = element_text(colour = "black", 
                                  size = 12), 
      legend.spacing.x = unit(0.1, 'cm'),
      legend.spacing.y = unit(0.1, 'cm'),
      # legend.box.spacing = unit(0, 'cm'),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),

      axis.title.y = element_text(
        colour = "black",
        family = "Times",
        angle = 90,
        vjust = 4
      ),
      axis.title.x = element_text(colour = "black", 
                                  vjust = -2),
      
      axis.text.y = element_text(colour = "black", family = "Times"),
      axis.text.x = element_text(colour = "black", family = "Times"),
      
      axis.ticks.x = element_line(size = 1),
      axis.ticks.y = element_line(size = 1),
      axis.line.x = element_line(size = 1),
      axis.line.y = element_line(size = 1), complete = TRUE) # end of ggplot theme
        }## finishin function
    

  


# Theme 2 (extras)


#text = "times"
#size?
# size =14,
# ,family = "Times"

# extra but dont knonw what it does
# generating new theme
# theme_new <- function(base_size = 11,
#                       base_family = "",
#                       base_line_size = base_size / 170,
#                       base_rect_size = base_size / 170){
#   theme_minimal(base_size = base_size,
#                 base_family = base_family,
#                 base_line_size = base_line_s
