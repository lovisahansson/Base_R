#------------------------------------------------------------------------------
# Name date
#------------------------------------------------------------------------------

setwd("~/R/Base_R")

install.packages('tidyverse')
install.packages('ggpubr')
install.packages('patchwork')
install.packages('cowplot')
install.packages('viridis')                 #color gradients
install.packages('usethis')                 #for GitHub
#install.packages('PerformaceAnalytics')
#install.packages('lme4')

library('tidyverse')
library('ggpubr')
library('patchwork')
library('cowplot')
library('viridis')
library('usethis')
#library('PerformanceAnalytics')
#library('lme4')

#------------------------------------------------------------------------------

#To import data and to check for any changes that need to be made (see below)
faba_data <- read.csv("faba_bean_phenotypic_traits.csv", header=T, sep=",")

#To mutate the data set at once without having to use multiple commands
faba_data <- read.csv("faba_bean_phenotypic_traits.csv", header=T, sep=",") %>%
  mutate_at(c('year', 'location', 'rep', 'plot', 'cultivar'), as.factor) %>%
  mutate_at(c('nodules', 'pods'), as.numeric)

  #mutate_if(is.character,as.factor)

#To create a subset of the data that contain only "E1" from the column "cultivar"
cultivar_E1 <- subset(faba_data, cultivar=="E1")

#Shows all the options within one factor
levels(faba_data$cultivar)

#------------------------------------------------------------------------------

#Visualization

#Manually decide on the order for cultivar
faba_data <- faba_data %>%
  mutate(cultivar = factor(cultivar, levels=c("E1", "E2", "H", "L1", "L2")))

ggplot(data=faba_data, aes(x=cultivar, y=height)) + 
  geom_boxplot(fill=penguin_corp_color("pink")) +
  geom_point(aes(colour=cultivar), size=2, alpha=0.6) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10))
  
#------------------------------------------------------------------------------

#Create own color palettes

penguin_corp_color <- function(...) {
  
  penguin_corp_colors <- c(
    `pink`     = "#F7B1AB",
    `lavender` = "#807182",
    `gray`     = "#E3DDDD",
    `brown`    = "#A45C3D",
    `purple`   = "#1C0221")
  
  cols <- c(...)
  
  if (is.null(cols))
    return (penguin_corp_colors)
  
  penguin_corp_colors[cols]
}
penguin_corp_palette <- function(palette = "main", ...) {
  
  penguin_corp_palettes <- list(
    `main` = penguin_corp_color("lavender", "gray", "pink", "brown"),
    
    `highlight` = penguin_corp_color("purple", "gray")
  )
  
  penguin_corp_palettes[[palette]]
  
}

scales::show_col(penguin_corp_palette("main"),cex_label=2)

palette_gen <- function(palette = "main", direction = 1) {
  
  function(n) {
    
    if (n > length(penguin_corp_palette(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      
      all_colors <- penguin_corp_palette(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}
palette_gen_c <- function(palette = "main", direction = 1, ...) {
  
  pal <- penguin_corp_palette(palette)
  
  pal <- if (direction >= 0) pal else rev(pal)
  
  colorRampPalette(pal, ...)
}
scale_fill_penguin <- function(palette = "main", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "fill", "penguin",
    palette_gen(palette, direction),
    ...
  )
}
scale_colour_penguin <- function(palette = "main", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "colour", "penguin",
    palette_gen(palette, direction),
    ...
  )
}

scale_color_penguin <- scale_colour_penguin

scale_color_penguin_c <- function(palette = "main", direction = 1, ...) {
  
  pal <- palette_gen_c(palette = palette, direction = direction)
  
  scale_color_gradientn(colors = pal(256), ...)
  
}
