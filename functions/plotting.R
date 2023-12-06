## ---------------------------
##
## Script name: Plotting.r
##
## Purpose of script: 
##      Functions for making plots from the PalmerPenguins dataset
##
## Date Created: 2023-12-04
##
##
## ---------------------------
##

#Scatterplot to show the relationship between body mass and flipper length for each species : 

plot_mass_flipper_figure <- function(penguins_mass_flippers){
  penguins_mass_flippers %>%
    ggplot(aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
    geom_point(size = 3, alpha=0.4) +
    scale_color_manual(values = c("darkorange","purple","cyan4"),
                       name = 'Species') +
    labs(title = "Penguin species body mass against flipper length",
      x = "Body Mass (g)",
      y = "Flipper Length (mm)") +
    theme(plot.title = element_text(hjust = 0.6, vjust = 1)) +
    theme_minimal() +
    guides(color = guide_legend(reverse = TRUE))
}

#Saving the scatter plot as a png 

save_mass_flipper_plot_png <- function(penguins_mass_flippers, 
                                       filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  mass_flipper_scatterplot <- plot_mass_flipper_figure (penguins_mass_flippers)
  print(mass_flipper_scatterplot)
  dev.off()
}

#saving the scatter plot as an svg 

save_mass_flipper_plot_svg <- function(penguins_mass_flippers, 
                                       filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  mass_flipper_scatterplot <- plot_mass_flipper_figure (penguins_mass_flippers)
  print(mass_flipper_scatterplot)
  dev.off()
}



#results plot of the linear regression of the correlation between body mass and flipper length for each penguin species :

results_mass_flipper_figure <- function(penguins_mass_flippers) {
  ggplot(penguins_mass_flippers, aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
    geom_point(size = 3, alpha = 0.4) +
    scale_color_manual(values = c("darkorange", "purple", "cyan4"),
                       name = 'Species') +
    labs(title = 'Regression analysis of species body mass against flipper length',
      x = "Body Mass (g)",
      y = "Flipper Length (mm)"
    ) +
    theme(plot.title = element_text(hjust = 0.3, vjust = 1)) +
    theme_minimal() + 
    guides(color = guide_legend(reverse = TRUE)) + 
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, aes(group = species, color = species), 
                show.legend = FALSE) +
    geom_text(aes(label = sprintf("Co-efficient: %.3e", 9.039e-03)), x = 4000, y = 230, hjust = 1, vjust = 0, 
              size = 3, color = "cyan4", fontface = "bold") +
    geom_text(aes(label = sprintf("Co-efficient: %.2e", 1.191e-02)), x = 4000, y = 225, hjust = 1, vjust = 0, 
              size = 3, color = "purple", fontface = "bold") +
    geom_text(aes(label = sprintf("Co-efficient: %.3e", 6.677e-03)), x = 4000, y = 220, hjust = 1, vjust = 0, 
              size = 3, color = "darkorange", fontface = "bold")
}  




#saving results scatter plot as a png

save_mass_flipper_results_png <- function(penguins_mass_flippers, 
                                          filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  mass_flipper_results_plot <- results_mass_flipper_figure(penguins_mass_flippers)
  print(mass_flipper_results_plot)
  dev.off()
}


#saving the results scatterplot as an svg

save_mass_flipper_results_svg <- function(penguins_mass_flippers, 
                                          filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  mass_flipper_results_plot <- results_mass_flipper_figure(penguins_mass_flippers)
  print(mass_flipper_results_plot)
  dev.off()
}


