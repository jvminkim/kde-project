list_of_packages_kde = c("MASS", "ggplot2")
lapply(list_of_packages_kde, library, character.only = TRUE)

batters_kde = MASS::kde2d(bip_data$x_land, bip_data$y_land)

bip_data = dplyr::mutate(bip_data, x_land = hc_x - 125.42, y_land = 198.27 - hc_y)


shohei_ohtani_hitting = dplyr::filter(bip_data, batter == "660271")

shohei_ohtani_kde = MASS::kde2d(shohei_ohtani_hitting$x_land, shohei_ohtani_hitting$y_land, n = 100)
ohtani_data = expand.grid(x = shohei_ohtani_kde$x, y = shohei_ohtani_kde$y)
ohtani_data$z = as.numeric(shohei_ohtani_kde$z)

x_diff = (max(ohtani_data$x) - min(ohtani_data$x)) /(length(unique(ohtani_data$x)) - 1)
y_diff = (max(ohtani_data$y) - min(ohtani_data$y)) /(length(unique(ohtani_data$y)) - 1)

cell_area = x_diff * y_diff

total_density = sum(ohtani_data$z * cell_area)
ohtani_data$z = ohtani_data$z/total_density

create_z_breaks <- function(df) {
  # Calculate grid cell area based on x and y ranges
  x_diff <- (max(df$x) - min(df$x)) / (length(unique(df$x)) - 1)
  y_diff <- (max(df$y) - min(df$y)) / (length(unique(df$y)) - 1)
  cell_area <- x_diff * y_diff
  
  # Normalize z to make it a proper density
  total_density <- sum(df$z * cell_area)
  df$z <- df$z / total_density
  
  # Add cumulative density
  df <- df %>%
    dplyr::arrange(z) %>%
    dplyr::mutate(cum_dens = cumsum(z * cell_area))
  
  # Compute z_breaks using cumulative density thresholds
  sapply(seq(0.10, 0.90, by = 0.10), function(cut) {
    df$z[min(which(df$cum_dens > cut))]
  }) %>%
    c(max(df$z) + 0.01)
}
z_breaks <- create_z_breaks(ohtani_data)


ggplot(ohtani_data, aes(x = x, y = y, z = z)) +
  geom_contour_filled(breaks = z_breaks, show.legend = FALSE) +
  geom_spray_chart +
  scale_fill_brewer(palette = "Greens") +
  theme_minimal() +
  coord_fixed() + 
  labs(fill = "Density Level",
       title = "Shohei Ohtani Spray Chart")
