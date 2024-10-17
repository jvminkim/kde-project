list_of_packages_viz = c("GeomMLBStadiums", "ggplot2", "dplyr", "wesanderson")
lapply(list_of_packages_viz, library, character.only=TRUE)

#Transform hc_x, hc_y into x,y coordinates

geom_mlb_stadium_template = function(team) {
  geom_base = GeomMLBStadiums::geom_mlb_stadium(stadium_ids = team,
                                                stadium_transform_coords = TRUE)
  return(geom_base)
}

geom_spray_chart = geom_mlb_stadium_template("generic")

statcast_data = dplyr::mutate(statcast_data, x_land = hc_x - 125.42, y_land = 198.27 - hc_y)

hitting_palette = wes_palette("Zissou1", 5, type = "continuous")

ggplot(data = shohei_ohtani_hitting, aes(x = hc_x_ , y = hc_y_)) +
  #geom_point() +
  facet_wrap(~stand) +
  geom_density_2d_filled(aes(fill = after_stat(level)),alpha = 0.6, bins = 15) +
  scale_fill_brewer(palette = "Reds") +
  geom_spray_chart +
  theme_bw()


statcast_data = GeomMLBStadiums::mlbam_xy_transformation(statcast_data)
shohei_ohtani_hitting = dplyr::filter(statcast_data, batter == "660271")
