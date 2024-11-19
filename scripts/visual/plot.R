list_of_packages_viz = c("GeomMLBStadiums", "ggplot2", "dplyr", "wesanderson", "plotly")
lapply(list_of_packages_viz, library, character.only=TRUE)

#Transform hc_x, hc_y into x,y coordinates

geom_mlb_stadium_template = function(team) {
  geom_base = GeomMLBStadiums::geom_mlb_stadium(stadium_ids = team,
                                                stadium_transform_coords = TRUE,
                                                stadium_segments = "all")
  return(geom_base)
}



geom_spray_chart = geom_mlb_stadium_template("generic")
bip_data = mlbam_xy_transformation(bip_data)
statcast_data = dplyr::mutate(statcast_data, x_land = hc_x - 125.42, y_land = 198.27 - hc_y)

hitting_palette = wes_palette("Zissou1", 5, type = "continuous")

z_breaks = c(sapply(seq(0.10, 0.90, by = 0.10), find_cut, df = df), max(df$z) + 0.01)

ggplot(data = shohei_ohtani_hitting, aes(x = x_land, y = y_land)) +
  facet_wrap(~stand) +
  geom_density_2d_filled(aes(fill = after_stat(level)), bins = 9) +
  scale_fill_brewer(palette = "Oranges", direction = 1) +
  geom_spray_chart +
  theme_bw()

statcast_data = GeomMLBStadiums::mlbam_xy_transformation(statcast_data)
shohei_ohtani_hitting = dplyr::filter(bip_data, batter == "660271")
