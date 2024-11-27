

#' Batter pool
#' 


get_batter_pool = function(data, year_start, year_end) {
  data = data |> 
    dplyr::filter(game_year >= year_start &
                  game_year <= year_end)
  
  batter_pool_year = data |> 
    dplyr::group_by(batter, pitch_type, game_year, stand) |> 
    dplyr::summarise(lf_prc = mean(spray_angle < -15),
                  cf_prc = mean(spray_angle >= -15 & spray_angle <= 15),
                  rf_prc = mean(spray_angle > 15),
                  launch_angle = mean(launch_angle),
                  launch_speed = mean(launch_speed),
                  n = dplyr::n())
  
  batter_pool_all = data |> 
    dplyr::group_by(batter, pitch_type, stand) |> 
    dplyr::summarise(lf_prc = mean(spray_angle < -15),
                     cf_prc = mean(spray_angle >= -15 & spray_angle <= 15),
                     rf_prc = mean(spray_angle > 15),
                     launch_angle = mean(launch_angle),
                     launch_speed = mean(launch_speed),
                     n = dplyr::n())
  batter_pool_all$game_year = as.integer(0)
  
  batter_pool = dplyr::ungroup(dplyr::bind_rows(batter_pool_year, batter_pool_all))
  
  batter_pool |> 
    dplyr::mutate(
      lf_prc = as.vector(scale(lf_prc)),
      cf_prc = as.vector(scale(cf_prc)),
      rf_prc = as.vector(scale(rf_prc)),
      launch_angle = as.vector(scale(launch_angle)),
      launch_speed = as.vector(scale(launch_speed))
    )
}


batter_pool_sim = get_batter_pool(bip_data, 2017, 2023)

make_bip_pool_synth_batter = function(.pitch_type, .batter, .pitcher, bip_data, batter_pool, .stand, .p_throws, ratio = 0.85) {

  p_bip = bip_data |> 
    dplyr::filter(batter != .batter) |> 
    dplyr::filter(pitcher == .pitcher) |>
    dplyr::filter(p_throws == .p_throws) |>
    dplyr::filter(stand == .stand) |>
    dplyr::filter(pitch_type == .pitch_type) |>
    dplyr::select(game_year, batter, x_land, y_land)

  b_study_char = batter_pool |> 
    dplyr::filter(batter == .batter) |> 
    dplyr::filter(stand == .stand) |> 
    dplyr::filter(pitch_type == .pitch_type) |> 
    dplyr::filter(game_year == 0)
  
  b_pool_char = batter_pool |> 
    dplyr::filter(batter %in% unique(p_bip$batter)) |> 
    dplyr::filter(pitch_type == .pitch_type) |> 
    dplyr::filter(stand == .stand) |> 
    dplyr::filter(game_year != 0)

  b_pool_sim = calc_sim_batter(b_study_char = b_study_char,
                               b_pool_char = b_pool_char,
                               ratio = ratio)
  
  b_pool = dplyr::bind_cols(b_pool_char, b_pool_sim)
  
  dplyr::left_join(p_bip, b_pool, by = c("batter", "game_year")) |> 
    dplyr::select(x_land, y_land, similarity, weight)
}

shohei_ohtani_synth = make_bip_pool_synth_batter("FF", 660271, 434378, bip_data, batter_pool_sim, "L", "R", .85)



get_pitcher_pool = function(data, year_start, year_end) {
  data = data |> 
    dplyr::filter(game_year >= year_start &
                  game_year <= year_end)
  
  pitcher_pool_year = data |> 
    dplyr::group_by(pitcher, pitch_type, game_year, p_throws) |> 
    dplyr::summarise(release_speed = mean(release_speed),
                     release_spin_rate = mean(release_spin_rate),
                     pfx_x = mean(pfx_x),
                     pfx_z = mean(pfx_z),
                     release_pos_x = mean(release_pos_x),
                     release_pos_y = mean(release_pos_y),
                     release_pos_z = mean(release_pos_z),
                     angle = mean(angle),
                     n = dplyr::n())
  
  pitcher_pool_all = data |> 
    dplyr::group_by(pitcher, pitch_type, p_throws) |> 
    dplyr::summarise(release_speed = mean(release_speed),
                     release_spin_rate = mean(release_spin_rate),
                     pfx_x = mean(pfx_x),
                     pfx_z = mean(pfx_z),
                     release_pos_x = mean(release_pos_x),
                     release_pos_y = mean(release_pos_y),
                     release_pos_z = mean(release_pos_z),
                     angle = mean(angle),
                     n = dplyr::n())
  pitcher_pool_all$game_year = as.integer(0)
  
  pitcher_pool = dplyr::ungroup(dplyr::bind_rows(pitcher_pool_year, pitcher_pool_all))
  
  pitcher_pool |> 
    dplyr::mutate(
      release_speed = as.vector(scale(release_speed)),
      release_spin_rate = as.vector(scale(release_spin_rate)),
      pfx_x = as.vector(scale(pfx_x)),
      pfx_z = as.vector(scale(pfx_z)),
      release_pos_x = as.vector(scale(release_pos_x)),
      release_pos_y = as.vector(scale(release_pos_y)),
      release_pos_z = as.vector(scale(release_pos_z)),
      angle = as.vector(scale(angle)),
    )
}

pitcher_pool = get_pitcher_pool(bip_data, 2017, 2023)


make_bip_pool_synth_pitcher = function(.pitch_type, .batter, .pitcher, bip_data, pitcher_pool, .stand, .p_throws, .ratio = 0.85) {
  
  b_bip = bip_data |> 
    dplyr::filter(batter == .batter) |> 
    dplyr::filter(pitcher == .pitcher) |> 
    dplyr::filter(p_throws == .p_throws) |> 
    dplyr::filter(stand == .stand) |> 
    dplyr::filter(pitch_type == .pitch_type) |> 
    dplyr::select(game_year, pitcher, x_land, y_land)
  
  p_study_char = pitcher_pool |>
    dplyr::filter(pitcher == .pitcher) |> 
    dplyr::filter(p_throws == .p_throws) |> 
    dplyr::filter(pitch_type == .pitch_type) |> 
    dplyr::filter(game_year == 0)
  
  p_pool_char = pitcher_pool |> 
    dplyr::filter(pitcher %in% unique(b_bip$pitcher)) |> 
    dplyr::filter(pitch_type == .pitch_type) |> 
    dplyr::filter(p_throws == .p_throws) |> 
    dplyr::filter(game_year != 0)
  
  p_pool_sims = calc_sim_pitcher(p_study_char = p_study_char,
                                 p_pool_char = p_pool_char,
                                 ratio = .ratio)
  
  p_pool = dplyr::bind_cols(p_pool_char, p_pool_sims)
  
  dplyr::left_join(b_bip, p_pool, by = c("pitcher", "game_year")) |> 
    dplyr::select(x_land, y_land, similarity, weight)
}

justin_verlander_synth = make_bip_pool_synth_pitcher("FF", 660271, 434378, bip_data, pitcher_pool, "L", "R", .85)

make_empirical_pool = function(.batter, .pitcher, bip_data, type, hands) {
  if (type == "batter") {
    pool = bip_data |> 
      dplyr::filter(batter == .batter) |> 
      dplyr::filter(p_throws == hands[["p_throws"]])
  }
  
  if (type == "pitcher") {
    pool = bip_data %>%
      dplyr::filter(.data$pitcher == .pitcher) %>%
      dplyr::filter(.data$stand == hands[["b_stands"]])
  }
  
  if (type == "both") {
    pool = bip_data %>%
      dplyr::filter(.data$batter == .batter) %>%
      dplyr::filter(.data$pitcher == .pitcher)
  }
  
  return(pool)
}


