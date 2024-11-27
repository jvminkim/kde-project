

process_statcast = function(data, height_data) {
  
  na_vars = c(
    "spin_dir",
    "spin_rate_deprecated",
    "break_angle_deprecated",
    "break_length_deprecated",
    "tfs_deprecated",
    "tfs_zulu_deprecated",
    "umpire"
  )
  
  existing_na_vars = na_vars[na_vars %in% colnames(data)]

  
  data$release_speed                   = as.numeric(data$release_speed)
  data$release_pos_x                   = as.numeric(data$release_pos_x)
  data$release_pos_z                   = as.numeric(data$release_pos_z)
  data$pfx_x                           = as.numeric(data$pfx_x)
  data$pfx_z                           = as.numeric(data$pfx_z)
  data$plate_x                         = as.numeric(data$plate_x)
  data$plate_z                         = as.numeric(data$plate_z)
  data$hc_x                            = as.numeric(data$hc_x)
  data$hc_y                            = as.numeric(data$hc_y)
  data$vx0                             = as.numeric(data$vx0)
  data$vy0                             = as.numeric(data$vy0)
  data$vz0                             = as.numeric(data$vz0)
  data$ax                              = as.numeric(data$ax)
  data$ay                              = as.numeric(data$ay)
  data$az                              = as.numeric(data$az)
  data$sz_top                          = as.numeric(data$sz_top)
  data$sz_bot                          = as.numeric(data$sz_bot)
  data$hit_distance_sc                 = as.numeric(data$hit_distance_sc)
  data$launch_speed                    = as.numeric(data$launch_speed)
  data$launch_angle                    = as.numeric(data$launch_angle)
  data$effective_speed                 = as.numeric(data$effective_speed)
  data$release_spin_rate               = as.numeric(data$release_spin_rate)
  data$release_extension               = as.numeric(data$release_extension)
  data$release_pos_y                   = as.numeric(data$release_pos_y)
  data$estimated_ba_using_speedangle   = as.numeric(data$estimated_ba_using_speedangle)
  data$estimated_woba_using_speedangle = as.numeric(data$estimated_woba_using_speedangle)
  data$woba_value                      = as.numeric(data$woba_value)
  data$woba_denom                      = as.numeric(data$woba_denom)
  data$babip_value                     = as.numeric(data$babip_value)
  data$iso_value                       = as.numeric(data$iso_value)
  data$launch_speed_angle              = as.numeric(data$launch_speed_angle)
  
  data = data |> 
  dplyr::select(-any_of("height_ft_in")) |> 
  dplyr::left_join(height_data |> dplyr::select(player_id, height_ft_in),
                   by = c("pitcher" = "player_id")) |> 
  dplyr::filter(pitch_type != "KN" &
                pitch_type != "EP" &
                pitch_type != "SC") |> 
  dplyr::mutate(pitch_type = ifelse(pitch_type == "FO", "FS",
                                   ifelse(pitch_type == "KC", "KO", 
                                          ifelse(pitch_type == "FA", "FF", pitch_type))),
                pitch_launch_h_c = atan(vx0 / vy0),
                pitch_launch_v_c = atan(vx0 / sqrt(vx0^2 + vy0^2)),
                spray_angle = atan((hc_x - 125.42) / (198.27 - hc_y)) * 180 / pi) |> 
  dplyr::group_by(pitcher, game_year, pitch_type) |> 
  dplyr::mutate(avg_release_x = mean(release_pos_x),
                avg_release_y = mean(release_pos_y),
                avg_release_z = mean(release_pos_z),
                adjacent = avg_release_z - (as.numeric(height_ft_in) * 0.7),
                opposite = abs(avg_release_x),
                hypotenuse = sqrt(adjacent^2 + opposite^2),
                angle = (acos((adjacent^2 + hypotenuse^2 - opposite^2) / 
                              (2 * (adjacent * hypotenuse))) * (180 / pi)))
  return(data)
}

bip_data = process_statcast(bip_data, player_heights)

