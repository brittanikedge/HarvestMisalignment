#/*=================================================*/
#' # Make experiment grids (basic cell, plot, strip)
#/*=================================================*/
make_trial_grids <- function(field, ab_line, starting_point, plot_width) {
  
  #/*=================================================*/
  #' # Define functions
  #/*=================================================*/
  #/*----------------------------------*/
  #' ## make polygons
  #/*----------------------------------*/
  make_polygon <- function(strt_point_new, multiplier, direction) {

    point_1 <- strt_point_new + subplot_length * ab_xy_nml * (multiplier - 1)
    point_2 <- point_1 -  plot_width * direction * ab_xy_nml_p90
    point_3 <- point_2 + ab_xy_nml * subplot_length
    point_4 <- point_3 +  plot_width * direction * ab_xy_nml_p90

    temp_polygon <- rbind(
      point_1,
      point_2,
      point_3,
      point_4,
      point_1
    ) %>% 
    list() %>% 
    st_polygon()

    return(temp_polygon)

  }
  #/*----------------------------------*/
  #' ## rotation matrix
  #/*----------------------------------*/
  rotate_mat_p90 <- matrix(
      c(
        cos(pi / 2),
        sin(pi / 2),
        -sin(pi / 2),
        cos(pi / 2)
      ),
      nrow = 2
    )

  #/*----------------------------------*/
  #' ## Make an extended line
  #/*----------------------------------*/
  # make line with starting point and a desired shift
  make_extended_line <- function(starting_point, step, multiplier, step_v){

    extended_line <- rbind(
      starting_point + step * multiplier,
      starting_point + step_v * 1000 + step * multiplier
      ) %>% 
      st_linestring() %>% 
      list() %>% 
      st_as_sfc() %>% 
      st_set_crs(st_crs(field))

    return(extended_line)
  }

  #/*----------------------------------*/
  #' ## Check intersections
  #/*----------------------------------*/
  # check the intersection of the lines in the direction specified
  check_intersection <- function(field, step, direction, step_v){

    is_int_ls <- rep(FALSE, 100) 

    for (i in 1:100) {

      #--- shift 50 meter every time ---#
      line <- make_extended_line(
        starting_point, 
        plot_width * step * direction, 
        i,
        step_v
      )

      is_int_ls[i] <- st_intersects(field, line, sparse = FALSE)[1,1]

      if (is_int_ls[i]){
        break
      }

    }

    return(is_int_ls)
    
  }

  #/*----------------------------------*/
  #' ## vector of points of sf of points
  #/*----------------------------------*/
  vect_to_sf_point <- function(vec){
    st_as_sfc(list(st_point(vec))) %>% 
      st_set_crs(st_crs(field))
  }

  #/*----------------------------------*/
  #' ## Re-assign plot id based on observation numbers per plot 
  #/*----------------------------------*/
  reassign_plot_id <- function (data, grp) {

    temp_data <- data[group == grp, ] 

    if(max(temp_data$plot_id) == 1) {
      #--- if there is only one plot_id in the strip ---#
      return(temp_data[, .(id, plot_id, group, x)]) 
    }

    if (nrow(temp_data[too_short == TRUE, ]) == 0){
      return(temp_data[, .(id, plot_id, group, x)])    
    }

    num_obs_short <- temp_data[too_short == TRUE, obs_per_plot] %>% 
      unique()

    short_plot_id <- temp_data[too_short == TRUE, plot_id] %>% 
      unique()

    num_obs_short_1 <- temp_data[plot_id == (short_plot_id - 1), obs_per_plot] %>% 
      unique()

    if (num_obs_short >= (2 * min_obs - mean_obs)) { # make the last two short
        
      first_obs_set <- ceiling((num_obs_short + mean_obs) / 2)

      temp_data[plot_id %in% c(short_plot_id, short_plot_id - 1), cum_num_reassign := cumsum(dummy)] %>% 
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 1] 

    } else if ((max(temp_data$plot_id) >= 3) & num_obs_short >= (3 * min_obs - 2 * mean_obs)) { 

      # make the last three short (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 3)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>% 
        #--- third last ---#
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id] %>% 
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2] %>% 
        .[cum_num_reassign > first_obs_set & cum_num_reassign <= 2 * first_obs_set, plot_id := short_plot_id - 1]  
      
    } else if (max(temp_data$plot_id) >= 3) { 

      # make the 2nd and 3rd last longer (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 2)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>% 
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id - 1] %>% 
        #--- third last ---#
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2]  

    } else {

      # make the two into one (there needs to be at least 2 plot ids)
      temp_data[, plot_id := 1]

    }

    # temp_data[, .N, by = plot_id]

    return(temp_data[, .(id, plot_id, group, x)])

  }

  #/*=================================================*/
  #' # Main code
  #/*=================================================*/
  ab_xy <- st_geometry(ab_line)[[1]][2,] - st_geometry(ab_line)[[1]][1,]
  ab_length <- sqrt(sum(ab_xy^2))
  ab_xy_nml <- ab_xy / ab_length
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  #--- identify the number of subplots in a strip ---#
  f_bbox <- st_bbox(field)
  max_dist <- sqrt((f_bbox["ymax"] - f_bbox["ymin"])^2 + (f_bbox["xmax"] - f_bbox["xmin"])^2) + 100 

  max_dist_cover <- ceiling(max_dist / 10) * 10
  num_subplots_in_a_strip <- ceiling(max_dist_cover / subplot_length) 

  #/*----------------------------------*/
  #' ## Detect which direction to go
  #/*----------------------------------*/
  is_int_p <- check_intersection(field, ab_xy_nml_p90, direction = 1, ab_xy_nml)

  if (any(is_int_p)) {
    direction <- 1
    how_many_in <- which(is_int_p)
  } else {
    direction <- - 1  
    how_many_in <- which(check_intersection(field, ab_xy_nml_p90, -1, ab_xy_nml))
  }

  #--- refresh the starting point ---#
  strt_point_new <- starting_point + how_many_in * plot_width * direction * ab_xy_nml_p90
  strt_point_new_sf <- vect_to_sf_point(strt_point_new)

  #/*----------------------------------*/
  #' ## Create strip of polygons strip by strip 
  #/*----------------------------------*/  
  is_intersecting <- TRUE

  exp_sf_ls <- list()
  group <- 1

  while (is_intersecting){

    exp_sf_ls[[paste(group)]] <- lapply(
      1:num_subplots_in_a_strip, 
      function(x) make_polygon(
        strt_point_new + plot_width * direction * ab_xy_nml_p90 * (group - 1), 
        x, 
        direction
      )
    ) %>% 
    st_as_sfc() %>% 
    st_set_crs(st_crs(field)) %>% 
    st_as_sf() %>% 
    mutate(
      group = group,
      id = 1:nrow(.)
    )  

    is_intersecting <- st_intersects(exp_sf_ls[[paste(group)]], field, sparse = F)[, 1] %>% any()


    group <- group + 1

  }

  all_plygons <- do.call("rbind", exp_sf_ls) %>% 
    .[field, ]

  #/*----------------------------------*/
  #' ## Reassign plot id
  #/*----------------------------------*/
  # group: strip id 
  # id: subplot id 
  # plot_id: plot id 
  min_obs <- 20 # (200 feet)
  mean_obs <- 25 # (250 feet)
  max_obs <- 30 #  (300 feet)
  
  data <- all_plygons %>% 
    data.table() %>% 
    #--- observations per strip ---#
    .[, obs_per_strip := .N, by = .(group)] %>% 
    #--- (initial) plot id ---#
    .[, dummy := 1] %>% 
    .[, cum_num := cumsum(dummy), by = .(group)] %>% 
    .[, plot_id := (cum_num - 1) %/% mean_obs + 1, by = .(group)] %>% 
    #--- max number of plots per group ---#
    .[, max_plot_id := max(plot_id), by = .(group, plot_id)] %>% 
    #--- number of subplots per plot ---#
    .[, obs_per_plot := .N, by = .(group, plot_id)] %>% 
    .[, too_short := obs_per_plot <= min_obs]

  group_ls <- data$group %>% unique()

  final_ploygons <- lapply(group_ls, function(x) reassign_plot_id(data, x)) %>% 
    rbindlist() %>% 
    st_as_sf() %>% 
    rename(geometry = x) %>% 
    rename(strip_id = group) %>% 
    mutate(cell_id := 1:nrow(.)) %>% 
    select(-id)
     
  return(final_ploygons)  

}

# /*=================================================*/
#' # Assign rates to the trial design data
# /*=================================================*/
# data_sf <- st_as_sf(data)
# rates_ls <- N_levels 

assign_rates <- function(data_sf, rates_ls, merge = TRUE) {

  gen_sequence <- function(length) {
    if (length %% 2 == 0) {
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else {
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }
    return(seq_r)
  }

  gen_rd_seq <- function(seq_element, num) {
    for (i in 1:num) {
      if (i == 1) {
        seq_return <- seq_element
      } else {
        if (runif(1) < 0.5) {
          # seq_return <- c(seq_return, rev(seq_element))
          seq_return <- c(seq_return, seq_element)
        } else {
          seq_return <- c(seq_return, seq_element)
        }
      }
    }
    return(seq_return)
  }

  get_seq_for_strip <- function(rate_ranks_seq, num_seq, exclude_ls = NULL) {

    position_ls <- 1:rates_len
    if (length(exclude_ls) == (rates_len - 1)){
      position <- position_ls[!(position_ls %in% exclude_ls)]
    } else {
      position <- sample(position_ls[!(position_ls %in% exclude_ls)], 1)
    }
    seq_possible <- gen_rd_seq(rate_ranks_seq, num_seq)

    return(seq_possible[position:(position + max_plot_id - 1)])
  }

  # /*=================================================*/
  #' # Assign rates
  # /*=================================================*/

  rates_data <- data.table(
    rate = rates_ls,
    rate_rank = seq_len(length(rates_ls))
  )

  rates_len <- nrow(rates_data)

  #--- create a sequence of rate ranks ---#
  rate_ranks_seq <- gen_sequence(rates_len)

  data_dt <- data.table(data_sf)

  strip_ls <- data_dt[, strip_id] %>% unique()

  design_data_ls <- list()

  for (i in strip_ls) {

    max_plot_id <- data_dt[strip_id == i, max(plot_id)]
    num_seq <- ceiling(max_plot_id / rates_len + 1)

    if ((i %% rates_len) == 1) {
      if (i == 1) {
        #--- the very first ---#
        rates_seq <- get_seq_for_strip(rate_ranks_seq, num_seq, 0)
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      } else {
        rates_seq <- get_seq_for_strip(
          rate_ranks_seq,
          num_seq,
          #--- avoid having the same rate right next to it in the previous block ---#
          exclude_ls = init_rate_memeory[rates_len]
        )
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      }
    } else {
      rates_seq <- get_seq_for_strip(
        rate_ranks_seq = rate_ranks_seq,
        num_seq = num_seq,
        exclude_ls = init_rate_memeory
      )
      init_rate_memeory <- c(init_rate_memeory, which(rates_seq[1] == rate_ranks_seq))
    }

    design_data_ls[[i]] <- data.table(
      plot_id = seq_len(max_plot_id),
      strip_id = i,
      rate_rank = rates_seq
    )

    # print(rates_seq)
    # print(init_rate_memeory)

  }

  design_data <- rbindlist(design_data_ls)

  # design_data[, .N, by = rate_rank]

  if (merge == TRUE) {
    data <- left_join(data_sf, design_data, by = c("plot_id", "strip_id")) %>%
      left_join(., rates_data, by = "rate_rank")
    return(data)
  } else {
    design_data <- left_join(design_data, rates_data, by = "rate_rank")
    return(design_data)
  }

}

#/*=================================================*/
#' # Tilt the field
#/*=================================================*/

st_tilt <- function(data_sf, angle) {

  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  wf_bbox <- st_bbox(data_sf)
  data_geom <- st_geometry(data_sf)

  base_point <- c(wf_bbox["xmax"], wf_bbox["ymin"]) %>%
    st_point() %>%
    st_sfc()

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf)) 

  data_sf$geometry <- data_tilted 

  return(data_sf)

}

#/*=================================================*/
#' # Shift the field
#/*=================================================*/

st_shift <- function(data_sf, shift) {

  data_geom <- st_geometry(data_sf)
  
  shift_sfc <- st_point(shift) %>% st_sfc()

  data_shifted <- data_geom + shift_sfc %>%
    st_set_crs(st_crs(data_sf)) 

  data_sf$geometry <- data_shifted 

  return(data_sf)

}




