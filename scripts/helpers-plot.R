library(ggplot2)
library(ggforce)
library(ggtext)
library(extrafont)
library(dplyr)

blackish_background <- '#1c1c1c' ## to match slide background
gray_points <- '#4d4d4d'
gray_text <- '#999999'
font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = font),
  title = ggplot2::element_text(size = 14, color = 'white'),
  plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = ggplot2::element_text(size = 14, color = '#f1f1f1'),
  axis.text = ggplot2::element_text(color = 'white', size = 14),
  axis.title = ggplot2::element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(color = gray_points),
  panel.grid.minor = ggplot2::element_line(color = gray_points),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(color = 'white', size = 12, face = 'bold'),
  legend.title = ggplot2::element_text(color = 'white', size = 12, face = 'bold'),
  legend.text = ggplot2::element_text(color = 'white', size = 12, face = 'plain'),
  plot.margin = ggplot2::margin(10, 10, 10, 10),
  plot.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = ggplot2::element_text(size = 12, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggplot2::element_text(size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background)
)
ggplot2::update_geom_defaults('text', list(family = font, size = 10 / .pt, fontface = 'bold'))
ggplot2::update_geom_defaults('point', list(color = 'white'))
ggplot2::update_geom_defaults('segment', list(color = 'white'))
ggplot2::update_geom_defaults('step', list(color = 'white'))
ggplot2::update_geom_defaults('line', list(color = 'white'))


summarize_pred_grid_across_features <- function(df, binary_feature_name) {
  df |> 
    group_by(
      .data[['is_pre_plant']],
      .data[['model_seconds_elapsed']],
      .data[['opponent_diff']],
      value = .data[[binary_feature_name]]
    ) |> 
    summarize(
      'wp' = mean(.data[['wp']])
    ) |> 
    ungroup()
}

# RColorBrewer::brewer.pal(7, 'PRGn') |> datapasta::vector_paste_vertical()
player_diff_pal <- c(
  '#762A83',
  '#AF8DC3',
  '#E7D4E8',
  '#F7F7F7',
  '#D9F0D3',
  '#7FBF7B',
  '#1B7837'
) |> 
  rlang::set_names(as.character(seq(-3, 3, by = 1)))

scale_x_continuous_wp_states <- function(...) {
  list(
    ...,
    ggplot2::scale_x_continuous(
      breaks = c(
        0, 
        max_pre_plant_second / 2, 
        max_pre_plant_second, 
        max_pre_plant_second + max_pre_plant_second_buffer / 2, 
        max_pre_plant_second + max_pre_plant_second_buffer, 
        max_pre_plant_second + max_pre_plant_second_buffer + 25, 
        max_pre_plant_second + max_pre_plant_second_buffer + 45
      ),
      labels = c('90', '45', '', 'Plant', '', '20', '0')
    ),
    ggplot2::theme(
      legend.position = 'top'
    ),
    ggplot2::labs(
      x = 'Seconds remaining'
    )
  )
}

# weights_by_second <- seconds_grid |>
#   dplyr::transmute(
#     'model_seconds_elapsed' = .data[['min_second']],
#     'n' = purrr::map2_int(
#       .data[['min_second']], 
#       .data[['max_second']], 
#       ~dplyr::filter(
#         all_model_pbp,
#         .data[['model_seconds_elapsed']] >= ..1, 
#         .data[['model_seconds_elapsed']] <= ..2
#       ) |> nrow()
#     )
#   ) |> 
#   dplyr::mutate('scaler' = .data[['n']] / max(.data[['n']]))
# scaler_by_second <- \(x) x^(-0.5)
plot_wp_grid <- function(data, feature_name, ...) {
  
  filt <- dplyr::filter(
    data, 
    .data[['side']] == default_side
  )
  
  agg <- if (!is.null(feature_name)) {
    filt |> 
      group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']],
        'feature_value' = .data[[feature_name]]
      ) |> 
      summarize(
        'wp' = mean(.data[['wp']])
      ) |> 
      ungroup() |> 
      mutate(
        'feature' = sprintf(
          '%s: %s',
          feature_name,
          .data[['feature_value']]
        )
      )
  } else {
    filt |> 
      group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']]
      ) |> 
      summarize(
        'wp' = mean(.data[['wp']])
      ) |> 
      ungroup()
  }
  
  df <- dplyr::bind_rows(
    dplyr::filter(agg, .data[['is_pre_plant']]),
    agg |> 
      dplyr::filter(!.data[['is_pre_plant']]) |> 
      dplyr::mutate(
        'model_seconds_elapsed' = .data[['model_seconds_elapsed']] + max_pre_plant_second + max_pre_plant_second_buffer
      )
  )
  
  p <- df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['model_seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_hline(
      color = 'white',
      ggplot2::aes(yintercept = 0.5)
    ) +
    ggplot2::geom_vline(
      color = 'white',
      data = tibble::tibble(),
      ggplot2::aes(xintercept = max_pre_plant_second)
    ) +
    ggplot2::geom_step(
      size = 1,
      ggplot2::aes(color = factor(.data[['opponent_diff']]))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = 'Net # of players',
        override.aes = list(size = 3)
      )
    ) +
    ggplot2::scale_color_manual(values = player_diff_pal) +
    scale_x_continuous_wp_states() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::labs(
      title = 'Offensive Win Probability',
      y = 'Win probability'
    )
  
  if (is.null(feature_name)) {
    return(p)
  }
  
  p +
    ggplot2::facet_wrap(~.data[['feature']])
}


side_labs_mapping <- c(
  'o' = 'offense',
  'd' = 'defense'
)

plot_round <- function(
    model,
    data,
    round_id,
    side = 'o',
    expand = FALSE,
    save = TRUE,
    path = file.path('figs', sprintf('round_id=%s&side=%s&expand=%s.png', round_id, side, tolower(expand))),
    ggsave.args = list(
      height = 10,
      width = 10,
      units = 'in'
    )
) {
  
  stopifnot(length(round_id) == 1)
  side <- rlang::arg_match(side, c('o', 'd'))
  
  round <- dplyr::filter(
    data,
    .data[['round_id']] == !!round_id
  )
  
  ## should be first row
  meta <- dplyr::filter(
    round,
    is.na(.data[['pbp_side']])
  )
  
  # o_team <- dplyr::filter(
  #   meta,
  #   .data[['side']] == 'o'
  # )
  # 
  # d_team <- dplyr::filter(
  #   meta,
  #   .data[['side']] == 'd'
  # )
  
  filt <- dplyr::filter(
    round, 
    .data[['side']] == !!side
  )
  
  side_meta <- dplyr::filter(
    meta,
    .data[['side']] == !!side
  )
  
  win_round <- as.character(unique(filt[['win_round']])) == 'yes'
  
  last_row <- dplyr::slice_max(
    filt,
    .data[['seconds_elapsed']], 
    n = 1, 
    with_ties = FALSE
  )
  
  filt <- dplyr::add_row(
    filt,
    last_row |> 
      dplyr::mutate(
        'activity' = 'End',
        'seconds_elapsed' = .data[['seconds_elapsed']] + 1
      )
  )
  
  round_has_plant <- unique(filt$round_has_plant)
  
  if (isTRUE(expand)) {
    f_line <- ggplot2::geom_line
    
    seconds_elapsed_range <- range(filt[['seconds_elapsed']])
    pre_plant_seconds_elapsed_range <- filt |> 
      dplyr::filter(.data[['is_pre_plant']]) |> 
      dplyr::pull(.data[['model_seconds_elapsed']]) |> 
      range()
    
    pre_plant_seconds <- filt |> 
      dplyr::filter(.data[['is_pre_plant']]) |> 
      dplyr::pull(.data[['model_seconds_elapsed']]) |> 
      unique()
    
    pre_plant_seconds_seq <- c(
      seq(pre_plant_seconds_elapsed_range[1], pre_plant_seconds_elapsed_range[2]),
      pre_plant_seconds
    ) |> 
      unique() |> 
      sort()
    
    ## TODO: Does this work for non-integer seconds?
    grid <- tibble::tibble(
      'model_seconds_elapsed' = pre_plant_seconds_seq,
      'is_pre_plant' = rep(TRUE, length(pre_plant_seconds_seq))
    ) |> 
      dplyr::mutate('seconds_elapsed' = .data[['model_seconds_elapsed']])
    
    if (isTRUE(round_has_plant)) {
      
      post_plant_seconds_elapsed_range <- filt |> 
        dplyr::filter(!.data[['is_pre_plant']]) |> 
        dplyr::pull(.data[['model_seconds_elapsed']]) |> 
        range()
      
      post_plant_seconds <- filt |> 
        dplyr::filter(!.data[['is_pre_plant']]) |> 
        dplyr::pull(.data[['model_seconds_elapsed']]) |> 
        unique()
      
      
      post_plant_seconds_seq <- c(
        seq(pre_plant_seconds_elapsed_range[1], pre_plant_seconds_elapsed_range[2]),
        pre_plant_seconds
      ) |> 
        unique() |> 
        sort()
      
      grid <- bind_rows(
        grid,
        tibble::tibble(
          'model_seconds_elapsed' = post_plant_seconds_seq,
          'is_pre_plant' = rep(FALSE, length(post_plant_seconds_seq))
        ) |> 
          dplyr::mutate('seconds_elapsed' = pre_plant_seconds_elapsed_range[2] + .data[['model_seconds_elapsed']])
      )
    }
    
    df <- grid |> 
      dplyr::left_join(
        filt,
        by = c('model_seconds_elapsed', 'seconds_elapsed', 'is_pre_plant'),
        multiple = 'all'
      ) |> 
      tidyr::fill(
        # tidyselect::vars_select_helpers$all_of(setdiff(colnames(filt), join_cols)),
        get_all_features(is_pre_plant = TRUE, named = FALSE, method = 'lb'),
        .direction = 'down'
      )
  } else {
    f_line <- ggplot2::geom_step
    df <- filt
  }
  
  df <- augment(
    model, 
    df
  ) |> 
    dplyr::mutate(
      'wp' = ifelse(.data[['activity']] == 'End', dplyr::lag(.data[['wp']], n = 1), .data[['wp']])
    )

  
  if (nrow(filt) == 0) {
    stop(
      'Could not find round_id or side in data.'
    )
  }
  
  labels <- df |> 
    # dplyr::distinct(.data[['engagement_id']], .keep_all = TRUE) |> 
    dplyr::mutate(
      'label' = dplyr::case_when(
        is.na(.data[['activity']]) ~ '???',
        .data[['activity']] %in% c('Start', 'End') ~ NA_character_,
        .data[['activity']] == 'Plant' & .data[['is_pre_plant']] ~ NA_character_, # sprintf('%s (%s) plants (pre-plant)', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Plant' & !.data[['is_pre_plant']] ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) plants', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']]),
        .data[['activity']] == 'Defuse' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) defuses', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']]),
        .data[['activity']] == 'Kill' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) kills %s (<span style="color:%s"><b>%s</b></span>)', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent_color']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Planter' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>)  kills %s (<span style="color:%s"><b>%s</b></span>) (planting)', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']], .data[['activity_opposer']],.data[['activity_opponent_color']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Defuser' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) kills %s (<span style="color:%s"><b>%s</b></span>) (defusing)', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent_color']], .data[['activity_opponent']]),
        .data[['activity']] == 'Self Kill' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) self-kills', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']]),
        .data[['activity']] == 'Team Kill' ~ sprintf('%s (<span style="color:%s"><b>%s</b></span>) team-kills %s', .data[['activity_player']], .data[['activity_team_color']], .data[['activity_team']], .data[['activity_opposer']])
      )
    ) |> 
    dplyr::mutate(
      'wpa' = .data[['wp']] - lag(.data[['wp']]),
      across(
        .data[['label']],
        ~ifelse(
          !is.na(.x),
          sprintf('(%ss left): %s: <b>%+.0f</b>', .data[['model_seconds_remaining']], .x, 100 * .data[['wpa']]),
          .x
        )
      )
    )
  
  
  non_na_labels <- labels |> 
    dplyr::filter(
    !is.na(.data[['label']])
  ) |> 
    dplyr::mutate(
      'rn' = dplyr::row_number(),
      'y_base' = ifelse(.data[['rn']] %% 2, 1, 0),
      'max_rn' = max(.data[['rn']] ),
      'hjust' = ifelse(.data[['rn']]  >= (.data[['max_rn']] / 2), 1.05, -0.05),
      'y_buffer' = (.data[['rn']] - 1) %/% 2,
      'y' = .data[['y_base']] + ifelse(.data[['y_base']] == 1, 1, -1) * .data[['y_buffer']] * 0.05 
    )
  
  max_abs_label_y <- max(abs(non_na_labels[['y']]))
  rng_y <- c(-max_abs_label_y, max_abs_label_y)

  non_white_color <- gray_text # '#7F7F7F'
  team_color <- ifelse(win_round, 'white', non_white_color)
  opponent_color <- ifelse(!win_round, 'white', non_white_color)
  base <- ggplot2::ggplot(df)
  
  if (isTRUE(round_has_plant)) {
    base <- base +
      ggplot2::geom_vline(
        data = tibble::tibble(),
        ggplot2::aes(
          xintercept = unique(filt[['plant_second']])
        ),
        # alpha = 0.5,
        color = 'white',
        color = gray_text,
        linetype = 1,
        size = 1
      )
  }
  
  p <- base +
    ggplot2::aes(x = .data[['seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_vline(
      data = non_na_labels,
      linetype = 2,
      color = 'white',
      alpha = 0.5,
      ggplot2::aes(xintercept = .data[['seconds_elapsed']])
    ) +
    f_line()  +
    ggplot2::scale_y_continuous(
      limits = c(-0.2, 1.2),
      labels = scales::percent,
      breaks = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    ggplot2::labs(
      title = glue::glue(
        "
        <span style='color:{team_color}; font-size=18pt'>{side_meta$team_label} ({side_meta$team})
        <img src='{side_meta$team_logo_url}' width='15'/>
        </span> <span style='color:{team_color}; font-size=12pt'>{side_meta$team_round_wins + as.integer(win_round)}</span> 
        <span style='color:{non_white_color}; font-size=12pt'>-</span>
        <span style='color:{opponent_color}; font-size=18pt'>{side_meta$opponent_label} ({side_meta$opponent})</span>
        <img src='{side_meta$opponent_logo_url}' width='15'/>
        <span style='color:{opponent_color}; font-size=12pt'>{side_meta$opponent_round_wins + (1 - as.integer(win_round))}</span>
        "
    ),
      subtitle = sprintf(
        '%s (%s) win probability',
        side_meta[['team']],
        side_labs_mapping[side]
      ),
      y = 'Win probability',
      x = 'Seconds elapsed'
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(),
      panel.grid.major.x = ggplot2::element_blank()
    ) + 
    ggtext::geom_richtext(
      data = non_na_labels,
      family = font,
      size = 12 / .pt,
      color = 'white',
      label.size = NA,
      fill = blackish_background,
      ggplot2::aes(
        hjust = .data[['hjust']],
        x = .data[['seconds_elapsed']], 
        y = .data[['y']],
        label = .data[['label']]
      )
    )

  if (isFALSE(save)) {
    return(p)
  }
  
  rlang::exec(
    ggplot2::ggsave,
    plot = p,
    filename = path,
    !!!ggsave.args
  )
}

add_lb_plot_caption <- function(...) {
  list(
    ...,
    ggplot2::labs(
      caption = 'Method: LOESS glm models'
    )
  )
}

plot_coefs <- function(model, ...) {
  
  df <- dplyr::bind_rows(
    dplyr::mutate(model[['pre']][['coefs']], 'is_pre_plant' = TRUE),
    dplyr::mutate(
      model[['post']][['coefs']],
      'model_seconds_elapsed' = .data[['model_seconds_elapsed']] + max_pre_plant_second + max_pre_plant_second_buffer,
      'is_pre_plant' = FALSE
    )
  ) |> 
    tidyr::pivot_longer(
      -c(.data[['model_seconds_elapsed']], .data[['is_pre_plant']]),
      names_to = 'feature',
      values_to = 'value'
    )
  
  df$weights <- convert_seconds_to_weights_lb(df[['model_seconds_elapsed']], df[['is_pre_plant']])
  
  df |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data[['model_seconds_elapsed']],
      y = .data[['value']],
      group = .data[['is_pre_plant']],
      color = .data[['is_pre_plant']]
    ) +
    ggplot2::guides(color = 'none') +
    # geom_vline(
    #   color = 'white',
    #   aes(xintercept = max_pre_plant_second + max_pre_plant_second_buffer / 2)
    # ) +
    # geom_text(
    #   aes()
    # ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      # method.args = list(span = 0.5, df$weights),
      method = 'loess',
      formula = y ~ x
    ) +
    scale_x_continuous_wp_states() +
    ggplot2::facet_wrap(~.data[['feature']], scales = 'free_y') +
    add_lb_plot_caption() +
    ggplot2::labs(
      title = 'Offensive win probability',
      y = 'Coefficient estimate'
    )
}

autoplot.wp_model <- function(object, type = 'grid', ...) {
  type <- rlang::arg_match(type, c('grid', 'coefs', 'round'))
  
  cls <- class(object)
  if (type == 'coefs' & !any(cls == 'wp_model_lb')) {
    stop(
      sprintf(
        'Object must have class `wp_model_lb` for `type = "coefs"`. Object has class %s',
        paste0(cls, collapse = ', ')
      )
    )
  }
  switch(
    type,
    'grid' = plot_wp_grid(generate_wp_grid(object), ...),
    'coefs' = plot_coefs(object, ...),
    'round' = plot_round(object, ...)
  )
}
