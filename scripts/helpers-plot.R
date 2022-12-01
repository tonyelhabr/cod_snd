library(ggplot2)
library(ggforce)
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
  plot.title = ggplot2::element_text(face = 'bold', size = 20, color = 'white'),
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

autoplot.wp_model_df <- function(df, round_id, side = c('o', 'd'), ...) {
  stopifnot(length(round_id) == 1)
  side <- match.arg(side)
  
  filt <- df |> 
    dplyr::filter(.data[['round_id']] == !!round_id, .data[['side']] == !!side)
  
  if (nrow(filt) == 0) {
    stop(
      'Could not find round_id or side in data.'
    )
  }
  
  side_lab <- switch(
    side,
    'o' = 'Offensive',
    'd' = 'Defensive'
  )
  
  prepped <- filt |> 
    dplyr::distinct(engagement_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      # 'hjust' = case_when(
      #   activity == 'Start' ~ 'right',
      #   TRUE ~ 'left'
      # )
      'label' = dplyr::case_when(
        is.na(.data[['activity']]) ~ '???',
        .data[['activity']] == 'Start' ~ 'Start',
        .data[['activity']] == 'Plant' ~ sprintf('%s (%s) plants', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Defuse' ~ sprintf('%s (%s) defuses', .data[['activity_opponent']], .data[['activity_player']]),
        .data[['activity']] == 'Kill' ~ sprintf('%s (%s) kills\n%s (%s)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Planter' ~ sprintf('%s (%s) kills\n%s %s (planting)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Defuser' ~ sprintf('%s (%s) kills\n%s (%s) (defusing)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Self Kill' ~ sprintf('%s (%s) self-kills', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Team Kill' ~ sprintf('%s (%s)\nteam-kills %s', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']])
      )
    )
  
  prepped |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_point() +
    ggplot2::geom_step() +
    ggforce::geom_mark_circle(
      label.colour = 'white',
      label.fill = 'transparent',
      con.colour = 'white',
      label.family = font,
      label.fontsize = 12,
      label.fontface = 'plain',
      # label.buffer = 0,
      expand = unit(0.01, 'mm'),
      con.linetype = 2,
      con.border = 'none',
      aes(
        group = .data[['label']],
        label = .data[['label']]
      )
    ) +
    # ggrepel::geom_text_repel(
    #   seed = 42,
    #   min.segment.length = 0,
    #   family = font,
    #   segment.colour = 'white',
    #   colour = 'white',
    #   size = 12 / .pt,
    #   nudge_x = -0.25,
    #   direction = 'x',
    #   force = 0.5,
    #   hjust = 0,
  #   aes(
  #     # hjust = .data[['hjust']]
  #     label = .data[['label']]
  #   )
  # ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
    ggplot2::labs(
      title = sprintf('%s win probability', side_lab),
      y = 'Win probability',
      x = 'Seconds elapsed'
    )
}
