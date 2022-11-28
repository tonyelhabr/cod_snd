
library(ggplot2)
blackish_background <- '#1c1c1c' ## to match slide background
gray_points <- '#4d4d4d'
gray_text <- '#999999'
font <- 'Titillium Web'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  title = element_text(size = 14, color = 'white'),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.title.position = 'plot',
  plot.subtitle = element_text(size = 14, color = '#f1f1f1'),
  axis.text = element_text(color = 'white', size = 14),
  axis.title = element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = gray_points),
  panel.grid.minor = element_line(color = gray_points),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  strip.text = element_text(color = 'white', size = 12, face = 'bold'),
  legend.title = element_text(color = 'white', size = 12, face = 'bold'),
  legend.text = element_text(color = 'white', size = 12, face = 'plain'),
  plot.margin = margin(10, 10, 10, 10),
  plot.background = element_rect(fill = blackish_background, color = blackish_background),
  plot.caption = element_text(size = 12, color = 'white', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 12, color = 'white', hjust = 0),
  plot.tag.position = c(0.01, 0.01),
  panel.background = element_rect(fill = blackish_background, color = blackish_background)
)
update_geom_defaults('text', list(family = font, size = 10 / .pt, fontface = 'bold'))
update_geom_defaults('point', list(color = 'white'))
update_geom_defaults('segment', list(color = 'white'))
update_geom_defaults('step', list(color = 'white'))
