library(rvest)
library(tidyverse)
main_url <- 'https://www.degruyter.com/journal/key/jqas/html?lang=en'
main_page <- main_url |> read_html()
nodes <- main_page |> 
  html_elements('.metadataInfoFont > p > a')
info <- tibble(
  name = nodes |> html_text2(),
  href = nodes |> html_attr('href')
)

parse_jqas_html_paper_headers <- function(href) {
  page <- href |> read_html()
  list(
    'h2' = page |> html_elements('h2.subheading') |> html_text2(),
    'h3' = page |> html_elements('h3.subheading') |> html_text2()
  ) |> 
    enframe('level', 'title') |> 
    unnest(title)
}

possibly_parse_jqas_html_paper_headers <- possibly(
  parse_jqas_html_paper_headers,
  otherwise = tibble(),
  quiet = FALSE
)

headers <- info |> 
  mutate(
    headers = map(href, possibly_parse_jqas_html_paper_headers)
  )

headers <- headers |> 
  unnest(headers) |> 
  mutate(
    index = title |> str_replace_all('(^[0-9.]+)\\s+(.*$)', '\\1'),
    h2 = index |> str_remove('[.].*$'),
    h3 = index |> str_remove('^[0-9+][.]'),
    across(h3, ~ifelse(index == h2, NA_character_, .x)),
    across(c(h2, h3), as.integer),
    title = title |> str_replace_all('(^[0-9.]+)\\s+(.*$)', '\\2')
  )
write_csv(headers, 'research/jqas_headers.csv', na = '')

headers |> 
  count(title, sort = TRUE)
headers |> 
  count(h2, title, sort = TRUE)
headers |> 
  drop_na(h3) |> 
  count(h3, title, sort = TRUE)
