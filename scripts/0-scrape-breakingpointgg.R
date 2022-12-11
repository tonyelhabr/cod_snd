library(rvest)
library(tibble)
library(fs)
library(stringr)
library(purrr)
library(cropcircles)

local_dir <- file.path('figs', 'img')
fs::dir_create(local_dir, recurse = TRUE)

scrape_team_logos <- function(season) {
  url <- sprintf('https://breakingpoint.gg/table/call-of-duty-league-%s/', season)
  page <- rvest::read_html(url)
  
  logo_elements <- rvest::html_elements(page, 'tbody > tr > td > a')

  tibble(
    team = rvest::html_text2(logo_elements),
    logo_url =  logo_elements |> 
      rvest::html_elements('span.team-logo > img') |>
      rvest::html_attr('src')
  ) |> 
    deframe() |> 
    walk(
      ~{
        path <- file.path('figs', 'img', basename(.x))
        if (fs::file_exists(path)) {
          return(path)
        }
        download.file(.x, destfile = path, mode = 'wb')
        temp_path <- cropcircles::circle_crop(path)
        path_cropped <- str_replace(path, '[.]png', '_cropped.png')
        fs::file_move(temp_path, path_cropped)
      }
    )
}

c(2021, 2022, 2023) |> walk(scrape_team_logos)
