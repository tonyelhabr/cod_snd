library(rvest)
library(tibble)
library(fs)
library(stringr)
library(purrr)
library(cropcircles)
url <- 'https://breakingpoint.gg/table/call-of-duty-league-2023/'
page <- read_html(url)

logo_elements <- page |> html_elements('tbody > tr > td > a')

local_dir <- file.path('figs', 'img')
fs::dir_create(local_dir, recurse = TRUE)

tibble(
  team = logo_elements |> html_text2(),
  logo_url =  logo_elements |> html_elements('span.team-logo > img') |> html_attr('src')
) |> 
  deframe() |> 
  walk(
    ~{
      path <- file.path('figs', 'img', basename(.x))
      download.file(.x, destfile = path, mode = 'wb')
      temp_path <- cropcircles::circle_crop(path)
      path_cropped <- str_replace(path, '[.]png', '_cropped.png')
      fs::file_move(temp_path, path_cropped)
    }
  )
