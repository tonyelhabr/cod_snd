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
        img <- magick::image_read(path_cropped)
        magick::image_border(img, color = 'red')
        fs::file_move(temp_path, path_cropped)
      }
    )
}

c(2021, 2022, 2023) |> walk(scrape_team_logos)

library(magick)
url <- 'https://breakingpoint.gg/wp-content/uploads/2022/01/Boston-Breach-128x122.png'
temp_file <- tempfile(fileext = '.png')

download.file(
  url,
  destfile = temp_file,
  mode = 'wb'
)
img <- image_read(temp_file)

dat <- image_data(img, "rgba")
dims <- dim(dat)
center <- floor(dims[2:3]/2)
r <- floor(min(dims[2:3])/2)
start_point <- round(center-r)
depth <- 2*r
geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
cropped_img <- image_crop(img, geom)


d1 <- image_draw(cropped_img)


info <- image_info(img)

# size may have changed after refit
size <- min(info$height, info$width)

is_image_square <- info$width == info$height


size = 50
image_crop(
  img,
  geometry = geometry_area(
    width = size,
    height = size,
    x_off = center[[1]],
    y_off = center[[2]]
  )
)


dat <- image_data(img, "rgba")
dims <- dim(dat)
center <- floor(dims[2:3]/2)
r <- floor(min(dims[2:3])/2)
start_point <- round(center-r)
depth <- 2*r
geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
img <- image_crop(img, geom)

# crop to a circle
dat <- image_data(img, "rgba")
dims <- dim(dat)
center <- floor(dims[2:3]/2)

x_vals <- 1:dims[2]
y_vals <- 1:dims[3]

for(x in x_vals) {
  d <- sqrt((x - center[1])^2 + (y_vals - center[2])^2)
  outside <- which(d > r)
  dat[4, x, outside] <- as.raw(00)
}
