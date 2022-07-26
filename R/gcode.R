#' Write a g-code file from a data frame of coordinates
#'
#' @param data The input data. This should be a data frame containing x and y
#' columns and a group column. If a color column is also present multiple g-code
#' files will be written, one for each color group.
#' @param filename The filename of the resulting g-code file(s)
#' @param pic Optionally supply a ggplot2 object and it will be saved
#' alongside the g-code file
#'
#' @return
#' @export
#'
#' @examples
write_gcode <- function(data, filename, pic = NULL) {

  time <- plotting_time(data)

  if(min(data$x) < 0 |
     min(data$y) < 0 |
     max(data$x) > 215 |
     max(data$y) > 300) {
    warning("out of bounds")
  }


  start <- "M280P0S30\nG90 G21"
  pen_up <- "G4 P50\nM280P0S30\nG1 F3000"
  pen_down <- "G4 P50\nM280P0S0\nG1 F800"
  finish <- "G1 X0 Y0"

  if(!is.null(data$color)) {
    filestem <- gsub(".gcode", "", filename)
    colors <- unique(data$color)
    print(colors)
    n <- length(colors)
    files <- paste0(filestem, "_", colors, ".gcode")
  } else {
    files <- filename
    data$color <- 1
    colors <- 1
    n <- 1
  }

  for(j in 1:n) {

    df_list <- data |>
      dplyr::filter(color == colors[j]) |>
      dplyr::group_by(group) |>
      dplyr::group_split()

    r2 <- function(x) sprintf("%.2f", x)

    p <- purrr::map(seq_along(df_list), function(i) {
      c(paste0("G1 X", r2(df_list[[i]]$x[1]), " Y", r2(df_list[[i]]$y[1])),
        # need to duplicate coords to move while pen up between groups
        pen_down,
        paste0("G1 X", r2(df_list[[i]]$x), " Y", r2(df_list[[i]]$y)),
        pen_up)
    })

    # run this to write the .gcode text file
    write.table(start, files[j], append= F, row.names=F, col.names=F, quote=F)

    lapply(p, function(x) {
      write.table( data.frame(x), files[j], append= T, row.names=F, col.names=F, quote=F)
    })

    write.table(finish, files[j], append= T, row.names=F, col.names=F, quote=F)

    message(paste(files[j], "created"))

    message(paste("approx. plotting time:", time))

  }

  if(!is.null(pic)) {
    ggplot2::ggsave(plot = pic,
           filename = paste0(filename, ".png"),
           width = 8.25,
           height = 11.75)
  }

}


#' Read a g-code file into a data.frame
#'
#' @param file A path to a g-code file
#'
#' @return A data.frame with columns for x, y, and grouping variable.
#' @export
#'
#' @examples
read_gcode <- function(file) {

  data <- read.delim(file, col.names = "text") |>
    dplyr::filter(stringr::str_detect(text, "G1 X") |
                  stringr::str_detect(text, "G1 F800") |
                  stringr::str_detect(text, "G1 F3000")) |>
    dplyr::mutate(pen_up = stringr::str_detect(lag(text, default = "G1 F3000"), "G1 F3000")) |>
    dplyr::filter(!pen_up, !stringr::str_detect(text, "G1 F3000")) |>
    dplyr::mutate(group = cumsum(ifelse(stringr::str_detect(text, "G1 F"), 1, 0))) |>
    dplyr::filter(stringr::str_detect(text, "G1 X")) |>
    tidyr::separate(text, sep = " Y", into = c("x", "y")) |>
    dplyr::mutate(x = gsub(pattern = "G1 X", replacement = "", x = x)) |>
    dplyr::mutate_all(as.numeric) |>
    dplyr::select(-pen_up)

  # pic <- ggplot(data, aes(x, y, group = group)) +
  #   geom_path() +
  #   coord_fixed()
  #
  # print(pic)

  return(data)

}


#' Determine approximate plotting time from a data.frame
#'
#' @param df A data.frame or tibble containing x and y coordinates and
#' optionally a group column
#' @param plotter_speed The drawing speed of the pen plotter, in millimeters per second
#'
#' @return The approximate plotting time (HH:MM:SS)
#' @export
#'
#' @examples
plotting_time <- function(df, plotter_speed = 800) {

  # group by colors
  df <- dplyr::mutate(df, dist = sqrt((x-dplyr::lag(x, default = 0))^2+(y-dplyr::lag(y, default = 0))^2))

  total_distance <- sum(df$dist)

  approx_mins <- total_distance / plotter_speed

  chron::times(approx_mins/24/60)

}


# plotting_time <- function(gcode) {
#
#   # read in a gcode file, including pen up movements
#   data <- read.delim(gcode, col.names = "text") |>
#     dplyr::filter(stringr::str_detect(text, "G1 X")) |>
#     tidyr::separate(text, sep = " Y", into = c("x", "y")) |>
#     dplyr::mutate(x = gsub(pattern = "G1 X", replacement = "", x = x)) |>
#     dplyr::mutate_all(as.numeric) |>
#     dplyr::mutate(dist = sqrt((x-dplyr::lag(x, default = 0))^2+(y-dplyr::lag(y, default = 0))^2))
#
#   total_distance <- sum(data$dist)
#
#   approx_mins <- total_distance / 800
#
#   chron::times(approx_mins/24/60)
#
# }



#' Title
#'
#' @param df
#' @param paper_width
#' @param paper_height
#' @param max_dim
#'
#' @return
#' @export
#'
#' @examples
resize_for_plotter <- function(df,
                               paper_width = 210, paper_height = 297,
                               # max_width = paper_width*.9,
                               # max_height = paper_height*.9,
                               max_dim = NULL) {

  x_range <- max(df$x) - min(df$x)
  y_range <- max(df$y) - min(df$y)

  if(!is.null(max_dim)) {
    if(x_range >= y_range) {
      ratio <- max_dim/x_range
    } else {
      ratio <- max_dim/y_range
    }
  } else {
    if(x_range <= y_range) {
      ratio <- paper_width/x_range
    } else {
      ratio <- paper_height/y_range
    }
  }

  # scale to max dim
  df <- df |>
    dplyr::mutate(x = scale(x, center = 0, scale = 1/ratio),
                  y = scale(y, center = 0, scale = 1/ratio))

  # now nudge so min values are 0,0
  nudge_x <- min(df$x)
  nudge_y <- min(df$y)

  df <- df |>
    dplyr::mutate(x = x - nudge_x,
                  y = y - nudge_y)
  # df <- df |>
  #   mutate(x = x * ratio,
  #          # x = x - min(x),
  #          # x = x + (paper_width-max(x))*.5,
  #          y = y * ratio,
  #          # y = y - min(y),
  #          # y = y + (paper_height-max(y))*.5
  #   )
  #
  # now position in center of page
  image_center_x <- max(df$x)/2
  image_center_y <- max(df$y)/2
  page_center_x <- paper_width/2
  page_center_y <- paper_height/2
  # print(y_center)
  df <- df |>
    dplyr::mutate(x = x - (image_center_x - page_center_x),
                  y = y - (image_center_y - page_center_y)
    )

  # include a check here for out-of-bounds X and Y values
  if(min(df$x) < 0 | min(df$y) < 0 | max(df$x) > 215 | max(df$y) > 300) {
    warning("out of bounds")
  }

  return(df)

}
