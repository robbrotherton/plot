#' See how a piece will look when framed
#'
#' @param df A data frame of coordinates (and optionally grouping and color
#'   columns)
#' @param paper_size String specifying the size of paper used for plotting,
#'   either in inches (e.g. "5x7") or one of "letter", "A4", "A3".
#' @param frame_size String specifying the size of the frame in inches (e.g.
#'   "11x14")
#' @param frame_depth The thickness of the frame, in inches
#' @param matt_size Optional string specifying the size of size of the frame in
#'   inches (e.g. "8x10")
#' @param matt_color Color of the matt
#' @param frame_color Color of the frame
#' @param show_paper_edges Logical; show red overlay where the edges of the
#'   paper are
#'
#' @return A ggplot object
#' @export
#'
#' @examples
frame <- function(df,
                  paper_size = "A4",
                  frame_size = "11x14",
                  frame_depth = .5,
                  matt_size = NULL,
                  matt_color = "snow1",
                  frame_color = "#B39671",
                  show_paper_edges = FALSE) {

  col_names <- colnames(df)

  # Check for group var; if missing, add one with warning
  has_group_var <- "group" %in% col_names

  if (!has_group_var) {
    df$group <- 1
    message("No group variable present.")
  }

  # Check for color var
  has_color_var <- "color" %in% col_names

  # Make base plot, with or without colors
  if (has_color_var) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_path(data = df, ggplot2::aes(x, y, group = group, color = color)) +
      ggplot2::scale_color_identity()
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_path(data = df, ggplot2::aes(x, y, group = group))
  }

  p + geom_frame(paper_size,
                 frame_size,
                 frame_depth,
                 matt_size,
                 matt_color,
                 frame_color,
                 show_paper_edges)

}


make_frame <- function(x, y, cx, cy, w, fill, line = NA) {
  x <- x/2
  y <- y/2
  xCoords <- c(cx-x-w,cx+x+w,cx+x+w,cx-x,cx-x,cx-x-w,cx-x-w)
  yCoords <- c(cy+y+w,cy+y+w,cy+y,cy+y,cy-y-w,cy-y-w,cy+y+w)

  ggplot2::geom_polygon(inherit.aes = FALSE,
                        data = NULL,
                        ggplot2::aes(
                          x = c(xCoords, -xCoords+cx*2),
                          y = c(yCoords, -yCoords+cy*2),
                          group = rep(1:2, each = 7)
                        ), fill = fill, color = line)
}

parse_size <- function(size) {

  if(!stringr::str_detect(size, "x"))  {
    size <- switch(toupper(size),
                   "A4" = "8.25x11.75",
                   "A3" = "11.75x16.5",
                   "LETTER" = "8.5x11")
  }

  width  <- strsplit(size, "[x]")[[1]][1] |> as.numeric()
  height <- strsplit(size, "[x]")[[1]][2] |> as.numeric()

  c(width, height) * 25.4

}


geom_frame <- function(paper_size = "A4",
                       frame_size = "11x14",
                       frame_depth = .5,
                       matt_size = NULL,
                       matt_color = "snow1",
                       frame_color = "#B39671",
                       show_paper_edges = FALSE) {

  out <- list()

  if(stringr::str_detect(paper_size, "x"))  {
    paper_width <- strsplit(paper_size, "[x]")[[1]][1] |> as.numeric()*25.4
    paper_height <- strsplit(paper_size, "[x]")[[1]][2] |> as.numeric()*25.4
  }
  if(paper_size=="A4") {
    paper_width <- 8.25*25.4
    paper_height <- 11.75*25.4
  }
  if(paper_size=="A3") {
    paper_width <- 11.75*25.4
    paper_height <- 16.5*25.4
  }

  frame_width <- strsplit(frame_size, "[x]")[[1]][1] |> as.numeric()*25.4
  frame_height <- strsplit(frame_size, "[x]")[[1]][2]|> as.numeric()*25.4

  if(!is.null(matt_size)) {
    matt_width <- strsplit(matt_size, "[x]")[[1]][1]  |> as.numeric()*25.4
    matt_height <- strsplit(matt_size, "[x]")[[1]][2] |> as.numeric()*25.4
    matt_depth <- (frame_width - matt_width)/2
  }

  frame_depth <- frame_depth*25.4

  xcenter <- paper_width/2
  ycenter <- paper_height/2

  if(!is.null(matt_size)) {
    out[[length(out)+1]] <- make_frame(matt_width,
                                       matt_height,
                                       xcenter,
                                       ycenter,
                                       w = matt_depth,
                                       fill = matt_color)

    out[[length(out)+1]] <- make_frame(matt_width,
                                       matt_height,
                                       xcenter,
                                       ycenter,
                                       w = 0,
                                       fill = NA,
                                       line = 'grey')
  }

  out[[length(out)+1]] <- make_frame(frame_width,
                                     frame_height,
                                     xcenter,
                                     ycenter,
                                     frame_depth,
                                     fill = frame_color)

  if(show_paper_edges) {

    out[[length(out)+1]] <- make_frame(paper_width,
                                       paper_height,
                                       xcenter,
                                       ycenter,
                                       w = 0,
                                       fill = NA,
                                       line = 'red')
  }

  list(out, ggplot2::coord_fixed(), ggplot2::theme_void())

}

