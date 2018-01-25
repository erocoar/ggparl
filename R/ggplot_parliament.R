#' Create a Parliament Diagram
#'
#' Draws a parliament diagram based on parties' member counts, names and colors, 
#' where each point in the arc represents a single member of parliament. 
#' Parties are plotted left to right in the order of the input vector, 
#' unless `autosort` is set to `TRUE`, in which case they are sorted from most to least members.
#' 
#' @param parties A vector of parties that make up the diagram
#' @param members A vector of the parties' member count
#' @param party_colors A vector of colors mapped to each party
#' @param border_color Border color of the individual circles. Defaults to NA
#' @param text_total Boolean indicator for whether to display the total number of members inside the diagram. Defaults to TRUE
#' @param autosort Sort the parties from most to least members. Defaults to FALSE
#' @param rad_inner Limiting inner radius of the diagram. Defaults to 1.5
#' @param rad_outer Limiting outer radius of the diagram. Defaults to 3
#' @param circle_n Number of points used to draw circle polygons for each member. Defaults to 360
#' @export
#' 
#' @examples 
#' # Generate Data
#' bt <- data.frame(
#'         parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
#'         seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#'         colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
#'         stringsAsFactors = FALSE)
#' ggplot_parliament(bt$parties, bt$seats, bt$colors)

ggplot_parliament <- function(
  parties, 
  members, 
  party_colors = NA, 
  border_color = NA,
  text_total = TRUE,
  autosort = FALSE,
  rad_inner = 1.5,
  rad_outer = 3,
  circle_n = 360
) {

  stopifnot(length(parties) == length(members), 
            (all(is.na(party_colors)) | length(party_colors) == length(parties)))
  
  row_coords <- function(r, n, start_angle = 0, end_angle = 0.5) {
    angles <- seq(start_angle, end_angle, length.out = n) * pi * 2
    matrix(c(cos(angles) * r, sin(angles) * r), ncol = 2)
  }
  
  circles <- function(points, radius) {
    angle <- seq(-pi, pi, length.out = circle_n)
    make_circle <- function(x, y, r, id) {
      data.frame(x = x + r * cos(angle), y = y + r * sin(angle), id)
    }
    
    circle_mat <- mapply(make_circle, 
                   id = seq_len(nrow(points)), 
                   x = points[, 1], y = points[, 2], r = radius, 
                   SIMPLIFY = FALSE)
    do.call(rbind, circle_mat)
  }

  n <- sum(members)
  rows <- 0
  n_treated <- 0

  if (autosort) {
    parties <- parties[order(members)]
    party_colors <- party_colors[order(members)]
    members <- members[order(members)]
  } else {
    members <- rev(members)
    parties <- rev(parties)
    party_colors <- rev(party_colors)
  }

  while (n_treated < n) {
    rows <- rows + 1
    point_rad <- 0.6 / rows
    arc_start <- arc_end <- point_rad
    row_centers <- seq(rad_inner + point_rad, 
                       rad_outer - point_rad, 
                       length.out = rows)
    row_sums <- floor(pi / (2 * asin(point_rad / (row_centers - point_rad))))
    n_treated <- sum(row_sums)
  }
  
  ratio <- if (n_treated > n) round((row_sums / sum(row_sums)) * n) else row_sums

  if (sum(ratio) != sum(members)) {
    diff <- sum(ratio) - sum(members)
    rs <- row_sums/sum(row_sums) * sum(members)
    idx <- order(rs - floor(rs), 
                 decreasing = diff < 0)
    ratio[idx[1:abs(diff)]] <- ratio[idx[1:abs(diff)]] + 
      switch((diff < 0) + 1, -1, 1)
  }
  
  arcs <- do.call(rbind, sapply(1:rows, function(i) {
    row_coords(row_centers[i], ratio[i])
    }, simplify = FALSE))
  
  color <- vector("numeric", sum(members))
  
  angles <- order(atan2(arcs[, 2], arcs[, 1]))
  
  for (i in 1:length(members)) {
    if (i == 1) {
      color[angles[1:members[1]]] <- 1
    } else {
      color[angles[sum(c(1, members[1:(i-1)])):sum(members[1:i])]] <- i
    }
  }

  color <- factor(rep(color, each = circle_n), labels = parties)
  circle_data <- circles(arcs, point_rad)
  
  p <- ggplot() +
    coord_fixed() + 
    theme_void() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    guides(fill = guide_legend(title = "Parties", reverse = TRUE)) +
    geom_polygon(
      aes(x = x, 
          y = y, 
          group = id,
          fill = color
          ),
      color = as.character(border_color),
      data = circle_data
      ) +
    geom_text(
      aes(x = 0, 
          y = 0, 
          label = switch(text_total + 1, "", sum(members))
          ), 
      fontface = "bold", 
      size = 17) +
    if (is.character(party_colors)) scale_fill_manual(values = party_colors) else NULL

  return(p)
}
