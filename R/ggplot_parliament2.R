#' Create an Arc-Bar Parliament Diagram
#' 
#' An arc bar diagram that allows for spacing between the individual arc components 
#' and spans 180 degrees.
#'   
#' @param parties A vector of parties that make up the diagram
#' @param shares A vector of the parties' member count or proportion of seats in parliament
#' @param party_colors A vector of colors mapped to each party
#' @param type The type of shares passed. Absolute for counts, proportion for proportions. Defaults to absolute
#' @param border_color Border color of the arc. Defaults to black
#' @param text_total Boolean indicator for whether to display the total number of members inside the diagram. Defaults to TRUE
#' @param autosort Sort the parties from most to least members. Defaults to FALSE.
#' @param rad_inner Limiting inner radius of the diagram. Defaults to 1.5
#' @param rad_outer Limiting outer radius of the diagram. Defaults to 3
#' @param arc_n Number of points used to draw arc polygons for each party Defaults to 360
#' @param sep Total spacing between parties as a proportion of pi. Defaults to 0.05
#' @export
#' 
#' @examples
#' # Generate Data
#' # Generate Data
#' bt <- data.frame(
#'         parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
#'         seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#'         colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
#'         stringsAsFactors = FALSE)
#' ggplot_parliament2(bt$parties, bt$seats, bt$colors, sep = 0.05)

ggplot_parliament2 <- function(
  parties, 
  shares, 
  party_colors = NA, 
  type = c("absolute", "proportion"),
  border_color = "black",
  text_total = TRUE,
  autosort = FALSE,
  rad_inner = 1.5,
  rad_outer = 3,
  arc_n = 360,
  sep = 0.05
) {
  
  type <- match.arg(type)
  
  stopifnot(length(parties) == length(shares), 
            (is.na(party_colors) | length(party_colors) == length(parties)), 
            (type == "absolute" | sum(shares) == 1))
  
  text_total <- if (text_total & type == "proportion") FALSE else text_total
  
  if (autosort) {
    parties <- parties[order(shares, decreasing = TRUE)]
    party_colors <- party_colors[order(shares, decreasing = TRUE)]
    shares <- rev(shares[order(shares, decreasing = TRUE)])
  } else {
    shares <- rev(shares)
  }
  
  shares_new <- switch(type, 
                       "absolute" = (shares / sum(shares)), 
                       "proportion" = shares)

  if (sep > 0) {
    shares_new <- shares_new * (1 - sep)
    sep_shares <- rep(shares_new, c(rep(2, length(shares_new) - 1), 1))
    sep_shares[seq(2, (length(sep_shares) - 1), 2)] <- sep / (length(shares_new) - 1)
    
    cc <- rev(cumsum(c(0, sep_shares * pi)))
    cc[1] <- pi
    cc[length(cc)] <- 0
    cc <- rbind(cc[seq(1, length(cc), 2)], cc[seq(2, length(cc), 2)])
  } else {
    cc <- rev(cumsum(c(0, shares_new * pi)))
  }

  df <- as.data.frame(
    do.call(
      rbind, 
      lapply(1:length(parties), function(i) {
      arc_seq <- seq(cc[1, i], cc[2, i], length.out = arc_n)
      cbind(
        c(
          cos(arc_seq) * rad_outer,
          rev(cos(arc_seq) * rad_inner)
          ),
        c(
          sin(arc_seq) * rad_outer,
          rev(sin(arc_seq) * rad_inner)
          ),
        as.numeric(i))
      }
      )
    )
  )
  
  df[, 3] <- as.factor(df[, 3])
  levels(df[, 3]) <- parties

  p <- ggplot() + 
    coord_fixed() +
    theme_void() + 
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    guides(fill = guide_legend(title = "Parties")) +
    geom_polygon(
      aes(
        x = df[, 1], 
        y = df[, 2], 
        group = df[, 3], 
        fill = df[, 3]
        ), 
      color = as.character(border_color)
      ) +
    geom_text(
      aes(x = 0, 
          y = 0.1, 
          label = switch(text_total + 1, "", sum(shares))
      ), 
      fontface = "bold", 
      size = 17) +
    if (is.character(party_colors)) scale_fill_manual(values = party_colors) else NULL
  
  return(p)
  
}
