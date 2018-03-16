library(ggplot2)
library(grid)

facet_share <- function(facets, scales = "fixed",
                        shrink = TRUE, labeller = "label_value", as.table = TRUE,
                        switch = NULL, drop = TRUE, dir = "h", strip.position = "top") {
  
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  
  free <- list(
    x = any(scales %in% c("free", "free_x")) & dir == "h",
    y = any(scales %in% c("free", "free_y")) & dir == "v"
  )
  
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right", "outer"))
  
  ggproto(NULL, FacetShare,
          shrink = shrink,
          params = list(facets = plyr::as.quoted(facets), free = free,
                        as.table = as.table, 
                        strip.position = strip.position,
                        drop = drop, 
                        labeller = labeller,
                        dir = dir)
  )
}

FacetShare <- ggproto("FacetShare", FacetWrap,
  shrink = TRUE,
                      
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if (inherits(coord, "CoordFlip")) {
      if (params$free$x) {
        layout$SCALE_X <- seq_len(nrow(layout))
      } else {
        layout$SCALE_X <- 1L
      }
      if (params$free$y) {
        layout$SCALE_Y <- seq_len(nrow(layout))
      } else {
        layout$SCALE_Y <- 1L
      }
    }
    
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    
    panels <- matrix(panels, nrow = nrow, ncol = ncol)
    panel_table <- gtable::gtable_matrix("layout", panels,
      widths = unit(rep(1, ncol), "null"), heights = unit(rep(1, nrow), "null"), clip = "on")
    
    panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
    
    panel_spacing <- switch(params$dir,
      "h" = if (is.null(theme$panel.spacing.x)) theme$panel.spacing else theme$panel.spacing.x,
      "v" = if (is.null(theme$panel.spacing.y)) theme$panel.spacing else theme$panel.spacing.y)
    
    panel_table$layout$name <- paste0("panel-", c(1, 2))
    
    labels_df <- layout[names(params$facets)]
    attr(labels_df, "facet") <- "wrap"
    strips <- render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme)
    
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
    
    if (params$dir == "h") {
      tick_idx <- grep("axis.ticks",
        sapply(axes$y$left[[1]]$children$axis$grobs, function(x) x$name))
      lab_idx <- (tick_idx == 1) + 1
      
      labs <- axes$y$left[[1]]$children$axis$grobs[[lab_idx]]
      labs$children[[1]]$hjust <- 0.5
      labs$children[[1]]$x <- unit(0.5, "npc")
      
      ax_tick_l <- ax_tick_r <- axes$y$left[[1]]$children$axis$grobs[[tick_idx]]
      tick_count <- length(ax_tick_r$x)
      ax_tick_r$x[seq(1, tick_count, 2)] <- unit(0, "npc")
      ax_tick_r$x[seq(2, tick_count, 2)] <- convertWidth(grobWidth(ax_tick_l), "pt")
    } else {
      tick_idx <- grep("axis.ticks",
       sapply(axes$x$bottom[[1]]$children$axis$grobs, function(x) x$name))
      lab_idx <- (tick_idx == 1) + 1
      
      labs <- axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]]
      labs$children[[1]]$vjust <- 0.5
      labs$children[[1]]$y <- unit(0.5, "npc")

      ax_tick_b <- ax_tick_t <- axes$x$bottom[[1]]$children$axis$grobs[[tick_idx]]
      ax_tick_t$y[seq(1, length(ax_tick_t$y), 2)] <- unit(0, "npc")
      ax_tick_t$y[seq(2, length(ax_tick_t$y), 2)] <- convertWidth(grobWidth(ax_tick_b), "pt")
    }
    
    if (params$dir == "h") {
      panel_table <- gtable::gtable_add_cols(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_cols(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_cols(panel_table, convertWidth(grobWidth(labs), "npc") * 1.5, 1)
      panel_table <- gtable::gtable_add_cols(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_cols(panel_table, panel_spacing*0.5, 1)
      
      panel_table <- gtable::gtable_add_grob(panel_table, ax_tick_l, l = 6, t = 1, clip = "on")
      panel_table <- gtable::gtable_add_grob(panel_table, labs, l = 4, t = 1, clip = "on")
      panel_table <- gtable::gtable_add_grob(panel_table, ax_tick_r, l = 2, t = 1, clip = "on")
    } else {
      panel_table <- gtable::gtable_add_rows(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_rows(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_rows(panel_table,
       convertHeight(grobHeight(labs), "npc") * 1.5, 1)
      panel_table <- gtable::gtable_add_rows(panel_table, panel_spacing*0.5, 1)
      panel_table <- gtable::gtable_add_rows(panel_table, panel_spacing*0.5, 1)

      panel_table <- gtable::gtable_add_grob(panel_table, ax_tick_b, t = 6, l = 1, clip = "on")
      panel_table <- gtable::gtable_add_grob(panel_table, ax_tick_b, t = 2, l = 1, clip = "on")
      panel_table <- gtable::gtable_add_grob(panel_table, labs, t = 4, l = 1, clip = "on")
    }
    
    ##############
  panel_table
  })


test <- data.frame(x = rep(seq_len(15), 2), y = rep(rnorm(15), 2),
                   fac = factor(rep(1:2, each = 15)))

# test$y[test$fac == 2] <- test$y[test$fac == 2] * -1
test$x[test$fac == 2] <- test$x[test$fac == 2] * -1

P <- ggplot(test, aes(x = x, y = y)) +
  geom_line(aes(color = fac), lwd = 1) +
  facet_share(~fac, dir = "h", scales = "free")
P
# 
# tt <- ggplot(test, aes(x = x, y = y)) + 
#   geom_line(aes(color = fac), lwd = 1) + 
#   scale_x_continuous(position = "top") +
#   facet_wrap(~fac, dir = "v", scales = "free")
# 




# mpg <- mpg
# mpg <- mpg[, c("class", "year")]
# mpg2 <- data.frame(rbind(mpg[mpg$year == 1999, ], mpg[mpg$year == 1999, ]))
# mpg2$year <- factor(rep(1:2, each = nrow(mpg2)/2))
# mpg2$class[mpg2$class=="2seater"] = "twoseater"

P <- ggplot(mpg2, aes(class)) + geom_bar(aes(y = ..count..)) + facet_share(~ year, dir = "v", scales = "free")
P

df <- data.frame(group = rep(c("Above", "Below"), each=10), x = rep(1:10, 2), y = c(runif(10, 0, 1), runif(10, -1, 0)))
p <- ggplot(df, aes(x=x, y=y, fill=group)) + 
  geom_bar(stat="identity", position="identity")
print(p) 
