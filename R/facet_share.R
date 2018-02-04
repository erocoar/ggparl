#
#
#
#
#
#

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
  
  map_data = function(data, layout, params) {
    if (plyr::empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    vars <- plyr::as.quoted(params$facets)
    
    facet_vals <- eval_facet_vars(vars, data, params$plot_env)
    facet_vals[] <- lapply(facet_vals[], as.factor)
    
    missing_facets <- setdiff(names(vars), names(facet_vals))
    if (length(missing_facets) > 0) {
      
      to_add <- unique(layout[missing_facets])
      
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      
      data <- plyr::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- plyr::unrowname(cbind(
        facet_vals[data_rep, ,  drop = FALSE],
        to_add[facet_rep, , drop = FALSE]))
    }
    
    keys <- plyr::join.keys(facet_vals, layout, by = names(vars))
    
    data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    data <- data[order(data$PANEL), ]
    dir_idx <- switch(params$dir, "h" = 1, "v" = 2)
    
    data[data$PANEL == 2, dir_idx] <- data[data$PANEL == 2, dir_idx] * -1
    data
  },
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)

    panels <- matrix(panels, nrow = nrow, ncol = ncol)

    panel_table <- gtable::gtable_matrix("layout", panels,
      widths = unit(rep(1, ncol), "null"), heights = unit(rep(1, nrow), "null"), clip = "on")

    panel_spacing <- switch(params$dir,
      "h" = if (is.null(theme$panel.spacing.x)) theme$panel.spacing else theme$panel.spacing.x,
      "v" = if (is.null(theme$panel.spacing.y)) theme$panel.spacing else theme$panel.spacing.y)
    
    panel_table$layout$name <- paste0("panel-", c(1, 2))
    
    axes <- render_axes(ranges, ranges, coord, theme, 
                        transpose = TRUE)

    if (params$dir == "h") {
      tick_idx <- grep("axis.ticks",
        sapply(axes$y$left[[1]]$children$axis$grobs, function(x) x$name))
      
      lab_idx <- (tick_idx == 1) + 1
      
      ax_tick_l <- axes$y$left[[1]]$children$axis$grobs[[tick_idx]]
      ax_tick_r <- ax_tick_l
      ax_tick_r$x[seq(1, length(ax_tick_r$x), 2)] <- unit(0, "npc")
      ax_tick_r$x[seq(2, length(ax_tick_r$x), 2)] <- convertWidth(grobWidth(ax_tick_l), "pt")
      
      shared_axis <- matrix(list(
        ax_tick_l,
        axes$y$left[[1]]$children$axis$grobs[[lab_idx]],
        ax_tick_r
      ), ncol = 3, nrow = 1)

      shared_axis <- gtable::gtable_matrix("shared.ax.y", shared_axis,
        widths = unit(c(axes$y$left[[1]]$children$axis$widths[[tick_idx]],
                        1,
                        axes$y$left[[1]]$children$axis$widths[[tick_idx]]),
                      c("pt", "grobwidth", "pt"),
                      list(NULL, axes$y$left[[1]]$children$axis$grobs[[lab_idx]], NULL)),
        heights = unit(1, "npc"), clip = "off")
      
      shared_axis <- gtable::gtable_add_col_space(shared_axis, panel_spacing)

    } else {
      tick_idx <- grep("axis.ticks",
        sapply(axes$x$bottom[[1]]$children$axis$grobs, function(x) x$name))
      
      lab_idx <- (tick_idx == 1) + 1
      
      ax_tick_b <- axes$x$bottom[[1]]$children$axis$grobs[[tick_idx]]
      ax_tick_t <- ax_tick_b
      ax_tick_t$y[seq(1, length(ax_tick_t$y), 2)] <- unit(0, "npc")
      ax_tick_t$y[seq(2, length(ax_tick_t$y), 2)] <- convertWidth(grobWidth(ax_tick_b), "pt")
      
      shared_axis <- matrix(list(
        ax_tick_b,
        axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]],
        ax_tick_b
      ), ncol = 1, nrow = 3)
      
      shared_axis <- gtable::gtable_matrix("shared.ax.x", shared_axis,
        widths = unit(1, "npc"),
        heights = unit(c(axes$x$bottom[[1]]$children$axis$heights[[tick_idx]],
                         1,
                         axes$x$bottom[[1]]$children$axis$heights[[tick_idx]]),
                       c("pt", "grobwidth", "pt"),
                       list(NULL, axes$x$bottom[[1]]$children$axis$grobs[[lab_idx]], NULL)),
        clip = "off")
      
      shared_axis <- gtable::gtable_add_row_space(shared_axis, panel_spacing)
    }

    # add y.axis
    if (params$dir == "h") {
      panel_table <- gtable::gtable_add_cols(panel_table, 
        convertWidth(grobWidth(shared_axis), "cm") + panel_spacing, 1)
      panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = 2, t = 1, clip = "off")
    } else {
      panel_table <- gtable::gtable_add_rows(panel_table,
        convertHeight(grobHeight(shared_axis), "cm") + panel_spacing, 1)
      panel_table <- gtable::gtable_add_grob(panel_table, shared_axis, l = 1, t = 2, clip = "off")
    }
    panel_table
  }
                
  )
  
eval_facet_vars <- function(vars, data, env = emptyenv()) {
  nms <- names(vars)
  out <- list()
  
  for (i in seq_along(vars)) {
    out[[ nms[[i]] ]] <- eval_facet_var(vars[[i]], data, env = env)
  }
  tibble::as_tibble(out)
}

eval_facet_var <- function(var, data, env = emptyenv()) {
  if (is.name(var)) {
    var <- as.character(var)
    if (var %in% names(data)) {
      data[[var]]
    } else {
      NULL
    }
  } else if (is.call(var)) {
    eval(var, envir = data, enclos = env)
  } else {
    stop("Must use either variable name or expression when faceting",
         call. = FALSE)
  }
}


test <- data.frame(x = rep(seq_len(15), 2), y = rep(rnorm(15), 2),
                   fac = factor(rep(1:2, each = 15)))


ggplot(test, aes(x = x, y = y)) + 
  geom_line(aes(color = fac), lwd = 1) + 
  facet_share(~fac, dir = "h", scales = "free") +
  theme(legend.position = "none")

