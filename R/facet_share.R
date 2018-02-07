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

FacetShare <- ggproto("FacetShare", Facet,
  shrink = TRUE,
  
  compute_layout = function(data, params) FacetWrap$compute_layout(data, params),
  map_data = function(data, layout, params) FacetWrap$map_data(data, layout, params),
  
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    lala <<- list(x_scale, y_scale, layout)
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
      if (params$dir == "h") {
        scales$x[[2]]$dir <- "h"
        scales$x[[2]]$oob <- function(x, ...) x
      }
    }
    if (!is.null(y_scale)) {
      scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
      if (params$dir == "v") {
        scales$y[[2]]$dir = "v"
        scales$y[[2]]$oob <- function(x, ...) x
      }
    }
    slala <<- scales
    scales
  },


  train_scales = function(x_scales, y_scales, layout, data, params) {
    lala2 <<- list(x_scales, y_scales, params)
    ppa <<- list(x_scales, y_scales, layout, data, params)
    dir <- if (length(x_scales) == 2) x_scales[[2]]$dir else y_scales[[2]]$dir
    data <- lapply(data, function(layer_data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      vars <- intersect(
        if (dir == "h") x_scales[[1]]$aesthetics else y_scales[[1]]$aesthetics, names(layer_data))
      trans <- layer_data$PANEL == 2L
      ldld <<- layer_data
      # if (is.numeric(layer_data[trans, vars])) {
      for (i in vars) {
        layer_data[trans, i] <- layer_data[trans, i] * -1
      }
        # layer_data[trans, vars] <- layer_data[trans, vars] * -1
      # }
      layer_data
    })

    # Facet$train_scales(x_scales, y_scales, layout, data, params)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      if (!is.null(x_scales)) {
        x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
        SCALE_X <- layout$SCALE_X[match_id]
        
        scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
      }
      
      if (!is.null(y_scales)) {
        y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
        SCALE_Y <- layout$SCALE_Y[match_id]
        
        scale_apply(layer_data, y_vars, "train", SCALE_Y, y_scales)
      }
    }
  },

  finish_data = function(data, layout, x_scales, y_scales, params) {

    dir <- if (length(x_scales) == 2) x_scales[[2]]$dir else y_scales[[2]]$dir
    to_intersect <- if (dir == "h") x_scales[[1]]$aesthetics else y_scales[[1]]$aesthetics
    vars <- intersect(to_intersect, names(data))
    trans <- data$PANEL == 2L
    ppa3 <<- list(data, layout, x_scales, y_scales, params, vars)
    
    
    for (i in vars) {
      layer_data[trans, i] <- layer_data[trans, i] * -1
    }
    # if (is.numeric(layer_data[trans, vars])) {
    #   data[trans, vars] <- data[trans, vars] * -1
    # }
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

    # add shared axis
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
  })
  
eval_facet_vars <- function(vars, data, env = emptyenv()) {
  nms <- names(vars)
  out <- list()
  
  for (i in seq_along(vars)) {
    out[[ nms[[i]] ]] <- eval_facet_var(vars[[i]], data, env = env)
  }
  oaut <<- out
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


P <- ggplot(test, aes(x = x, y = y)) + 
  geom_line(aes(color = fac), lwd = 1) + 
  facet_share(~fac, dir = "h", scales = "free") +
  theme(legend.position = "none")

P


mpg <- mpg
mpg <- mpg[, c("class", "year")]
mpg <- data.frame(rbind(mpg[mpg$year == 1999, ], mpg[mpg$year == 1999, ]))
mpg$year <- factor(rep(1:2, each = nrow(mpg)/2))


ggplot(mpg, aes(class)) + geom_bar(aes(y = ..count..)) + facet_share(~year, scales = "free")

ggplot(mpg, aes(class)) + geom_bar()


SUPERLIST <<- list(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

SUPERLIST[[1]][[2]]$children$











#https://github.com/thomasp85/ggforce/blob/master/R/facet_zoom.R
scale_apply <- function(data, vars, method, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()
  
  n <- length(scales)
  if (any(is.na(scale_id))) stop()
  
  scale_index <- split_indices(scale_id, n)
  
  a <- lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      scales[[i]][[method]](data[[var]][scale_index[[i]]])
    })
    # Join pieces back together, if necessary
    if (!is.null(pieces)) {
      unlist(pieces)[order(unlist(scale_index))]
    }
  })
  aaa <<- a
  a
}

