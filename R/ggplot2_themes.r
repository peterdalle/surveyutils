#' Larger ggplot2 theme for Powerpoint presentations
#'
#' Uses larger text size to make it easier to read.
#'
#' Black text on white background.
#'
#' @param font_size base font size.
#' @param ... additional parameters passed to ggplot2 theme().
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples
theme_powerpoint <- function(font_size = 20, ...) {
  custom_theme <- ggplot2::theme(
    panel.border = ggplot2::element_rect(color = "black", fill = "transparent"),
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(size = font_size),
    ...
    )
  return(ggplot2::theme_minimal() + custom_theme)
}


#' Black ggplot2 theme for Powerpoint presentations
#'
#' White text on black background.
#'
#' Based on https://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2/.
#'
#' @param base_size base font size.
#' @param base_family base font family.
#' @param ... additional parameters passed to ggplot2 theme().
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples
theme_powerpoint_black = function(base_size = 12, base_family = NULL, ...) {
  t <- ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
           ggplot2::theme(
             axis.line = ggplot2::element_blank(),
             axis.text.x = ggplot2::element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
             axis.text.y = ggplot2::element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
             axis.ticks = ggplot2::element_line(color = "white", size  =  0.2),
             axis.title.x = ggplot2::element_text(size = base_size, color = "white", margin = ggplot2::margin(0, 10, 0, 0)),
             axis.title.y = ggplot2::element_text(size = base_size, color = "white", angle = 90, margin = ggplot2::margin(0, 10, 0, 0)),
             axis.ticks.length = ggplot2::unit(0.3, "lines"),
             legend.background = ggplot2::element_rect(color = NA, fill = "black"),
             legend.key = ggplot2::element_rect(color = "white",  fill = "black"),
             legend.key.size = ggplot2::unit(1.2, "lines"),
             legend.key.height = NULL,
             legend.key.width = NULL,
             legend.text = ggplot2::element_text(size = base_size*0.8, color = "white"),
             legend.title = ggplot2::element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
             legend.position = "right",
             legend.text.align = NULL,
             legend.title.align = NULL,
             legend.direction = "vertical",
             legend.box = NULL,
             panel.background = ggplot2::element_rect(fill = "black", color  =  NA),
             panel.border = ggplot2::element_rect(fill = NA, color = "white"),
             panel.grid.major = ggplot2::element_line(color = "grey35"),
             panel.grid.minor = ggplot2::element_line(color = "grey20"),
             panel.spacing = ggplot2::unit(0.5, "lines"),
             strip.background = ggplot2::element_rect(fill = "grey30", color = "grey10"),
             strip.text.x = ggplot2::element_text(size = base_size*0.8, color = "white"),
             strip.text.y = ggplot2::element_text(size = base_size*0.8, color = "white",angle = -90),
             plot.background = ggplot2::element_rect(color = "black", fill = "black"),
             plot.title = ggplot2::element_text(size = base_size*1.2, color = "white"),
             plot.subtitle = ggplot2::element_text(size = base_size*0.95, color = "white"),
             plot.margin = ggplot2::unit(rep(1, 4), "lines"),
             ...)
  return(t)
}

#' ggplot2 theme for journal publications
#'
#' @param ... additional parameters passed to ggplot2 theme().
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples
theme_publish <- function(base_family=NULL, ...) {
  ggplot2::theme_bw(base_family=base_family) +
    ggplot2::theme(panel.spacing = ggplot2::unit(2, "mm"),
                   panel.border = ggplot2::element_rect(color="#666666"),
                   #panel.grid.major.y = element_line(color="#dddddd", linetype="solid", size=0.15),
                   #panel.grid.major.x=element_blank(),
                   panel.grid.minor.x=ggplot2::element_blank(),
                   panel.grid.minor.y=ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(family=base_family),
                   plot.subtitle = ggplot2::element_text(family=base_family),
                   plot.caption = ggplot2::element_text(size=8, hjust=0, margin=ggplot2::margin(t=15)))
}


#' Stata style ggplot2 theme
#'
#' @param font_size
#' @param lines
#' @param legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_stata <- function(font_size=12, lines=TRUE, legend=TRUE, ...) {
  t <- theme_template(font_size=font_size, lines=lines, legend=legend,
                     bg_color = "#EAF2F3",
                     panel_bg_color = "#EAF2F3",
                     title_color = "black",
                     subtitle_color = "black",
                     axis_color = "black",
                     secondary_text_color = "black",
                     ...)
  return(t)
}

#' Economist style ggplot2 theme
#'
#' @param font_size
#' @param lines
#' @param legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_economist <- function(font_size=12, lines=TRUE, legend=TRUE, ...) {
  t <- theme_template(font_size=font_size, lines=lines, legend=legend,
                     bg_color = "#d5e4eb",
                     panel_bg_color = "#d5e4eb",
                     title_color = "black",
                     subtitle_color = "black",
                     axis_color = "black",
                     secondary_text_color = "black",
                     ...)
  return(t)
}

#' Pinky gray ggplot2 theme
#'
#' @param font_size
#' @param lines
#' @param legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_pinkygray <- function(font_size=12, lines=TRUE, legend=TRUE, ...) {
  t <- theme_template(font_size=font_size, lines=lines, legend=legend,
                     bg_color = "#f0f0f0",
                     panel_bg_color = "#EBEBEB",
                     title_color = "#D64752",
                     subtitle_color = "#576167",
                     axis_color = "#757C89",
                     secondary_text_color = "#576167",
                     ...)
  return(t)
}

#' Greenish ggplot2 theme
#'
#' @param font_size
#' @param lines
#' @param legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_green <- function(font_size=12, lines=TRUE, legend=TRUE, ...) {
  t <- theme_template(font_size=font_size, lines=lines, legend=legend,
                     bg_color = "#D8E0C9",
                     panel_bg_color = "#D8E0C9",
                     title_color = "black",
                     subtitle_color = "black",
                     axis_color = "black",
                     secondary_text_color = "black",
                     ...)
}

#' Bold and blue ggplot2 theme
#'
#' @param font_size
#' @param lines
#' @param legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_boldandblue <- function(font_size=18, lines=TRUE, legend=TRUE, ...) {
  t <- theme_template(font_size=font_size, font_family="Arial Rounded MT Bold", lines=lines, legend=legend,
                      bg_color = "#36AFE5",
                      panel_bg_color = "#36AFE5",
                      title_color = "white",
                      subtitle_color = "white",
                      axis_color = "white",
                      secondary_text_color = "white",
                      ...)
}

#' Template for theme ggplot2 themes.
#'
#' Used internally. Not intended to be exported.
#'
#' @param font_size base font size.
#' @param lines whether to show lines or not.
#' @param legend whether to show legend or not.
#' @param theme
#'
#' @return
# @export
#'
#' @examples
theme_template <- function(font_size, font_family="Arial", lines, legend, bg_color, panel_bg_color,
                          title_color, subtitle_color, axis_color,
                          secondary_text_color, ...) {
  #  theme(title = element_text(family = "Verdana"),
  #       panel.background = element_rect(fill = "transparent", colour = "black"))
  t <- theme_classic() +
    theme(plot.title = element_text(face="bold", size = font_size*1.3, family=font_family, color=title_color),
          plot.subtitle = element_text(family=font_family, color=secondary_text_color),
          plot.caption = element_text(color="#A0AAAF", size=font_size*0.9),
          plot.background = element_rect(fill=bg_color),
          panel.background = element_rect(fill=panel_bg_color),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "white"),
          panel.grid.major.x = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(color=axis_color, size =.7),
          axis.text = element_text(color=secondary_text_color),
          axis.title= element_text(color=secondary_text_color),
          legend.position = "top",
          legend.background = element_blank(),
          legend.key.width = unit(1, units = "mm"),
          legend.key.size = unit(1, units = "mm"),
          legend.box = "horizontal",
          legend.title = element_text(color=secondary_text_color),
          #legend.box.background = element_rect(color = "#A0AAAF", fill="transparent", size = 1),
          legend.text = element_text(face="bold", color=secondary_text_color),
          panel.border = element_blank(),
          ...)
  # Remove lines?
  if (!lines) {
    t <- t + theme(axis.line = element_blank())
  }
  # Remove legend?
  if (!legend) {
    t <- t + theme(legend.position = "none")
  }
  return (t)
}


# ------------------------------------------------------------------------------
# Do something
# ------------------------------------------------------------------------------

#' Plot examples of all themes
#'
#' @return
#' @export
#'
#' @examples
plot_theme_chart <- function() {
  gg <- ggplot2::ggplot(mtcars, aes(cyl, mpg, color=gear)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Title: written above the plot",
                  subtitle = "Subtitle: add som interpretation guide",
                  caption = "Caption: where the data comes from ",
                  x = "X axis text",
                  y = "Y axis text",
                  color = "Color")
  l <- list()
  l[[1]] <- gg + theme_powerpoint() + labs(title = "theme_powerpoint()")
  l[[2]] <- gg + theme_powerpoint_black() + labs(title = "theme_powerpoint_black()")
  l[[3]] <- gg + theme_economist() + labs(title = "theme_economist()")
  l[[4]] <- gg + theme_pinkygray() + labs(title = "theme_pinkygray()")
  l[[5]] <- gg + theme_boldandblue() + labs(title = "theme_boldandblue()")
  patchwork::wrap_plots(l)
}



#' Plot examples of all theme modifiers
#'
#' @return
#' @export
#'
#' @examples
plot_theme_modifier_chart <- function() {
  gg <- ggplot2::ggplot(mtcars, aes(cyl, mpg, color=gear)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Title: written above the plot",
                  subtitle = "Subtitle: add som interpretation guide",
                  caption = "Caption: where the data comes from ",
                  x = "X axis text",
                  y = "Y axis text",
                  color = "Color")
  l <- list()
  l[[1]] <- gg + theme_pinkygray() + theme_smaller() + labs(title = "theme_pinkygray() + theme_smaller()")
  l[[2]] <- gg + theme_pinkygray() + theme_larger() + labs(title = "theme_pinkygray() + theme_larger()")
  patchwork::wrap_plots(l)
}


register_all_fonts_on_windows <- function() {
  extrafont::font_import(prompt = FALSE)
  extrafont::loadfonts(device = "win")
}


export_plot_to_powerpoint____temp <- function(plot, filename) {
  # http://www.sthda.com/english/wiki/saving-high-resolution-ggplots-how-to-preserve-semi-transparency
  pptx <- ReporteRs::pptx()
  pptx <- ReporteRs::addSlide(pptx, slide.layout = "Two Content")
  pptx <- ReporteRsaddTitle(pptx, "Survival Curves: Editable Vector Graphics" )
  pptx <- ReporteRs::addPlot(pptx, function() print(plot, newpage = FALSE), vector.graphic = TRUE)
  writeDoc(pptx, file = filename)
}
