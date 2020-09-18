#' Tidy model and append labels, names, sort and remove intercept
#'
#' @param model a regression model
#' @param name name of the model.
#' @param intercept whether intercept should be included (default is FALSE).
#' @param sort whether estimates should be sorted in descending order.
#' @param labels vector of estimate label names. Must be the same length as
#' the number of estimates (including intercept).
#'
#' @return
#' @export
#'
#' @examples
tidy_model <- function(model, name=NULL, intercept=FALSE,
                       sort=FALSE, labels=NULL, conf.level=0.95) {
  df <- broom::tidy(model, conf.int=TRUE, conf.level=conf.level) %>%
    dplyr::mutate(labelestimate = paste(round(estimate, 2), sep=""))

  # Significant at alpha.
  df$significant<- ifelse(df$conf.low < 0 & df$conf.high > 0,
                          "Not significant", "Significant")

  # Rename labels.
  df$label <- as.character(df$term)
  if(!is.null(labels)) {
    for (term in unique(df$term)) {
      df$label[df$term == term] <- labels[term]
    }
    df$label[is.na(df$label)] <- df$term[is.na(df$label)]
  }
  df$label <- as.factor(df$label)

  if(sort) {
    # Sort by estimate, descdending order.
    df$label <- reorder(df$label, df$estimate)
  } else {
    # Keep the order of the labels as in the data.
    df$label <- as.character(df$label)
    df$label <- factor(df$label, levels=rev(unique(df$label)))
  }

  if(!intercept){
    # Remove intercept.
    df <- df %>% dplyr::filter(term != "(Intercept)")
  }

  # Set model name.
  df$modelname <- name

  return(df)
}



#' Title
#'
#' @param old_labels character vector with old labels.
#' @param new_labels character vector with new labels.
#'
#' @return character vector with the old labels replaced with the new labels.
#' @export
#'
#' @examples
relabel <- function(old_labels, new_labels) {
  if (is.null(new_labels)) {
    return(old_labels)
  }

  for (term in unique(old_label)) {
    old_label[old_label == term] <- new_label[term]
  }
  old_labels[is.na(old_labels)] <- new_labels[is.na(ol_labels)]

  df$label <- as.factor(df$label)

}


#' Coefficient plot for a regression model
#'
#' @param model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
coefplot <- function(model, ...) {
  coefplot_compare(model, model, ...)
}


#' Coefficient plot for comparison of two regression models
#'
#' Works and is tested with ordinary lm and mixed-effect lme4 models.
#'
#' @param model1 model 1.
#' @param model2 model 2.
#' @param modelnames vector (of length 2) with model names.
#' @param intercept whether the intercept should be included in the plots
#' (default is FALSE).
#' @param sort whether the estimates should be sorted in descending order
#' (default is FALSE).
#' @param title title of the plot.
#' @param subtitle sub title of the plot.
#' @param caption caption of the plot.
#' @param labels1 vector with label names for model 1. Should be the same length
#' as the number of estimates in the model, including the intercept.
#' @param labels2 vector with label names for model 2. Should be the same length
#' as the number of estimates in the model, including the intercept.
#' @param reverse_labels whether or not to reverse the label ordering.
#' @param nudge_x nudge the point label estimates on the X axis so that they do
#' not overlap the point.
#' @param nudge_y nudge the point label estimates on the Y axis so that they do
#' not overlap the point.
#' @param highlight.significant whether or not significant findings should be
#' highlighted.
#' @param dodge numeric value that separate the two models from each other.
#' @param conf.level confidence level of the error bars (defaults to 0.95).
#'
#' @return a ggplot2 plot with one coefficient plot with the two models.
#' @export
#'
#' @examples
coefplot_compare <- function(model1, model2, modelnames=NULL,
                             intercept=FALSE, sort=FALSE, title="Predictors",
                             subtitle=NULL, caption=NULL,
                             labels1=NULL, labels2=NULL,
                             reverse_labels=FALSE,
                             nudge_x=0, nudge_y=0.3,
                             dodge=0.5,
                             conf.level=0.95,
                             highlight.significant=FALSE) {

  if(is.null(modelnames)) {
    modelnames[1] <- deparse(substitute(model1))
    modelnames[2] <- deparse(substitute(model2))
  }

  # If the models have different significant levels.
  if (is.numeric(conf.level) & length(conf.level) == 2) {
    conf.level1 <- conf.level[1]
    conf.level2 <- conf.level[2]
    if (is.null(caption)) {
      caption <- paste0("Bars represent ", round(conf.level1 * 100, 2),
                        " and ", round(conf.level2 * 100, 2),
                        "% C.I.")
    }
  } else {
    conf.level1 <- conf.level
    conf.level2 <- conf.level
    if (is.null(caption)) {
      caption <- paste0("Bars represent ", round(conf.level * 100, 2), "% C.I.")
    }
  }

  df <- rbind(tidy_model(model1, name=modelnames[1], intercept, sort, labels1, conf.level=conf.level1),
              tidy_model(model2, name=modelnames[2], intercept, sort, labels2, conf.level=conf.level2))

  df <- df %>% na.exclude()

  if (reverse_labels) {
    df$label <- reorder(df$label, desc(df$label))
  }


  if(highlight.significant) {
    gg <- ggplot2::ggplot(df, aes(label, estimate, color=modelname, alpha=significant))
  } else {
    gg <- ggplot2::ggplot(df, aes(label, estimate, color=modelname))
  }
  gg +
    ggplot2::scale_alpha_discrete(range = c(0.2, 1)) +
    ggplot2::geom_hline(yintercept = 0, color="black", alpha=0.5, linetype=2) +
    ggplot2::geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.6,
                    position = position_dodge(width = dodge)) +
    ggplot2::scale_color_manual(values=c("black", "gray70")) +
    #geom_text(aes(label=labelestimate), alpha=0.7, nudge_y=nudge_y, nudge_x=nudge_x) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_text(color="black"),
                   legend.position = "bottom",
                   legend.direction='vertical') +
    ggplot2::labs(title = title,
                   subtitle = subtitle,
                   linetype = NULL,
                   color = NULL,
                   alpha = NULL,
                   caption = caption,
                   y = "Estimate",
                   x = NULL) +
    ggplot2::coord_flip()
}

#' Coefficient plot for mixed-effects model
#'
#' @param model
#' @param intercept
#' @param sort
#' @param title
#' @param subtitle
#' @param caption
#' @param labels
#' @param nudge_x
#' @param nudge_y
#'
#' @return
#' @export
#'
#' @examples
coefplot_mlm <- function(model, intercept=FALSE, sort=FALSE,
                         title="Predictors", subtitle="",
                         caption="Bars represent 95 % C.I.", labels=NULL,
                         conf.level=0.95,
                         nudge_x=0, nudge_y=0.3) {
  tidy_model(model, name="Model 1", intercept, sort, labels, conf.level=conf.level) %>%
    na.exclude() %>%
    ggplot2::ggplot(aes(label, estimate)) +
    #ggplot2::geom_point(size=3, fill="black") +
    ggplot2::geom_pointrange(aes(ymin=conf.low, ymax=conf.high), size=0.8,
                    lty=1, col="black") +
    ggplot2::geom_hline(yintercept = 0, color="black", alpha=0.5, linetype=2) +
    ggplot2::geom_text(aes(label=labelestimate), color="gray50", size=3,
              nudge_y=nudge_y, nudge_x=nudge_x) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title=title,
         subtitle = subtitle,
         caption=caption,
         x=NULL,
         y="Estimate",
         alpha = NULL) +
    ggplot2::coord_flip()
}


#' Grouped coefficient plot for mixed-effects model
#'
#' @param model
#' @param intercept
#' @param sort
#' @param title
#' @param caption
#' @param labels
#' @param grouplabels
#' @param nudge_y
#' @param nudge_x
#'
#' @return
#' @export
#'
#' @examples
grouped_coefplot_mlm <- function(model, intercept=FALSE, sort=FALSE,
                                 title="Predictors",
                                 conf.level=0.95,
                                 caption="Bars represent 95 % C.I.",
                                 labels=NULL, grouplabels=NULL,
                                 nudge_y=0.3, nudge_x=0) {
  if(is.null(grouplabels)) { stop("grouplabels cannot be null.") }

  df <- broom::tidy(model, conf.int=TRUE, conf.level=conf.level) %>%
    mutate(labelestimate = paste(round(estimate, 2), sep=""))
  if(!is.null(labels)) {
    # Rename labels. Add empty string for the sd_ terms (one for each level).
    emptystring <- rep("", abs(NROW(df) - NROW(labels)))
    df$label <- c(labels, emptystring)
  } else {
    df$label <- df$term
  }
  if(!is.null(grouplabels)) {
    # Add groups. Add empty string for the sd_ terms (one for each level).
    emptystring <- rep("", abs(NROW(df) - NROW(grouplabels)))
    df$grouplabel <- c(grouplabels, emptystring)
  }
  if(sort == TRUE) {
    # Sort by estimate, descdending order.
    df$label <- reorder(df$label, df$estimate)
  }
  if(intercept == FALSE){
    df <- df %>% dplyr::filter(term != "(Intercept)")
  }

  df %>%
    #filter(!startsWith(term, "sd_")) %>% # Exclude variance estimates.
    na.exclude() %>%
    ggplot2::ggplot(aes(estimate, label)) +
    ggplot2::geom_point(size=3, fill="black") +
    ggplot2::geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0,
                   lwd=1, lty=1, col="black") +
    ggplot2::geom_vline(xintercept = 0, color="black", alpha=0.5, linetype=2) +
    ggplot2::geom_text(aes(label=labelestimate), alpha=0.5, nudge_y=nudge_y,
              nudge_x=nudge_x) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title=title,
         caption=caption,
         x="Estimate",
         y=NULL,
         alpha=NULL) +
    ggplot2::facet_wrap(~ grouplabel, dir="v", strip.position="top", drop=TRUE,
               scales="free_y", ncol=1)
}



#' Title
#'
#' Solution by Sandy Muspratt:
#' https://stackoverflow.com/questions/40229263/how-to-rotate-legend-symbols-in-ggplot2?noredirect=1&lq=1
#'
#' @param gg
#'
#' @return
#' @export
#'
#' @examples
rotate_legend <- function(gg) {
  g = ggplot2::ggplotGrob(gg)

  # Get names of segment grobs
  grid::grid.ls(grid::grid.force(g))$name   # "GRID.segments"

  # Check the structure of the segment grobs
  #str(grid::getGrob(grid::grid.force(g), grid::gPath("GRID.segments"),
  #                  grep = TRUE, global = TRUE))

  # Edit the segment grobs using the editGrob() function
  # 1) Rotate the segments
  g <- grid::editGrob(grid::grid.force(g), grid::gPath("GRID.segments"),
                      grep = TRUE, global = TRUE,
                      vp = grid::viewport(angle = 90))

  # 2) set end points of segments
  g <- grid::editGrob(grid::grid.force(g), grid::gPath("GRID.segments"),
                      grep = TRUE, global = TRUE,
                      x0 = ggplot2::unit(0.1, "npc"),
                      y0 = ggplot2::unit(0.5, "npc"),
                      x1 = ggplot2::unit(0.9, "npc"),
                      y1 = ggplot2::unit(0.5, "npc"))

  # Draw it
  grid::grid.newpage()
  grid::grid.draw(g)
}

