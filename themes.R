# Publication themes taken from https://rpubs.com/Koundy/71792

#' Publication Theme
#'
#' This is a publication ready theme created by Koundinya Desiraju in the
#' following blog post: \url{https://rpubs.com/Koundy/71792}.
#'
#' @param base_size The size of the fonts used throughout the theme.
#' @param base_family The font family used throughout the theme.
#' @param legend.position The position of the legends ("none", "left", "right",
#'   "bottom", "top").
#'
#' @export
theme_pub <- function(base_size=14, base_family="helvetica",
                      legend.position = 'bottom') {
  # helvetica is Helvetica on Mac OS X :)
  if (loyalr::is_mac() && base_family == 'helvetica') {
    base_family = 'Helvetica'
  }

  if (legend.position != 'bottom' && legend.position != 'top') {
    legend.direction = 'vertical'
  } else {
    legend.direction = 'horizontal'
  }

  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
    + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
      size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(face = "bold",size = rel(1)),
      axis.title.y = ggplot2::element_text(angle=90,vjust =2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour="black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.position = legend.position,
      legend.direction = legend.direction,
      legend.key.size= ggplot2::unit(0.2, "cm"),
      legend.margin = ggplot2::unit(0.2, "cm"),
      legend.title = ggplot2::element_text(face = "italic"),
      plot.margin = ggplot2::unit(c(10,5,5,5), "mm"),
      strip.background =
        ggplot2::element_rect(colour="#f0f0f0", fill="#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    ))
}


#' Defines the color palette for the 'fill' aesthetic used by the publication
#' theme.
#'
#' @param ... Arguments passed to ggplot2::discrete_scale
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = factor(carb), fill = factor(carb))) +
#'   geom_bar() +
#'   scale_fill_pub() +
#'   theme_pub()
#'
#' @rdname theme_pub
#'
#' @export
scale_fill_pub <- function(...){
  ggplot2::discrete_scale("fill", "Publication",
    scales::manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c",
                                  "#662506", "#a6cee3", "#fb9a99", "#984ea3",
                                  "#ffff33")),
    ...)
}


#' Defines the color palette for the 'color' aesthetic used by the publication
#' theme.
#'
#' @param ... Arguments passed to ggplot2::discrete_scale
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg, y = disp, color = factor(carb))) +
#'   geom_point() +
#'   scale_color_pub() +
#'   theme_pub()
#'
#' @rdname theme_pub
#'
#' @export
scale_color_pub <- function(...){
  ggplot2::discrete_scale("color", "Publication",
    scales::manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c",
                                  "#662506", "#a6cee3", "#fb9a99", "#984ea3",
                                  "#ffff33")),
    ...)
}
