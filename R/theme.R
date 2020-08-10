#' mainTheme
#' 
#' Add \code{+ mainTheme()} to ggplots
#' to apply the theme used in the shiny app.
#'
#' @return list. A list to be added to a ggplot object.
#' @export
mainTheme <- function() {
  list(
    theme_minimal(),
    theme(
      axis.line        = element_line(colour = "grey70", size = .75),
      text             = element_text(
        color          = "black",
        face           = "bold",
        family         = "sans"
      ),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.background  = element_blank(),
      legend.position  = "bottom",
      panel.background = element_rect(color = "grey70", fill = NA, size = 1),
      strip.background = element_rect(fill = "grey80", color = "black"),
      strip.text       = element_text(color = "black")
    )
  )
}
