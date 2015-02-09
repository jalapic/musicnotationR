#' Generate default ggplot2 color palette
#'
#' @param n The number of colors to generate
#' @return A vector of colors
#' @examples
#' gg_colors(4) #First four ggplot2 colors
#' @export

gg_colors <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

