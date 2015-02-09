#' Generate music notation graph of social interactions
#'
#' @param df A dataframe with at least 3 columns in the order: time, id1, id2.
#' Every observation/row should be a mututally exclusive behavioral event.
#' The first variable, time, can be in numeric or date/time format. The 2nd and
#' 3rd variables, id1 and id2 should be character variables denoting the id of
#' the individual (id1) directing each behavior to the other individual (id2) that
#' receives the behavior.  If want to discriminate lines by different behaviors,
#' a 4th column should be the behavior of interest.  Other columns are permissible
#' in the dataframe that come after these required variables.
#' @param ranks The default is NULL meaning that individuals will be plotted
#' in alphanumerical order.  Otherwise, a character vector indicating the rank
#' of individuals (from highest to lowest) which will determine the plotted order
#' of individuals.
#' @param arrowsize The size of the arrowhead.
#' df, ranks=NULL, arrowsize=0.3, arrowweight=0.75, pointsize=3, ylabel=NULL,
#' @param arrowweight The weight of the arrowhead.
#' @param pointsize The size of points.
#' @param ylabel The y-axis title.
#' @param colors The colors to be used for plotting.  The defualt is
#' \strong{\code{type}="default"} meaning the default ggplot2 color palette is
#' used.  Other options are:  \strong{\code{type}="terrain"}, \strong{\code{type}="heat"}
#' \strong{\code{type}="cm"}, \strong{\code{type}="random"}, \strong{\code{type}="grays"}
#' \strong{\code{type}="rainbow"}, \strong{\code{type}="topo"}, or alternatively a
#' character vector of color names or codes of can be used.
#' @param gridcolors default is to use grey horizontal grid lines,
#' but if \strong{\code{type}="gridcolors=T"} then horizontal grid lines of the same
#' color as each individual will be used.
#' @param gridlinesize Weight/size of gridlines.
#' @param labels Default is \strong{\code{type}="rank"} which will use numeric values as
#' y-axis labels, but if \strong{\code{type}="name"} then the individual's id will be used.
#' @param behav Default is \strong{\code{type}="default"} which is to not differentiate
#' different behaviors in the plot, but if \strong{\code{type}="yes"} then lines will be
#' differentially styled (dotted, dashed, etc.) to distinguish different behaviors.  A
#' character vector with the names of each behavior can be used to determine the order in
#' which different line styles are to be applied.  To change from \strong{\code{type}="default"}
#' requires a 4th variable in the dataframe identifying the behavior at each observation.
#' @return A music notation graph of social interactions.
#' @examples
#' musicnot(flies)
#' musicnot(flies, gridcolors=T)
#' musicnot(flies, gridcolors=T, gridlinesize=0.5)
#' musicnot(flies, gridcolors=T, gridlinesize=0.5, colors="topo")
#' musicnot(flies, gridcolors=T, gridlinesize=0.5, colors="rainbow")
#' musicnot(flies, gridcolors=T, gridlinesize=0.5, labels="name")
#' musicnot(flies, gridcolors=T, gridlinesize=0.5, behav="yes")
#' @section References:
#' Ivan D. Chase, (2006),
#' Music notation: a new method for visualizing social interaction in animals and humans
#' Frontiers in Zoology 3: 18
#' @section Further details:
#' tba
#' @export




musicnot <- function(df, ranks=NULL, arrowsize=0.3, arrowweight=0.75, pointsize=3, ylabel=NULL,
                     colors = "default", gridcolors = NULL, gridlinesize=1,
                     labels = "rank", behav="default") {

  library(ggplot2)
  library(grid)

  df[,2] <- as.character(df[,2])
  df[,3] <- as.character(df[,3])

  colnames(df)[1] <- "time"


  myids <- sort(unique(c(df[,2], df[,3]))) #required for number of steps

  if (is.null(ranks)==T){

    df$mywinner <- match(df[,2], myids)
    df$myloser <- match(df[,3], myids)

  }

  else

    if (is.null(ranks)==F){
      df$mywinner <- match(df[,2], ranks)
      df$myloser <- match(df[,3], ranks)
    }


  ### defining colors

  if (colors == "default") {mycolors <- gg_colors(length(myids)) }  #if colors are not defined default
  else
    if (colors == "terrain") {mycolors <- terrain.colors(length(myids)) }
  else
    if (colors == "heat") {mycolors <- heat.colors(length(myids)) }
  else
    if (colors == "cm") {mycolors <- rev(cm.colors(length(myids))) }
  else
    if (colors == "topo") {mycolors <- topo.colors(length(myids)) }
  else
    if (colors == "rainbow") {mycolors <- rainbow(length(myids)) }
  else
    if (colors == "random") {mycolors <- sample(colours(), length(myids))}
  else
    if (colors == "grays") {
      mypal <- palette(gray(0:(length(myids)+1) / (length(myids)+1)))
      mycolors <- mypal[1:length(myids)]
    }
  else
    mycolors <- colors




  ### defining grid colors

  if (is.null(gridcolors)==T) {
    mygridcolors <- "grey90"
  }

  else
    mygridcolors <- mycolors


  ### defining y-axis tick labels

  if (labels=="rank") {  mylabels <- 1:length(myids) }

  else

  if (labels=="name") {

    if(is.null(ranks)==T){mylabels <- myids }

    else
      mylabels <- ranks
  }


  ### define linetypes

  if (behav=="default") {  mylinetype <- 1 }

  else

    if (behav=="yes") {  mylinetype <- match((df[,4]), unique(df[,4]))  }

  else

    mylinetype <-   match((df[,4]), behav)





  ### plotting

  myplot <-

    ggplot() +
    geom_segment(aes
                 (x = time,
                  xend = time,
                  y = mywinner,
                  yend = myloser,
                  color=factor(mywinner)
                 ),
                 linetype=mylinetype,

                 arrow=arrow(length = unit(arrowsize, "cm")),
                 lwd=arrowweight,

                 data=df)  +

    geom_point(aes
               (x = time,
                y = mywinner,
                color = factor(mywinner)
               ),
               size=pointsize,
               data=df)  +


    coord_cartesian(ylim=c(0.5,length(myids)+0.5)) +

    scale_y_reverse(breaks=1:length(myids), labels=mylabels)  +

    theme_bw() +

    theme(legend.position = "none") +

    ylab(ylabel) +

    scale_color_manual(values = mycolors, limits=c(1:length(myids))) +

    theme(panel.grid.major.y = element_line(color = mygridcolors, size=gridlinesize))



  return(myplot)

}
