#' Generate random dates and times between two dates
#'
#' @param N the number of random dates/times to produce
#' @param st the start date in the format e.g. "2014/11/17"
#' @param et the end date in the format e.g. "2014/11/17"
#' @return A sequence of random dates and times
#' @examples
#' random_datetime(100, st="2015/01/01", et="2015/01/31")
#' @export


random_datetime <- function(N, st="2015/01/01", et="2015/12/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
  return(rt)
}
