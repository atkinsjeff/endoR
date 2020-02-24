#' Calculate rugosity and other higher level complexity metrics
#'
#' \code{center_pts} calculates the effective number of layers in a canopy.
#'
#'
#' @param df a data frame of VAI for x, z bins from
#'
#' @keywords enl
#' @return the effective number of layers
#' @export
#' @examples
#' # Calculates the effective number of layers
#' calc_enl(pcl_vai)
#'
#'
center_pts <- function(df) {
x[,1] <- x[,1] - (max(x[,1]) - min(x[,1]))
x[,2] <- x[,2] - (max(x[,2]) - min(x[,2]))
x[,3] <- x[,3] - min(x[,3])

return()
}
