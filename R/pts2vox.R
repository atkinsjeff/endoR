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
pts2vox<-function(x, sub.vox, main.vox){
    require(VoxR)
    cs_vox<-vox(x, sub.vox)

    cs_vox_1m <- vox(cs_vox, main.vox)
    colnames(cs_vox_1m)[1:3]<-c("X","Y","Z")

    cs_vox_1m$pgap<-1-(cs_vox_1m$nbpts/(1/(sub.vox^3)))
    cs_vox_1m$PAVD<- -1.1*log(cs_vox_1m$pgap)

    return(cs_vox_1m)
}
