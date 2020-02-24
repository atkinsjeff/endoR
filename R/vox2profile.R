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
vox2profile<-function(PAVD, Z, data) {

    data$n<-1

    PAVD_profile<-aggregate(PAVD~Z, FUN='mean', data)
    PAVD_profile$sd<-aggregate(PAVD~Z, FUN='sd', data)[,2]
    PAVD_profile$n<-aggregate(n~Z, FUN='sum', data)[,2]
    PAVD_profile$ci_min<-PAVD_profile$PAVD-(1.96*PAVD_profile$sd)/sqrt(PAVD_profile$n)
    PAVD_profile$ci_max<-PAVD_profile$PAVD+(1.96*PAVD_profile$sd)/sqrt(PAVD_profile$n)

    return(PAVD_profile)

}
