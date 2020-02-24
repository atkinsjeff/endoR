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
topo_correct<-function(scan, resolution = 2, plane = FALSE){
    las<-LAS(scan[,1:3])
    crs(las)<-"+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"

    # r <- raster(xmn=-200, xmx=200, ymn=-200, ymx=200, resolution = resolution)
    r <- raster(xmn=floor(min(las@data$X-resolution)), xmx=ceiling(max(las@data$X+resolution)), ymn=floor(min(las@data$Y-resolution)), ymx=ceiling(max(las@data$Y+resolution)), resolution = resolution)
    topo<-grid_metrics(las, quantile(Z, 0.01), r)
    plot(topo, col = viridis(250))

    crs(topo)<-"+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"
    slope<-terrain(topo, opt = "slope", unit = "degrees", neighbors = 8)
    plot(slope)

    topo[slope>40]<-NA
    setMinMax(topo)

    topo.df<-as.data.frame(rasterToPoints(topo))
    colnames(topo.df)<-c("X","Y","Z")

    ws <- seq(3,12, 3)
    th <- seq(0.1, 1.5, length.out = length(ws))

    topo.las<-LAS(topo.df)
    crs(topo.las)<-"+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"
    # topo.las@data$Classification<-2
    ground<-lasground(topo.las, pmf(ws, th), last_returns = FALSE)

    # plot(ground, color = "Classification")

    topo.las.r<-grid_terrain(ground, res = resolution, knnidw(k = 21))
    plot(topo.las.r)

    las<- las - topo.las.r

    return(las)

    if(plane == TRUE) {
        topo_pts<-as.data.frame(rasterToPoints(topo))
        colnames(topo_pts)[3]<-"z"
        topo_pts$r<-sqrt(topo_pts$x^2 + topo_pts$y^2)
        # ggplot(topo_pts, aes(x, y, fill = z)) + geom_raster() + scale_fill_viridis()

        plane<-rlm(z~x+y,
                   data = na.omit(topo_pts),
                   weights = 1/r,
                   scale.est = "Huber")

        topo_pts$z_pred<-predict(plane,new.data = topo_pts)
    }
}
