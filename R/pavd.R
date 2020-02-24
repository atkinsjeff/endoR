#' Runs the whole thing
#'
#' \code{pavd} calculates the effective number of layers in a canopy.
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



pavd <- function(m) {

#### Select Parameters ####
sub.vox=0.2 #small subvoxel size
main.vox=1 #large voxel size
top.res=2 #resolution of topographic correction

#### Get Files ####
files<-list.files("input", full.names = TRUE)

#### Read Data ####
CS_plot <- fread(files[1], header = TRUE, sep = ",")
CS_plot<-CS_plot[,1:3]
colnames(CS_plot)<-c("X","Y","Z")

#CS_plot<-center.pts(CS_plot) #uncomment if you want to center the point cloud

# create output directory
if (!dir.exists("./output")){
    dir.create("./output")
} else {
    print("Dir already exists!")
}

CS_plot.norm<-topo.correct(CS_plot, resolution = top.res)
# plot(CS_plot.norm)

#### Initial Voxelization ####
cs_vox<-vox(CS_plot.norm@data[Z>=1,1:3], sub.vox)
# fwrite(cs_vox, "output/vox.xyz", sep = " ")
colnames(cs_vox)[1:3]<-c("X","Y","Z")

#### PAVD Voxels ####
cs_vox_1m<-pts2vox(cs_vox, sub.vox, main.vox)
# fwrite(cs_vox_1m, "output/vox_PAVD.xyz", sep = " ")

#### PAVD Profile ####
PAVD_profile<-vox2profile(PAVD, Z, cs_vox_1m)
# fwrite(PAVD_profile, "output/PAVD_profile.txt", sep = " ")

# define ggplot theme
#GGPLOT THEME#
my.theme<-theme(text = element_text(size = 8.0, colour = "black")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.title.x = element_text(vjust=-0.5)) +
    theme(axis.title.y = element_text(vjust=1.5)) +
    theme(plot.title = element_text(vjust=2.5)) +
    theme(legend.position= "none")

##### Plot Results ####
plot_grid(
    ggplot(aggregate(nbpts~X+Z, FUN='sum', cs_vox),
           aes(X,Z, fill=nbpts)) + geom_tile()+my.theme+
        scale_fill_viridis()+ggtitle("Sub-Voxel Density (Side View)") + coord_equal(ratio=1),
    ggplot(aggregate(PAVD~X+Y, FUN='sum', cs_vox_1m),
           aes(X,Y, fill=PAVD)) + geom_tile()+my.theme+
        scale_fill_viridis()+ggtitle("Total PAI (Top View)") + coord_equal(ratio=1),
    ggplot(aggregate(PAVD~X+Z, FUN='sum', cs_vox_1m),
           aes(X,Z, fill=PAVD)) + geom_tile()+my.theme+
        scale_fill_viridis()+ggtitle("Total PAI (Side View)")+ coord_equal(ratio=1),
    ggplot(PAVD_profile,
           aes(y=PAVD, x=Z)) +
        ylab( expression(PAVD ~ (m^{2} ~ m^{-3}))) + xlab( expression(Height ~ (m))) +
        geom_ribbon(aes(x=Z, ymin=ci_min, ymax=ci_max), color = "blue", linetype=2, fill = "blue", alpha=0.2)+
        geom_path()+coord_flip()+my.theme+ggtitle("PAVD Profile"),
    nrow=1, labels = c("a","b","c", "d"))

ggsave("figures/test.pdf", width=3.75*4, height = 3.75)

}
