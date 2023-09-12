library(lidR)
setwd("~/Desktop/UNB/LIDAR")
las <- readLAS("Cor_R1_S30.las")
print(las)

unique(las$Intensity)
hist(las$UserData)

las1<- as.dataframe(las)

las_check(las)

plot(las)

plot(las, color ="RGB")


x <- plot(las, bg = "white", size = .5)

vox <- voxelize_points(las, 0.5)
plot(vox, voxel = TRUE, bg = "white")


p1 <- c(-8, 9)
p2 <- c(8, 9)
las_tr <- clip_transect(las, p1, p2, width = 1, xz = TRUE)

library(ggplot2)

ggplot(las_tr@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- rlang::enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

plot_crossection(las, colour_by = factor(Classification))




##Ground classification

#LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#las <- readLAS(LASfile, select = "xyzrn")
las_ground <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))

plot(las, color = "Classification", size = 3, bg = "white") 


p1 <- c(-8, 9)
p2 <- c(8, 9)
plot_crossection(las, p1 , p2, colour_by = factor(Classification)) #not working

library(RCSF)
las <- classify_ground(las, algorithm = csf())
plot(las, color = "Classification", size = 3, bg = "white") 


mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las, mycsf)
plot(las, color = "Classification", size = 3, bg = "white") 


gnd <- filter_ground(las)
plot(gnd, size = 3, bg = "white") 



#DEM
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzc")
plot(las, size = 3, bg = "white")

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
PRACplot_dtm3d(dtm_tin, bg = "white") 



dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 

dtm_kriging <- rasterize_terrain(las, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 


#rasterise

rst<- rasterize_canopy(las, res = .5, algorithm=p2r())
col=height.colors(25)
plot(rst, col=col)


hist(las$Intensity)



##Coordinate Transformations
library(pracma)
library(rgl)
abc<-payload(las)
abc$X


df <- data.frame(X = as.numeric(abc$X), Y = as.numeric(abc$Y), Z = as.numeric(abc$Z))
head(df)

hdf<- cbind(abc$X, abc$Y,abc$Z )

abc<- matrix(c(abc$X, abc$Y,abc$Z), ncol=3)

str(hdf)
cart2sph(hdf)

plot3d(df$x, df$y, df$z, zlim = c(0, pi))


#spherical coordinates
projection <- function(x, y, z) {
  r     <- sqrt(x^2 + y^2  + z^2)
  theta <- acos(z / r)
  phi   <- atan2(y, x)
  y <- theta * cos(phi)
  x <- theta * sin(phi)
  z <- sqrt((pi / 2)^2 - x^2 - y^2)
  data.frame(x, y, z)
}

df_spherical <- projection(abc$X, abc$Y,abc$Z)
plot3d(df_spherical$x, df_spherical$y, df_spherical$z, zlim = c(0, pi))

#projecting spherical on 2d surface
proj2_2d<- function(x, y, z) {
  X <- x/(1+z)
  Y <- y/(1+z)
  data.frame(X, Y)
}
df_spherical <- proj2_2d(df_spherical$x, df_spherical$y, df_spherical$z)
plot(df_spherical)






#cylindrical coordinates
projection_xyz <- function(x, y, z) {
  r     <- sqrt(x^2 + y^2)
  theta <- atan2(y,x)
  z <- z
  x <- pi*cos(theta)
  y <-pi*sin(theta)
  z <- z
  data.frame(x, y, z)
}

projection_theta <- function(x, y, z) {
  r     <- sqrt(x^2 + y^2)
  theta <- atan2(y,x)
  z <- z
  x <- 26*cos(theta)
  y <-26*sin(theta)
  z <- z
  data.frame(r, theta, z)
}


df_cylindrical <- projection_xyz(abc$X, abc$Y,abc$Z)

df_cylindrical_theta <- projection_theta(abc$X, abc$Y,abc$Z)


plot3d(df_cylindrical$x, df_cylindrical$y, df_cylindrical$z, zlim = c(0, pi))

#projecting cylindrical on 2d surface
proj2_2d<- function(x, y, z) {
  X <- atan2(y,x)
  Y <- z
  data.frame(X, Y)
}

proj2_2d_theta<- function(r, theta, z) {
  X <- theta
  Y <- z
  data.frame(X, Y)
}

df_cylindrical <- proj2_2d_theta(df_cylindrical_theta$r, df_cylindrical_theta$theta, df_cylindrical_theta$z)
plot(df_cylindrical)

###example

x <- 0.5*cos(pi/6); y <- 0.5*sin(pi/6); z <- sqrt(1 - x^2 - y^2)
(s <-cart2sph(c(x, y, z)))      # 0.5235988 1.0471976 1.0000000
sph2cart(s)                     # 0.4330127 0.2500000 0.8660254

cart2pol(c(1,1))                # 0.7853982 1.4142136
cart2pol(c(1,1,0))              # 0.7853982 1.4142136 0.0000000
pol2cart(c(pi/2, 1))            # 6.123234e-17 1.000000e+00
pol2cart(c(pi/4, 1, 1))
