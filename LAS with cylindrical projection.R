library(lidR)
setwd("~/Desktop/UNB/LIDAR")
las <- readLAS("Cor_R1_S30.las")
print(las)

las_check(las)

plot(las)

plot(las, color ="RGB")


##Coordinate Transformations
library(pracma)
library(rgl)
abc<-payload(las)


df <- data.frame(X = as.numeric(abc$X), Y = as.numeric(abc$Y), Z = as.numeric(abc$Z))
head(df)


str(hdf)
cart2sph(hdf)

plot3d(df$x, df$y, df$z, zlim = c(0, pi))


#cylindrical coordinates

projection_theta <- function(x, y, z) {
  r     <- sqrt(x^2 + y^2)
  theta <- atan2(y,x)
  z <- z
  x <- 26*cos(theta) # 26 because 
  y <-26*sin(theta)
  z <- z
  data.frame(r, theta, z)
}



df_cylindrical_theta <- projection_theta(abc$X, abc$Y,abc$Z)


plot3d(df_cylindrical$x, df_cylindrical$y, df_cylindrical$z, zlim = c(0, pi))

#projecting cylindrical on 2d surface

proj2_2d_theta<- function(r, theta, z) {
  X <- theta
  Y <- z
  data.frame(X, Y)
}

df_cylindrical <- proj2_2d_theta(df_cylindrical_theta$r, df_cylindrical_theta$theta, df_cylindrical_theta$z)
plot(df_cylindrical)