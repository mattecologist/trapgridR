


library(raster)


r <- raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=extent(0, round(block_size, 0), 0, round(block_size, 0)))
r1 <- rasterize(traps[,2:1], r, 1)
D <- distanceFromPoints(object = r1, xy = traps[,2:1])
mxd <- which.max(D)
pt <- xyFromCell(r, mxd)
set1sp <- SpatialPoints(pt)
set2sp <- SpatialPoints(traps[,2:1])

min(gDistance(set1sp, set2sp, byid=TRUE))

plot (D)
points (pt, pch=20, col="red")
title(round(min(gDistance(set1sp, set2sp, byid=TRUE)), 1))


half.norm <- function(x, d0=.9, sigma=50){
  return(d0*exp(-(x/sigma)^2))
}

half.norm(seq(0, 100, 1), sigma=50)

plot(half.norm(D, sigma=200))

cap.prob <- function(lam=0.01, d=50){
  2*(exp(lam*d)+exp(-lam*d))^(-1)
}




esc.prob <- function(lam=0.01, d=50){
 1-(2*(exp(lam*d)+exp(-lam*d))^(-1))
}


q.bar.est <- mean(values(esc.prob(1/100, D)))


