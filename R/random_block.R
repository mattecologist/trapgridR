

make_random_block <- function (n.traps=4, n.sides=4, block.size=100000,
                               regular.block = F, regular.traps =F,
                               min.dist=100, longest.edge=T){


  regular.poly <- function(nSides, area) # https://stackoverflow.com/questions/4859482/how-to-generate-random-shapes-given-a-specified-area-r-language
  {
    # Find the radius of the circumscribed circle
    radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))

    # I assume the center is at (0;0) and the first point lies at (0; radius)
    points <- list(x=NULL, y=NULL)
    angles <- (2*pi)/nSides * 1:nSides

    points$x <- cos(angles) * radius
    points$y <- sin(angles) * radius

    return (points);
  }

  convex.poly <- function(nSides, area){ # https://stackoverflow.com/questions/4859482/how-to-generate-random-shapes-given-a-specified-area-r-language
    # Find the radius of the circumscribed circle, and the angle of each point if this was a regular polygon
    radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
    angle <- (2*pi)/nSides

    # Randomize the radii/angles
    radii <- rnorm(nSides, radius, radius/10)
    angles <- rnorm(nSides, angle, angle/10) * 1:nSides
    angles <- sort(angles)

    points <- list(x=NULL, y=NULL)
    points$x <- cos(angles) * radii
    points$y <- sin(angles) * radii

    # Find the area of the polygon
    m <- matrix(unlist(points), ncol=2)
    m <- rbind(m, m[1,])
    current.area <- 0.5 * (sum(m[1:nSides,1]*m[2:(nSides+1),2]) - sum(m[1:nSides,2]*m[2:(nSides+1),1]))

    points$x <- points$x * sqrt(area/current.area)
    points$y <- points$y * sqrt(area/current.area)

    return (points)
  }

if (regular.block == T){
  p <- as.data.frame(regular.poly(n.sides, block.size))
} else{

  p <- as.data.frame(convex.poly(n.sides, block.size))
}


p <- rbind(p, p[1,])
poly1 <- mapview::coords2Polygons(as.matrix(as.data.frame(p)), ID="test")


## need to rescale to 0, 0

poly1 <- maptools::elide(poly1, shift=c(0-extent(poly1)[1], 0-extent(poly1)[3]))

p <- poly1@polygons[[1]]@Polygons[[1]]@coords



if (longest.edge == TRUE){

## this bit is from taRifx.geos::cumDist

prevCoords = p[1:dim(p)[1]-1,]
currCoords = p[2:dim(p)[1],]
distToPrev = rep(NA,dim(p)[1])

for(rowNum in 2:dim(p)[1]) {
  distToPrev[rowNum] = taRifx.geo::simpledist(rbind(prevCoords[(rowNum-1),],currCoords[(rowNum-1),]))
}

z1 <- p[paste(which.max(distToPrev)-1):paste(which.max(distToPrev)),]

zz <- SpatialPoints(rbind(cbind(sum(z1[,1])/2, sum(z1[,2])/2)))

}else{
  zz <- gCentroid(poly1)
}

## this is just to hold the value of the sides
block_size = sqrt(block.size)

r <- raster::raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=extent(poly1))
r1 <- raster::rasterize(poly1, r, 1)

D <- raster:: distanceFromPoints(object = r1, xy = zz)
#     mxd <- which.max(D)
#
D <- raster::mask (D, r1)
#
counter <- 0
halt <- 0


while (counter < 1000 & halt < 1 ){


  if (longest.edge == T){
    if (n.traps == 1){
      all.pts <- zz@coords
    }else{
      rand.pts <- sampleRandom(D, n.traps-1, xy=TRUE)[,1:2]
      all.pts <- rbind(rand.pts, zz@coords)
    }
  }

  if (longest.edge == F){
      all.pts <- sampleRandom(D, n.traps, xy=TRUE)[,1:2]
  }


  if (regular.traps == T){
    all.pts <- spsample(poly1, n.traps, type="regular") ## need to get this to adhere to cellsize
    all.pts <- all.pts@coords
  }


  if (n.traps != 1){

    if (min(dist(all.pts) > min.dist, na.rm=T)){
      halt <- halt + 1
    }
  }else{
    halt <- halt + 1
  }
  counter <- sum(counter, 1)
}

if (counter < 1000){

plot (D)
plot (poly1, add=T)
points (all.pts, pch=20)

return (list(all.pts, poly1))}else{
  return(message("We tried 1000 times, but min.dist is too large..."))
}
}
