#' Random traps per hectare
#'
#' @param points The two-column existing trap coordinates
#' @param poly The polygon of the orchard/block boundary
#' @param traps.per.hectare Density of traps per hectare required
#' @author Matt Hill
#' @return 2 column coordinates of all traps
#' @export
#'
traps_per_hectare <- function(points=points,
                              poly=poly,
                              traps.per.hectare=1){

  while (nrow(points) < round(round(sum(raster::area(poly))/10000, 1)*traps.per.hectare, 0)){

    block_size = sqrt(sum(raster::area(poly)))
    r <- raster::raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=raster::extent(poly))
    r1 <- raster::rasterize(poly, r, 1)

    D <- raster:: distanceFromPoints(object = r1, xy = points)
    D <- raster::mask (D, r1)
    mxd <- raster::which.max(D)
    pt <- raster::xyFromCell(D, mxd)

    points <- rbind(points, pt)
  }

  raster::plot (D)
  plot(poly, add=T)
  points (points, pch=20)

  return(points)

}


#' Random traps per distance from trap
#'
#' @param points The two-column existing trap coordinates
#' @param poly The polygon of the orchard/block boundary
#' @param dist.thresh Minimum distance between all traps and all parts of the block
#' @author Matt Hill
#' @return 2 column coordinates of all traps
#' @export
#'
traps_per_distance <- function(points=points,
                               poly=poly,
                               dist.thresh = 100){

  block_size = sqrt(sum(raster::area(poly)))
  r <- raster::raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=raster::extent(poly))
  r1 <- raster::rasterize(poly, r, 1)

  trap.dist <- 100000

  while (trap.dist > dist.thresh){

    D <- raster:: distanceFromPoints(object = r1, xy = points)
    D <- raster::mask (D, r1)


    mxd <- raster::which.max(D)
    trap.dist <- raster::extract (D, raster::xyFromCell(D, mxd))

    if (trap.dist > dist.thresh){


      D2 <- raster::reclassify(D, c(-Inf, dist.thresh, NA, dist.thresh, Inf, 1))

      poly.temp <- raster::rasterToPolygons(D2, dissolve=T)


      if (sum(raster::area(poly.temp))/dist.thresh^2 > 2.5){
        points.temp <- (sp::makegrid(poly.temp, cellsize = 10))
      } else{
        points.temp <- as.data.frame(sp::spsample(poly.temp, 1, type="regular"))

      }


      biggest.dist <- cbind(points.temp, raster::extract (D, points.temp))
      pt <- as.matrix(biggest.dist[which.max(biggest.dist[,3]),c(1, 2)])


      points <- rbind(points, pt)
      raster::plot (D2)
    }

  }

  raster::plot (D)
  graphics::plot(poly, add=T)
  graphics::points (points, pch=20)


  row.names(points) <- seq_along(points[,1])

  return(points)

}
