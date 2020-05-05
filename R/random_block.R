#' Create random and regular blocks
#'
#' @param n.sides Shape of the block, how many sides (3+)
#' @param block.size Area of the block in metres squared
#' @param regular.block Force block to have evenly sized and angled edges (e.g. regular shape)
#' @author Matt Hill
#' @return A SpatialPolygon of specified size and shape
#' @export


make_random_block <- function (n.sides=4,
                               block.size=100000,
                               regular.block = F){


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
    radii <- stats::rnorm(nSides, radius, radius/10)
    angles <- stats::rnorm(nSides, angle, angle/10) * 1:nSides
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

  poly1 <- sp::Polygon(as.matrix(as.data.frame(p)))

  poly1 <- sp::SpatialPolygons(list(sp::Polygons(list(poly1), ID = "test")), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  #poly1 <- mapview::coords2Polygons(as.matrix(as.data.frame(p)), ID="test")


  ## need to rescale to 0, 0


  poly1 <- maptools::elide(poly1, shift=c(0-raster::extent(poly1)[1], 0-raster::extent(poly1)[3]))

  raster::plot (poly1)

  return(poly1)
}


#' Create random and regular traps
#'
#' @param block A SpatialPolygon - usually generated using \code{make_random_block()}
#' @param block.size Size of the block in metres squared
#' @param n.traps How many traps to place in block
#' @param regular.traps Place traps in a regular pattern through block
#' @param min.dist Minimum distance between all traps
#' @param longest.edge Identify the longest edge of the block and place a trap in the middle of it
#' @param perim Space n traps regularly around the perimeter
#' @author Matt Hill
#' @return Trap points, block polygon
#' @export

make_random_traps <- function (block=block,
                               block.size=100000,
                               n.traps=4,
                               regular.traps =F,
                               min.dist=100,
                               longest.edge=F,
                               perim = F){

  poly1 <- block

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

    zz <- sp::SpatialPoints(rbind(cbind(sum(z1[,1])/2, sum(z1[,2])/2)))

  }
  if (longest.edge == FALSE){
    zz <- rgeos::gCentroid(poly1)
  }

  ## this is just to hold the value of the sides
  block_size = sqrt(block.size)

  r <- raster::raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=raster::extent(poly1))
  r1 <- raster::rasterize(poly1, r, 1)

  D <- raster:: distanceFromPoints(object = r1, xy = zz)
  #     mxd <- which.max(D)
  #
  D <- raster::mask (D, r1)
  #
  counter <- 0
  halt <- 0



  if (perim == F){

    while (counter < 1000 & halt < 1 ){


      if (longest.edge == T){
        if (n.traps == 1){
          all.pts <- zz@coords
        }else{
          rand.pts <- raster::sampleRandom(D, n.traps-1, xy=TRUE)[,1:2]
          all.pts <- rbind(rand.pts, zz@coords)
        }
      }

      if (longest.edge == F){
        if (n.traps == 1){
          all.pts <- rbind(raster::sampleRandom(D, 1, xy=TRUE)[,1:2])
        }else{
          all.pts <- raster::sampleRandom(D, n.traps, xy=TRUE)[,1:2]
        }
      }




      if (regular.traps == T){
        #all.pts <- sp::spsample(poly1, n.traps, type="regular") ## need to get this to adhere to cellsize

        flexible.size <- 100

        all.pts <- sp::makegrid(poly1, n.traps, cellsize = flexible.size)

        marker1 <- length (all.pts[,1])

        while ( marker1 > n.traps){
          all.pts <- sp::makegrid(poly1, n.traps, cellsize = flexible.size)
          all.pts <- sp::SpatialPoints(all.pts)
          all.pts <- rgeos::gIntersection(all.pts, poly1)
          plot (poly1)
          graphics::points (all.pts)
          flexible.size <- flexible.size * 1.01
          marker1 <- length (all.pts)
        }



        all.pts <- all.pts@coords
        row.names(all.pts) <- seq_along(all.pts[,1])




        #all.pts@coords <- all.pts@coords[sample(nrow(all.pts@coords), n.traps), ]

        plot (poly1)
        graphics::points (all.pts)
      }


      if (n.traps != 1){

        if (min(stats::dist(all.pts) > min.dist, na.rm=T)){
          halt <- halt + 1
        }
      }else{
        halt <- halt + 1
      }
      counter <- sum(counter, 1)
    }

    if (counter < 1000){

      raster::plot (D)
      raster::plot (poly1, add=T)
      graphics::points (all.pts, pch=20)

      return (list(all.pts, poly1))}else{
        return(message("We tried 1000 times, but min.dist is too large..."))
      }
  }

  if (perim == T){
    k <- methods::as(poly1, "SpatialLines")
    all.pts <- sp::spsample(k, n.traps, type="regular")

    all.pts <- all.pts@coords
    row.names(all.pts) <- seq_along(all.pts[,1])

    raster::plot (D)
    raster::plot (poly1, add=T)
    graphics::points (all.pts, pch=20)

    return (list(all.pts, poly1, D))
  }
}

#' Write out traps from a block as a trapgrid file
#'
#' @param gridname Name for the trapping grid file to be output
#' @param traps Coordinates of traps
#' @param lambda The trap efficiency
#' @return A trapping grid text file
#' @export

make_block_grid <- function(gridname="footest",
                             traps=traps,
                             lambda=0.005){

  traps <- cbind(as.data.frame(traps), lambda=rep(lambda, length(as.data.frame(traps)[,1])))

  #gridSize <- raster::extent(traps)[c(2,4)]

  gridSize <- c(max(traps[,"x"]) , max(traps[,"y"]))


  gridSize <- round(gridSize, 0)

  cat(paste(gridSize), sep="\t", file=gridname)
  cat("\n", paste(""), file=gridname, append=TRUE)
  utils::write.table(traps, file=gridname,
              na = "",
              row.names = FALSE,
              col.names = FALSE,
              sep = "\t",
              append=TRUE)

  return(print(paste("Trapping grid ", gridname, "written")))
}

#' Setup outbreak file from random blocks
#'
#' @param traps The two-column trap coordinates
#' @param block The polygon of the orchard/block boundary
#' @param in_orchard True/False flag for whether flies emerge from within OR outside orchard/block
#' @param nOutbreaks Number of outbreak locations
#' @param outbreak_name Name of file to write out
#' @param outbreak_buf Distance from orchard in which outbreaks can occur
#' @param orchard_buf Distance around orchard (in metres) that outbreaks cannot occur
#' @author Matt Hill
#' @return A trapping grid text file
#' @export

make_block_outbreak <- function (traps=traps,
                                 block=block,
                                 in_orchard = FALSE,
                                 nOutbreaks=10,
                                 outbreak_name = "outbreaks",
                                 orchard_buf=25,
                                 outbreak_buf=250){
  # Function to generate an outbreak file
  # TrapGrid: "You may supply an optional Outbreak file, which is a two-column tab-delimited file containing the x and y locations of outbreaks to be simulated."

  donut_buff <- block
  # Within block is simple.

  if (in_orchard == TRUE){

    outbreak_set <- as.data.frame(sp::spsample(block,
                                               n=nOutbreaks,
                                               type="random"))

    traps_sp <- sp::SpatialPoints(traps)

    raster::plot (block)
    graphics::axis(1)
    graphics::axis(2)
    graphics::points (traps_sp, pch=20)
    graphics::points (outbreak_set)



  }

  if (in_orchard == FALSE){

    orchard_buffer <- rgeos::gBuffer(block, width=orchard_buf, byid=TRUE)
    outbreak_buffer <- rgeos::gBuffer(block, width=outbreak_buf, byid=TRUE)


    if (orchard_buf < outbreak_buf){

      # create the buffer around the block, and lets call it a donut
      donut_buff <- rgeos::gDifference(outbreak_buffer, orchard_buffer)

      #take the traps object, convert to spatial and give same extent as the donut
      traps_sp <- sp::SpatialPoints(traps)
      traps_sp@bbox <- as.matrix(raster::extent(donut_buff))

      #now shift these so that the bottom left corner is 0, 0
      donut_buff <- maptools::elide(donut_buff, shift=c(0-raster::extent(donut_buff)[1], 0-raster::extent(donut_buff)[3]))
      traps_sp <- maptools::elide(traps_sp, shift=c(0-raster::extent(traps_sp)[1], 0-raster::extent(traps_sp)[3]))


      # Now can generate the outbreak points

      outbreak_set <- sp::spsample(donut_buff,n=nOutbreaks,type="random")
      outbreak_set <- as.data.frame(outbreak_set)

      plot (donut_buff)
      graphics::axis(1)
      graphics::axis(2)
      graphics::points (traps_sp, pch=20)
      graphics::points (outbreak_set)}else{
        return(message("Outbreak buffer cannot be smaller than orchard buffer"))

      }

  }



  utils::write.table(outbreak_set, outbreak_name,
              na = "",
              row.names = FALSE,
              col.names = FALSE,
              sep = "\t",
              append=FALSE)



  print(paste("Outbreak file", outbreak_name, "written"))

  return(list("outbreak_set"=outbreak_set,
              "traps_sp"=traps_sp,
              "buffer_region"=donut_buff,
              "block"=block))

  ## need to return some parameters here to reset the position of the trapping grid prior
  ## to using make_actual_grid (gridSize in particular)
}

