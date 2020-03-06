#' Retrieve a Java instance of TrapGrid.
#'
#' Retrieve a Java instance of TrapGrid
#'
#' @param filepath Path of the trapping grid file
#' @param nDays Number of days to run simulation
#' @param nFlies Number of flies per outbreak
#' @param nSim Number of simulations
#' @param D Dispersal parameter
#' @return Model output
#' @export

trapgridR<-function(filepath= paste0(system.file(package="trapgridR"), "/java/foogrid"),
                    nDays =14, #[-nd <number of days>]
                    nFlies =100, #[-nf <number of flies per outbreak>]
                    nSim =10, # [-ns <number of simulations>]
                    D= 10^5,
                    outbreaks=NULL) {
  rJava::.jinit()
  rJava::.jaddLibrary('trapgrid', paste0(system.file(package="trapgridR"), "/java/TrapGrid.jar"))
  rJava::.jaddClassPath(paste0(system.file(package="trapgridR"), "/java/TrapGrid.jar"))
  trapgrid <- rJava::.jnew('com.reallymany.trapgrid.Driver')


   if (!is.null(outbreaks)){
    rJava::.jcall(obj=trapgrid, method="main", c("-tg", filepath,
                                                 "-nf", nFlies,
                                                 "-nd", nDays,
                                                 "-dc", D,
                                                 "-ob", outbreaks), returnSig = "V")
   } else {
     rJava::.jcall(obj=trapgrid, method="main", c("-tg", filepath,
                                                  "-nf", nFlies,
                                                 "-nd", nDays,
                                                  "-ns", nSim,
                                                 "-dc", D), returnSig = "V")
   }


  hold <- scan(paste0("out.txt"), skip=4, blank.lines.skip = TRUE, nlines=nDays)
  outfile <- data.frame(cbind(hold[seq(1, length(hold), 2)], hold[seq(2, length(hold), 2)]))
  colnames(outfile) <- c("Day",	"Av Cumulative Escape Probability")

  fly_loc <- read.csv("fly.csv", header = FALSE)
  colnames(fly_loc) <- c("Simulation Number", "Day", "X", "Y")

  #Remove closing parenthesis from Y Coord
  fly_loc <- data.frame(lapply(fly_loc, function(x) {
    gsub('\\)', "", x)
  }))
  #Remove Opening parenthesis from X Coord
  fly_loc <- data.frame(lapply(fly_loc, function(x) {
    gsub('\\(', "", x)
  }))

  # add number of each fly per rep
  fly_loc$flynum <- rep(1:nFlies, nDays)

  ## Read output for for all individual simulations
  hold <- readLines("out.txt")
  simresults <- grep("Outbreak",hold)

  simRuns <- data.frame()

  for (i in 1:length(simresults)){
    temp <- hold[(simresults[i]+1):(simresults[i]+nDays)]
    temp <- unlist(strsplit(temp, paste("\t")))
    temp <- data.frame("SimRun"=i, cbind(temp[seq(1, length(temp), 3)],temp[seq(2, length(temp), 3)], temp[seq(3, length(temp), 3)]))
    colnames(temp) <- c("SimRun","Outbreak.Location","Day","Cumulative.Escape.Probability")
    temp[,3] <- as.numeric(as.character(temp[,3]))
    temp[,4] <- as.numeric(as.character(temp[,4]))
    simRuns <- rbind(simRuns, temp)
  }

  #head (simRuns)
  fn1 <- "out.txt"
  if (file.exists(fn1)){file.remove(fn1)}
  fn2 <- "fly.csv"
  #if (file.exists(fn2)){file.remove(fn2)}


  # add number of reps
  fly_loc$Simulation.Number <-rep(1:length(unique(simRuns$Outbreak.Location)), each=nFlies*nDays)

  # importantly, convert from factor
  fly_loc$X <- as.numeric(as.character(fly_loc$X))
  fly_loc$Y <- as.numeric(as.character(fly_loc$Y))

  out_list <- list(simRuns, fly_loc)
  names(out_list) <- c("simRuns", "flyLoc")

  return(out_list)
}



#' Setup regular trapping grid
#'
#' @param gridname Name for the trapping grid file to be output
#' @param gridSize Describes the bottom right corner in metres, from the top left corner which is 0,0
#' @param gridSpace The distance between regularly spaced traps
#' @param lambda The trap efficiency
#' @return A trapping grid text file
#' @export

make_regular_grid <- function(gridname="footest",
                                gridSize= c(364, 572),
                                gridSpace=80,
                                lambda=0.02){


## Generate traps with a fixed lambda value
traps <- cbind(expand.grid(seq(from=0, to=gridSize[1], by=gridSpace), seq(from=0, to=gridSize[2], by=gridSpace)), lambda)
traps <- cbind(traps[,2], traps[,1], traps[,3])
plot (traps[,1:2])


## Write out the trapping grid file - the second line is a fix to append the table to a new line - the java file can't have a tab space at the end of the first line
cat(paste(gridSize), sep="\t", file=paste0(gridname))
cat("\n", paste(""), file=paste0(gridname), append=TRUE)
write.table(traps, file=paste0(gridname),
            na = "",
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t",
            append=TRUE)

print(paste("Trapping grid ", gridname, "written"))
return(traps)

}

#' Setup random trapping grid
#'
#' Generate a random distribution of grids on a defined rectangle (in metres)
#' Can inclue a perimeter trap on each boundary if desired
#' Calculates that each trap is a mininum distance apart before writing out trap grid file
#'
#' @param gridname Name for the trapping grid file to be output
#' @param lambda The trap efficiency
#' @param n.traps Number of random traps
#' @param perim Include perimeter traps or not
#' @param x1 Width of area (begins at 0)
#' @param y1 Height of area (begins at 0)
#' @param d Minimum distance between each trap
#' @param trials How many attempts to generate trap arrangement with given parameters
#' @return A trapping grid text file
#' @export

make_random_grid <- function(n.traps=10,
                             x1=2000,
                             y1=2000,
                             d=400,
                             trials = 1000,
                             perim=FALSE,
                             gridname="footest",
                             lambda=0.02){
  gridSize <- c(x1, y1)
  for(i in 1:trials){

    randos <- cbind(runif(n.traps,0,x1),runif(n.traps,0,y1))
    if (perim == TRUE){
      peri <- (rbind(c(0, runif(1, 0, x1)),
                     c(x1, runif(1, 0, y1)),
                     c(runif(1, 0, x1),y1),
                     c(runif(1, 0, x1),0)))
      traps <- rbind(peri, randos)
    }
    else traps <- randos

    if(min(dist(traps)) >= d){
      traps <- cbind(round(traps[,2],0), round(traps[,1],0), rep(lambda, length(traps[1])))

      ## Write out the trapping grid file - the second line is a fix to append the table to a new line - the java file can't have a tab space at the end of the first line
      cat(paste(gridSize), sep="\t", file=paste0(gridname))
      cat("\n", paste(""), file=paste0(gridname), append=TRUE)
      write.table(traps, file=paste0(gridname),
                  na = "",
                  row.names = FALSE,
                  col.names = FALSE,
                  sep = "\t",
                  append=TRUE)
      plot (traps[,2]~traps[,1])

      return(message("Trapping grid ", gridname, " written"))}
  }
  return(message("Unable to generate this landscape - try making it bigger, or min distance smaller"))
}

#' Setup actual trapping grid
#'
#' @param gridname Name for the trapping grid file to be output
#' @param gridSize Describes the bottom right corner in metres, from the top left corner which is 0,0
#' @param gridSpace The distance between regularly spaced traps
#' @param lambda The trap efficiency
#' @return A trapping grid text file
#' @export

make_actual_grid <- function(gridname="footest",
                             traps=traps,
                            lambda=0.005){

traps <- cbind(traps, lambda=rep(lambda, length(traps[,1])))

gridSize <- c(max(traps[,"Latitude"]) , max(traps[,"Longitude"]))
gridSize <- round(gridSize, 0)

cat(paste(gridSize), sep="\t", file=gridname)
cat("\n", paste(""), file=gridname, append=TRUE)
write.table(traps, file=gridname,
            na = "",
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t",
            append=TRUE)

return(print(paste("Trapping grid ", gridname, "written")))
}

#' Setup outbreak file
#'
#' @param traps Name for the trapping grid file to be output
#' @param in_orchard True/False flag for whether flies can emerge from within orchard
#' @param nSim Number of sims
#' @param outbreak_name Name of file to write out
#' @param lambda The trap efficiency
#' @param per_area Flag to override the num_outbreaks and do it by area
#' @param orchard_buf Distance around orchard (in metres) that outbreaks cannot occur
#' @return A trapping grid text file
#' @export


make_outbreak_file <- function (traps=traps,
                                in_orchard = TRUE,
                                nOutbreaks=10,
                                outbreak_name = "outbreaks",
                                lambda=0.05,
                                per_area=FALSE,
                                orchard_buf=25,
                                outbreak_buf=250){
  # Function to generate an outbreak file
  # TrapGrid: "You may supply an optional Outbreak file, which is a two-column tab-delimited file containing the x and y locations of outbreaks to be simulated."
  # x1 <- min(traps[,1]-100)
  # x2 <- max(traps[,1]+100)
  # y1 <- min(traps[,2]-100)
  # y2 <- max(traps[,2]+100)
  #
  # ## ISSUE: trapgrid can't take -ve numbers.... need to apply a correction...
  # ## apply correction to traps
  # traps[,1] <- traps[,1] - x1
  # traps[,2] <- traps[,2] - y1
  #
  # outbreak_set <- as.data.frame(sp::spsample(sp::SpatialPoints(rbind(cbind(x1, y1), cbind(x2, y2))),
  #                                        n=nOutbreaks,
  #                                        type="random"))

  # outbreak_set[,1] <- outbreak_set[,1] - x1
  # outbreak_set[,2] <- outbreak_set[,2] - y1

  outbreak_set <- as.data.frame(sp::spsample(sp::SpatialPoints(rbind(cbind(min(traps[,1]), min(traps[,2])), cbind(max(traps[,1]), max(traps[,2])))),
                                          n=nOutbreaks,
                                          type="random"))

   outbreak_set[,1] <- outbreak_set[,1] - min(traps[,1])
   outbreak_set[,2] <- outbreak_set[,2] - min(traps[,2])

  outbreak_set <- round(outbreak_set, 0)

  if (in_orchard == FALSE){
    hpts <- chull(traps[,1:2])
    hpts <- c(hpts, hpts[1])
    polyline <- traps[,1:2][hpts, ]
    polytemp <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyline)), ID=1)))

    orchard_buffer <- rgeos::gBuffer(polytemp, width=orchard_buf, byid=TRUE)
    outbreak_buffer <- rgeos::gBuffer(orchard_buffer, width=outbreak_buf, byid=TRUE)

    outbreak_set <- data.frame()


    # This sets it to be 1 outbreak per 10000m^2

    if (per_area == TRUE){
    total_outbreaks <- round(raster::area(outbreak_buffer)/10000, 0)
    }else{
      total_outbreaks <- nOutbreaks
    }


    zz <- 0
    ## zz is a counter
    while (zz < total_outbreaks){
      temp_point <- sp::spsample(outbreak_buffer,
                             n=1,
                             type="random")
      if (is.na(sp::over(temp_point, orchard_buffer))){
         if (temp_point@coords[1] > 0){ # ensure all are positive
           if (temp_point@coords[2] >0 ){ # ensure all are positive
             outbreak_set <- rbind(outbreak_set, as.matrix(as.data.frame(temp_point))[,1:2])
             zz <- zz +1
           }
         }


      }
    }
  }else{
    orchard_buffer=NA
    outbreak_buffer=NA
  }

  traps_sp <- sp::SpatialPoints(traps[,1:2])
  buffer_traps <- rgeos::gBuffer(traps_sp, width=1/lambda)

  # https://github.com/bruab/TrapGrid/blob/master/src/com/reallymany/trapgrid/Outbreak.java
  # Amazing Outbreak class abstracts a collection of OutbreakLocations. The constructor expects
  # a tab-separated file containing x, y, n and D values where (x, y) is the location of
  # an outbreak, n is the number of flies and D is their diffusion coefficient.
  # @author bhall

  #outbreak_set <- cbind(outbreak_set, rep(nFlies, nrow(outbreak_set)), rep(D))

  #outbreak_set <- round(outbreak_set, 1)

  write.table(outbreak_set, outbreak_name,
              na = "",
              row.names = FALSE,
              col.names = FALSE,
              sep = "\t",
              append=FALSE)

  print(paste("Outbreak file", outbreak_name, "written"))

  return(list("outbreak_set"=outbreak_set, "traps_sp"=traps_sp,
              "buffer_traps" = buffer_traps, "orchard_buffer"=orchard_buffer,
              "outbreak_buffer"=outbreak_buffer))
}



