#' Retrieve a Java instance of TrapGrid.
#'
#' Retrieve a Java instance of TrapGrid
#'
#' @param filepath Path of the trapping grid file
#' @return Model output
#' @export

trapgridR<-function(filepath= "inst/java/foogrid",
                    nDays =14, #[-nd <number of days>]
                    nFlies =100, #[-nf <number of flies per outbreak>]
                    nSim =10, # [-ns <number of simulations>]
                    D= 10^5) {
  rJava::.jinit()
  rJava::.jaddLibrary('trapgrid', 'inst/java/TrapGrid.jar')
  rJava::.jaddClassPath('inst/java/TrapGrid.jar')
  trapgrid <- rJava::.jnew('com.reallymany.trapgrid.Driver')
  rJava::.jcall(obj=trapgrid, method="main", c("-tg", filepath,
                                               "-nf", nFlies,
                                               "-nd", nDays,
                                               "-ns", nSim,
                                               "-dc", D), returnSig = "V")


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

  return(list(simRuns, fly_loc))
}



#' Setup regular trapping grid
#'
#' @param gridname Name for the trapping grid file to be output
#' @param gridSize Describes the bottom right corner in metres, from the top left corner which is 0,0
#' @param gridSpace The distance between regularly spaced traps
#' @param lambda The trap efficiency
#' @return A trapping grid text file
#' @export

make_trapping_grid <- function(gridname="footest",
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

return(print(paste("Trapping grid ", gridname, "written")))

}

