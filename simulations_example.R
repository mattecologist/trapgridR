### Simulations

library (tidyverse)
library (trapgridR)
library (ggplot2)
library (raster)
library (rgeos)
library (gganimate)

cols <- c("#0073C2FF", "#EFC000FF", "#868686FF")
block_size <- 316.2278 # 10 hectares


trap.dist <- function(x){
  r <- raster(ncols=round(block_size, 0), nrows=round(block_size, 0), crs=NULL, ext=extent(0, round(block_size, 0), 0, round(block_size, 0)))
  r1 <- rasterize(x, r, 1)
  D <- distanceFromPoints(object = r1, xy = x)
  mxd <- which.max(D)
  pt <- xyFromCell(r, mxd)
  set1sp <- SpatialPoints(pt)
  set2sp <- SpatialPoints(traps[,2:1])

  return(min(gDistance(set1sp, set2sp, byid=TRUE)))
}





### SET 1 ####################### Full design
traps.arrange <- c("random", "perim", "regular")
flies.num <- c(1, 10)

lambda.range <- c(0.025, 0.05)
n_traps <- c(5)

away_from_orch <- c(100)
d.values <- c(10^3)
##################################


### SET 2 ####################### Number of traps
traps.arrange <- c("random")
flies.num <- c(10)

lambda.range <- c(0.05)
n_traps <- c(1, 3, 5, 9)

away_from_orch <- c(10)
d.values <- c(10^3)
##################################


### SET 3 ####################### Number of flies
traps.arrange <- c("random")
flies.num <- c(1, 10, 100)

lambda.range <- c(0.05)
n_traps <- c(5)

away_from_orch <- c(100)
d.values <- c(10^3)
##################################



sim_params <- expand.grid("traps" = traps.arrange, "flies" = flies.num, "lambda" = lambda.range,
                          "n_traps" = n_traps,
                          "away_from_orch" = away_from_orch,
                          "d.values" = d.values)




sim_params <- cbind("RunID" = as.factor(seq_along(sim_params[,1])), sim_params)



output <- data.frame()

for (x in 1:100){

  for (i in 1:nrow(sim_params)){


    if (sim_params[i,]$traps == "random"){
      traps <-make_random_grid(n.traps=sim_params[i,]$n_traps,
                               perim=F,
                               x1=block_size,
                               y1=block_size,
                               d=50,
                               gridname = "orchard_traps",
                               lambda= sim_params$lambda[i])
      traps <- as.data.frame(traps)
      names (traps) <- c("Latitude", "Longitude", "lamda")

    }else{
      if (sim_params[i,]$traps == "regular"){
        traps <- make_regular_grid("orchard_traps", gridSize = c(block_size, block_size),
                                   gridSpace = block_size,
                                   lambda = sim_params[i,]$lambda)
        traps <- rbind(traps, c((block_size/2), (block_size/2), sim_params[i,]$lambda))
        traps <- as.data.frame(traps)
        names (traps) <- c("Latitude", "Longitude", "lamda")
} else{
      if (sim_params[i,]$traps == "perim"){# perimeter trapping with a 200 m distance within the 10 ha
        traps <-make_random_grid(n.traps=sim_params[i,]$n_traps-4,
                                 perim=TRUE,
                                 x1=block_size,
                                 y1=block_size,
                                 d=50,
                                 gridname = "orchard_traps",
                                 lambda= sim_params$lambda[i])
        traps <- as.data.frame(traps)
        names (traps) <- c("Latitude", "Longitude", "lamda")
      }
    }
  }

  outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = FALSE, per_area = FALSE,
                                       nOutbreaks =1,
                                       outbreak_name = "outbreaks",
                                       orchard_buf = sim_params$away_from_orch[i])




  model.temp <- trapgridR("orchard_traps", nDays=28, nFlies=sim_params$flies[i], nSim=10, D=sim_params$d.values[i],
                          outbreaks = "outbreaks")


  outbreaks <- tidyr::separate(model.temp$simRuns, Outbreak.Location, into=c("X", "Y"), sep=",")[,c("X", "Y")]
  outbreaks <- outbreaks[complete.cases(outbreaks),]
  outbreaks$X <- as.numeric(gsub("\\(|\\)", "", outbreaks$X))
  outbreaks$Y <- as.numeric(gsub("\\(|\\)", "", outbreaks$Y))
  outbreaks <- as.data.frame(unique(cbind(outbreaks$X, outbreaks$Y)))
  outbreaks <- sp::SpatialPoints(outbreaks[,1:2])


  test_outbreaks <- as.data.frame(outbreaks.data$outbreak_set)
  colnames (test_outbreaks) <- c("Latitude", "Longitude")



  ggplot (data=as.data.frame(outbreaks.data$traps_sp), aes(Latitude, Longitude)) +
    geom_point(size=2, shape=7)+
    # geom_point(data=as.data.frame(outbreaks), aes(V1, V2), colour=cols[3], alpha=0.8)+
    geom_point(data=test_outbreaks, aes(Latitude, Longitude), colour=cols[3], alpha=1, shape=13, size=10)+
    geom_polygon(data=outbreaks.data$buffer_traps, aes(x=long, y=lat, group = group), colour=cols[2], fill=cols[2], alpha=0.45)+
    geom_polygon(data=outbreaks.data$orchard_buffer, aes(x=long, y=lat, group = group), alpha=0.2, fill=cols[1])+
    geom_point(data=model.temp$flyLoc[model.temp$flyLoc$Day==1,], aes(X, Y, colour=as.factor(Simulation.Number)), show.legend = F)+
    theme_minimal()+
    coord_equal()+
    xlab("X (m)")+
    ylab("Y (m)")+
    ggtitle(paste(round(trap.dist(traps[,2:1]), 2)))
  ggsave(file=paste0("./dump/trap_",x,"_", i,"_day1.png"), height=4, width=5)

  ggplot (data=as.data.frame(outbreaks.data$traps_sp), aes(Latitude, Longitude)) +
    geom_point(size=2, shape=7)+
    geom_point(data=as.data.frame(outbreaks), aes(V1, V2), colour=cols[3], alpha=0.8)+
    # geom_point(data=test_outbreaks, aes(Latitude, Longitude), colour=cols[3], alpha=1, shape=13, size=10)+
    geom_polygon(data=outbreaks.data$buffer_traps, aes(x=long, y=lat, group = group), colour=cols[2], fill=cols[2], alpha=0.45)+
    geom_polygon(data=outbreaks.data$orchard_buffer, aes(x=long, y=lat, group = group), alpha=0.2, fill=cols[1])+
    geom_point(data=model.temp$flyLoc[model.temp$flyLoc$Day==14,], aes(X, Y, colour=as.factor(Simulation.Number)), show.legend = F)+
    theme_minimal()+
    coord_equal()+
    xlab("X (m)")+
    ylab("Y (m)")+
    ggtitle(paste(round(trap.dist(traps[,2:1]), 2)))
  ggsave(file=paste0("./dump/trap_",x,"_", i,"_day14.png"), height=4, width=5)


  run.df <- model.temp$simRuns %>%
    group_by(Day, SimRun)%>%
    summarise(meanEscape = mean(Cumulative.Escape.Probability))%>%
    as.data.frame()

  run.df <- cbind("RunID" = paste(i), "Trap.dist" = trap.dist(traps[,2:1]), run.df)

  output <- rbind(output, run.df)

}

}

output <- left_join(sim_params, output)



### SET 1 animation
ggplot(output, aes(x=as.factor(traps), y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(traps)))+
  ggsci::scale_fill_jco()+
  facet_wrap(~lambda*d.values*flies, scales="free_x", ncol = 2)+
  transition_time(as.integer(Day))+
  #transition_states(as.integer(Day), transition_length = 3, state_length = 1)+
  labs(title = 'Day: {frame_time}')


output2 <- output[output$Day == 7,]

ggplot(output2, aes(x=traps, y=1-meanEscape))+
  geom_boxplot(aes(fill=traps))+
  ggsci::scale_fill_jco()+
  xlab("Trap Arrangement")+
  facet_grid(cols=vars(lambda), rows=vars(flies), scales="free")+
  ggtitle("After 7 Days")+
  theme_dark()

output2 <- output[output$Day == 14,]

ggplot(output2, aes(x=traps, y=1-meanEscape))+
  geom_boxplot(aes(fill=traps))+
  ggsci::scale_fill_jco()+
  xlab("Trap Arrangement")+
  facet_grid(cols=vars(lambda), rows=vars(flies), scales="free")+
  ggtitle("After 14 Days")+
  theme_dark()


## Set 2 number of traps
output2 <- output[output$Day == 7,]

ggplot(output2, aes(x=as.factor(n_traps), y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(n_traps)))+
  xlab("Number of Traps")+
  ggtitle("After 7 Days")

output2 <- output[output$Day == 14,]

ggplot(output2, aes(x=as.factor(n_traps), y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(n_traps)))+
  xlab("Number of Traps")+
  ggtitle("After 14 Days")



## Set 3 number of flies
output2 <- output[output$Day == 7,]

ggplot(output2, aes(x=as.factor(flies), y=1-meanEscape, fill=as.factor(flies)))+
  geom_boxplot()+
  xlab("Number of Flies")+
  ggtitle("After 7 Days")

output2 <- output[output$Day == 14,]

ggplot(output2, aes(x=as.factor(flies), y=1-meanEscape, fill=as.factor(flies)))+
  geom_boxplot()+
  xlab("Number of Flies")+
  ggtitle("After 14 Days")





ggplot(output2, aes(x=RunID, y=meanEscape))+geom_boxplot(aes(fill=outbreak))

ggplot (output, aes(x=Day, y=1-meanEscape, group=interaction(RunID)))+
  # geom_line(alpha=0.5)+
  geom_smooth(aes(colour=RunID),alpha=0.3)+
  ggtitle("Sim Outputs")


ggplot(output2)+
  geom_density(aes(meanEscape, fill=as.factor(flies)), alpha=0.5)


