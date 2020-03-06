### Simulations

library (tidyverse)
library (trapgridR)
library (ggplot2)

block_size <- 1000 # 10 hectares

traps.arrange <- c("random")
flies.num <- c(10)

lambda.range <- 0.005
n_traps <- c(1,2, 5, 10, 20)

away_from_orch <- c(1000)
d.values <- c(10^5)


sim_params <- expand.grid("traps" = traps.arrange, "flies" = flies.num, "lambda" = lambda.range,
                          "n_traps" = n_traps,
                          "away_from_orch" = away_from_orch,
                          "d.values" = d.values)

sim_params <- cbind("RunID" = as.factor(seq_along(sim_params[,1])), sim_params)



output <- data.frame()

for (x in 1:10){

for (i in 1:nrow(sim_params)){


if (sim_params[i,]$traps == "regular"){
  traps <-make_regular_grid(gridname= "orchard_traps",
                    gridSize = c(1000, 1000),
                    gridSpace = 400,
                    lambda= sim_params$lambda[i])

  traps<-as.data.frame(traps)
  names (traps) <- c("Latitude", "Longitude", "lamda")


}else{
  traps <- data.frame()
  for (xx in 1:sim_params$n_traps[i]){
    temp <- runif(2, 0, block_size)
    traps <- rbind(traps, temp)

  names (traps) <- c("Latitude", "Longitude")

  #Assign a lamba (trap attractiveness)
  make_actual_grid(gridname="orchard_traps", traps, lambda = sim_params$lambda[i])
}

}

    outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = FALSE, per_area = FALSE,
                                         nOutbreaks =1, ## is this just making 10 outbreaks and disregarding fly numbers?
                                         outbreak_name = "outbreaks",
                                         orchard_buf = sim_params$away_from_orch[i])




model.temp <- trapgridR("orchard_traps", nDays=14, nFlies=sim_params$flies[i], nSim=10, D=sim_params$d.values[i])


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
  #geom_point(data=as.data.frame(outbreaks), aes(coords.x1, coords.x2), colour=cols[3], alpha=0.8)+
  geom_point(data=test_outbreaks, aes(Latitude, Longitude), colour=cols[3], alpha=1, shape=13, size=10)+
  geom_polygon(data=outbreaks.data$buffer_traps, aes(x=long, y=lat, group = group), colour=cols[2], fill=cols[2], alpha=0.45)+
  geom_polygon(data=outbreaks.data$orchard_buffer, aes(x=long, y=lat, group = group), alpha=0.2, fill=cols[1])+
  geom_point(data=model.temp$flyLoc[model.temp$flyLoc$Day==1,], aes(X, Y, colour=as.factor(Simulation.Number)), show.legend = F)+
  theme_minimal()+
  coord_equal()+
  xlab("X (m)")+
  ylab("Y (m)")

ggsave(file=paste0("./dump/trap_",x,"_", i,"_day1.png"), height=4, width=5)


ggplot (data=as.data.frame(outbreaks.data$traps_sp), aes(Latitude, Longitude)) +
  geom_point(size=2, shape=7)+
  #geom_point(data=as.data.frame(outbreaks), aes(coords.x1, coords.x2), colour=cols[3], alpha=0.8)+
  geom_point(data=test_outbreaks, aes(Latitude, Longitude), colour=cols[3], alpha=1, shape=13, size=10)+
  geom_polygon(data=outbreaks.data$buffer_traps, aes(x=long, y=lat, group = group), colour=cols[2], fill=cols[2], alpha=0.45)+
  geom_polygon(data=outbreaks.data$orchard_buffer, aes(x=long, y=lat, group = group), alpha=0.2, fill=cols[1])+
  geom_point(data=model.temp$flyLoc[model.temp$flyLoc$Day==14,], aes(X, Y, colour=as.factor(Simulation.Number)), show.legend = F)+
  theme_minimal()+
  coord_equal()+
  xlab("X (m)")+
  ylab("Y (m)")

ggsave(file=paste0("./dump/trap_",x,"_", i,"_day14.png"), height=4, width=5)


run.df <- model.temp$simRuns %>%
  group_by(Day, SimRun)%>%
  summarise(meanEscape = mean(Cumulative.Escape.Probability))%>%
  as.data.frame()

run.df <- cbind("RunID" = paste(i), run.df)

output <- rbind(output, run.df)

}

}

output <- left_join(sim_params, output)


output2 <- output[output$Day == 14,]

ggplot(output2, aes(x=RunID, y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(n_traps)))+
  facet_wrap(~traps, scales="free_x")

ggplot(output2, aes(x=RunID, y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(d.values)))+
  facet_wrap(~away_from_orch*d.values, scales="free_x")


ggplot(output2, aes(x=RunID, y=meanEscape))+geom_boxplot(aes(fill=outbreak))

ggplot (output, aes(x=Day, y=1-meanEscape, group=interaction(RunID)))+
  # geom_line(alpha=0.5)+
  geom_smooth(aes(colour=RunID),alpha=0.3)+
  ggtitle("Sim Outputs")


ggplot(output2)+
  geom_density(aes(meanEscape, fill=as.factor(flies)), alpha=0.5)


