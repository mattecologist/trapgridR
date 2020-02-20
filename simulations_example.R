### Simulations

library (tidyverse)
library (trapgridR)
library (ggplot2)

block_size <- 1000 # 10 hectares

traps.arrange <- c("random")
flies.num <- c(1, 2, 5, 10, 50)
outbreak.loc <- c("outside")
lambda.range <- c(0.001, 0.005, 0.01, 0.05)
n_traps <- c(1, 10)

sim_params <- expand.grid("traps" = traps.arrange, "flies" = flies.num, "outbreak" = outbreak.loc, "lambda" = lambda.range, "n_traps" = n_traps)

sim_params <- cbind("RunID" = as.factor(seq_along(sim_params[,1])), sim_params)



output <- data.frame()

for (x in 1:10){

for (i in 1:nrow(sim_params)){


if (sim_params[i,]$traps == "regular"){
  make_regular_grid("my_grid")
}else{
  traps <- data.frame()
  for (xx in 1:sim_params$n_traps[i]){
    temp <- runif(2, 0, block_size)
    traps <- rbind(traps, temp)
  }
  names (traps) <- c("Latitude", "Longitude")

  #Assign a lamba (trap attractiveness)
  make_actual_grid(gridname="orchard_traps", traps, lambda = sim_params$lambda[i])

  if (sim_params[i,]$outbreak == "outside"){

    outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = FALSE, per_area = FALSE,
                                         nSim=sim_params$flies[i], ## is this just making 10 outbreaks and disregarding fly numbers?
                                         outbreak_name = "outbreaks")
  }
  if (sim_params[i,]$outbreak == "within"){

    outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = TRUE, per_area = FALSE,
                                         nSim=sim_params$flies[i],
                                         outbreak_name = "outbreaks")
  }

}

model.temp <- trapgridR("orchard_traps", nDays=14, nFlies=sim_params$flies[i], D=10^5, outbreaks = "outbreaks")

run.df <- model.temp$simRuns %>%
  group_by(Day, SimRun)%>%
  summarise(meanEscape = mean(Cumulative.Escape.Probability))%>%
  as.data.frame()

run.df <- cbind("RunID" = paste(i), run.df)

output <- rbind(output, run.df)

}

}
output <- left_join(sim_params, output)

ggplot (output, aes(x=Day, y=1-meanEscape, group=interaction(RunID)))+
 # geom_line(alpha=0.5)+
  geom_smooth(aes(colour=RunID),alpha=0.3)+
  ggtitle("Sim Outputs")


output2 <- output[output$Day == 14,]

ggplot(output2, aes(x=RunID, y=meanEscape))+geom_boxplot(aes(fill=outbreak))

ggplot(output2, aes(x=RunID, y=1-meanEscape))+
  geom_boxplot(aes(fill=as.factor(n_traps)))+
  facet_wrap(~lambda, scales="free_x")

ggplot(output2)+
  geom_density(aes(meanEscape, fill=as.factor(flies)), alpha=0.5)


