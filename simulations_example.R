### Simulations

library (tidyverse)

traps.arrange <- c("random")
flies.num <- c(1, 10, 50, 100, 1000)
outbreak.loc <- c("within", "outside")


sim_params <- expand.grid("traps" = traps.arrange, "flies" = flies.num, "outbreak" = outbreak.loc)




output <- data.frame()

for (i in 1:nrow(sim_params)){


if (sim_params[i,]$traps == "regular"){
  make_regular_grid("my_grid")
}else{
  traps <- data.frame()
  for (xx in 1:10){
    temp <- runif(2, 0, 2000)
    traps <- rbind(traps, temp)
  }
  names (traps) <- c("Latitude", "Longitude")

  #Assign a lamba (trap attractiveness)
  make_actual_grid(gridname="orchard_traps", traps, lambda = 0.005)

  if (sim_params[i,]$outbreak == "outside"){

    outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = FALSE,
                                         outbreak_name = "outbreaks")
  }
  if (sim_params[i,]$outbreak == "within"){

    outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = TRUE,
                                         outbreak_name = "outbreaks")
  }

}

model.temp <- trapgridR("orchard_traps", nDays=28, nFlies=sim_params$flies[i], nSim=2, D=10^5, outbreaks = "outbreaks")

run.df <- model.temp$simRuns %>%
  group_by(Day)%>%
  summarise(meanEscape = mean(Cumulative.Escape.Probability))%>%
  as.data.frame()

run.df <- cbind("RunID" = paste(i), run.df)

output <- rbind(output, run.df)

}

ggplot (output, aes(x=Day, y=1-meanEscape, colour=RunID))+
  geom_line()+
  ggtitle("Sim Outputs")


