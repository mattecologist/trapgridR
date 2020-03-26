cols <- viridis::viridis(3)
library (ggsci)
library (ggplot2)

traps <- data.frame()
for (i in 1:10){
  temp <- runif(2, 0, 1000)
  traps <- rbind(traps, temp)
}
names (traps) <- c("Latitude", "Longitude")

#traps[,1] <- traps[,1]+1000
#traps[,2] <- traps[,2]+1000

plot (traps[,"Latitude"]~ traps[,"Longitude"])




make_actual_grid(gridname="orchard_traps", traps, lambda = 0.005)

outbreaks.data <- make_outbreak_file(traps=traps, in_orchard = FALSE, per_area = FALSE,
                                     nOutbreaks=10, ## is this just making 10 outbreaks and disregarding fly numbers?
                                     outbreak_name = "outbreaks",
                                     outbreak_buf = 100,
                                     orchard_buf = 1000)



model2 <- trapgridR(filepath="orchard_traps", nDays=28, nFlies=1, D=10^5, outbreaks = "outbreaks")



outbreaks <- tidyr::separate(model2$simRuns, Outbreak.Location, into=c("X", "Y"), sep=",")[,c("X", "Y")]
outbreaks <- outbreaks[complete.cases(outbreaks),]
outbreaks$X <- as.numeric(gsub("\\(|\\)", "", outbreaks$X))
outbreaks$Y <- as.numeric(gsub("\\(|\\)", "", outbreaks$Y))
outbreaks <- unique(cbind(outbreaks$X, outbreaks$Y))
outbreaks <- sp::SpatialPoints(outbreaks[,1:2])


test_outbreaks <- as.data.frame(outbreaks.data$outbreak_set)
colnames (test_outbreaks) <- c("Latitude", "Longitude")


ggplot (data=as.data.frame(outbreaks.data$traps_sp), aes(Latitude, Longitude)) +
  geom_point(size=2, shape=7)+
  #geom_point(data=as.data.frame(outbreaks), aes(coords.x1, coords.x2), colour=cols[3], alpha=0.8)+
  geom_point(data=test_outbreaks, aes(Latitude, Longitude), colour=cols[3], alpha=1, shape=13, size=10)+
  geom_polygon(data=outbreaks.data$buffer_traps, aes(x=long, y=lat, group = group), colour=cols[2], fill=cols[2], alpha=0.45)+
  geom_polygon(data=outbreaks.data$orchard_buffer, aes(x=long, y=lat, group = group), alpha=0.2, fill=cols[1])+
  geom_point(data=model2$flyLoc[model2$flyLoc$Day==14,], aes(X, Y, colour=as.factor(Simulation.Number)), show.legend = F)+
  theme_minimal()+
  coord_equal()+
  xlab("X (m)")+
  ylab("Y (m)")


library (gganimate)
ggplot (model2$flyLoc[model2$flyLoc$Simulation.Number==1:4,], aes(as.integer(X), as.integer(Y),  colour=as.factor(Simulation.Number)))+
  geom_point()+
  transition_time(as.integer(Day)) +
  scale_colour_viridis_d()+
  theme_minimal()+
  facet_wrap(~Simulation.Number, nrow = 2)




