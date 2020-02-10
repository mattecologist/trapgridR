### Simulations

traps.arrange <- c("regular", "random")
flies.num <- c(5, 10, 50, 100)
outbreak.loc <- c("within", "outside")


expand.grid("traps" = traps.arrange, "flies" = flies.num, "outbreak" = outbreak.loc)
