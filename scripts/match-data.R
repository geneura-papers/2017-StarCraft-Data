require(data.table)
require(plyr)
t <- read.csv2("datos/normalizado.csv")
load("datos/duration.Rdata")

match.data <- data.frame(id=character(),
                         Winner=character(),
                         Loser=character(),
                         Duration=integer())

for( i in 1:nrow(duration)) {
    id <- duration[i,"id"]
    print(paste( "Doing ",i, id ))
    t.row <- t[t$id==id,][1,]
    print(paste(t.row$Winner_Race,t.row$Loser_Race))
    match.data <- rbind( match.data,
                        data.frame(id=id,
                                   Winner=t.row$Winner_Race,
                                   Loser=t.row$Loser_Race,
                                   Duration=duration[i,"maxFrames"]))
}
match.data$WL=interaction(match.data$Winner,match.data$Loser)

wins.data <- data.frame(Race=character(),
                        Wins=integer(),
                        Loses=integer(),
                        Ratio=double())

for (i in c("Protoss","Zerg","Terran")) {
    race.data <- match.data[ match.data$Winner == i |  match.data$Loser == i,]
    wins.data <- rbind( wins.data,
                       data.frame( Race=i,
                                  Wins=length(race.data[match.data$Winner == i,]$id),
                                  Loses=length(race.data[match.data$Loser == i,]$id) )
                       )
}
wins.data$Ratio = wins.data$Wins/(  wins.data$Wins + wins.data$Loses)                         
                                  
                                  
