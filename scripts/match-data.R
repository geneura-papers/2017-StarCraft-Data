require(data.table)
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
