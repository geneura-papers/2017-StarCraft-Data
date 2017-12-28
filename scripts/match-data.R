match.data <- data.frame(id=character(),
                         Winner=character(),
                         Loser=character(),
                         Duration=integer())
for( i in 1:nrow(duration)) {
  id <- duration[i,"id"]
  t.row <- t[t$id==id,][1,]
  match.data <- rbind( match.data,
                       data.frame(id=id,
                                  Winner=t.row$Winner_Race,
                                  Loser=t.row$Loser_Race,
                                  Duration=duration[i,"maxFrames"]))
}
