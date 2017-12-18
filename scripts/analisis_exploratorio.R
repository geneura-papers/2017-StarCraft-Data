#Librerías
source("funciones.R")

#Cargamos los datos
source("cargaDatos.R")

#Tamaño del conjunto de datos
unique.ids <- length(unique(t$id))

#Duration 
duration <- aggregate.data.frame(t$Frame,by=list(t$id),FUN = max)
names(duration) <- c("id","maxFrames")
summary(duration$maxFrames/24/60)
#boxplot(duration$maxFrames/24/60)
#hist(duration$maxFrames/24/60)

ggplot(duration,aes(x=maxFrames/24/60)) + geom_histogram(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,90,120))
#ggplot(duration,aes(x=maxFrames/24/60)) + geom_histogram(breaks=c(0,15,30,45,60,90,120),color="white") + geom_text(aes(label=..count..), vjust=-1.5)

ggplot(duration,aes(x=maxFrames/24/60)) + 
  stat_bin(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,75,90,120,150,180),color="white") +
  stat_bin(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,75,90,120,150,180), geom="text", aes(label=..count..), vjust=-1.5) +
  scale_y_continuous(limits = c(0,1000)) + 
  scale_x_continuous(breaks = c(seq(from=0,to=60,by=5),75,90,120,150,180),minor_breaks = seq(from=0,to=165,by=5)) +
  xlab("Duration (minutes)") + ylab("Number of games") +
  theme_bw()

#ggsave(filename = "../Repositorios/Revistas y Congresos/2017-StarCraft-Data/imgs/histogram_duration_games.eps",width = )

ggplot(duration,aes(x=maxFrames/24/60)) + 
  stat_bin(binwidth=15,color="white") +  stat_bin(binwidth=15, geom="text", aes(label=..count..), vjust=-1.5) +
  scale_y_continuous(limits = c(0,2000)) + scale_x_continuous(breaks = seq(from=0,to=165,by=15)) +
  xlab("Duration (minutes)") + ylab("Number of games") +
  theme_bw() 
