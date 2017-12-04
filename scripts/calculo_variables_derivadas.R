#source("duration.R")

library(data.table)
library(reshape2)
library(ggplot2)
library(ggpmisc)


#=============================================
#Variables que quiero:
#=============================================
# Primera sangre (unidad terrestre muerte)
# Primer edificio destruido
# Crear X Soldados
# Alcanzar X Minerales
# Alcanzar X Gas
# Alcanzar X Supply
# Primera unidad enemiga observada


## CONJUNTO DE PRUEBAS

pruebas <- t[t$id=="PvP-116",]
pruebas2 <- t[t$id=="TvT-2",]

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_GroundUnitValue),color="red") + geom_point(aes(y=Winner_GroundUnitValue),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_GroundUnitValue),color="red") + geom_point(aes(y=Winner_GroundUnitValue),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_AirUnitValue),color="red") + geom_point(aes(y=Winner_AirUnitValue),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_AirUnitValue),color="red") + geom_point(aes(y=Winner_AirUnitValue),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_BuildingValue),color="red") + geom_point(aes(y=Winner_BuildingValue),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_BuildingValue),color="red") + geom_point(aes(y=Winner_BuildingValue),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=ratioGroundUnitValue),color="blue")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=ratioGroundUnitValue),color="blue")

ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_BuildingValue),color="red") + geom_point(aes(y=Winner_BuildingValue),color="green") + 
  geom_line(aes(y=Loser_Minerals),color="red") + geom_line(aes(y=Winner_Minerals),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_TotalGas),color="red") + geom_point(aes(y=Winner_TotalGas),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_TotalGas),color="red") + geom_point(aes(y=Winner_TotalGas),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_TotalMinerals),color="red") + geom_point(aes(y=Winner_TotalMinerals),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_TotalMinerals),color="red") + geom_point(aes(y=Winner_TotalMinerals),color="green")

ggplot(pruebas2,aes(x=Frame)) + geom_point(aes(y=Loser_TotalSupply),color="red") + geom_point(aes(y=Winner_TotalSupply),color="green")
ggplot(pruebas,aes(x=Frame)) + geom_point(aes(y=Loser_TotalSupply),color="red") + geom_point(aes(y=Winner_TotalSupply),color="green")

source("variable/firstBlood.R")
source("variable/firstEnemyBuildingDestroyed.R")
source("variable/firstDiscoveringEnemyBuilding.R")

n = 1000
source("variable/firstReachNminerals.R")
source("variable/firstReachNgas.R")
source("variable/firstReachNsupply.R")




#=====================================================
#Decrementa el supply
#=====================================================
t$g_ <- c(0,diff(t$Winner_Supply))
t$l_ <- c(0,diff(t$Loser_Supply))

tmp <- t[,c("id","g_","l_","Frame")]

#tmp_g <- tmp[tmp$g_ < 0 & tmp$Frame > 0, c("id","Frame")]
tmp_g <- tmp[tmp$g_ < 0 & tmp$Frame > 0 & tmp$g_ < 0, c("id","Frame")]
names(tmp_g) <- c("id","g_Frame")
res_g <- aggregate.data.frame(tmp_g,by=list(tmp_g$id),FUN=min)

tmp_l <- tmp[tmp$l_ < 0 & tmp$Frame > 0 & tmp$l_ < 0, c("id","Frame")]
names(tmp_l) <- c("id","l_Frame")
res_l <- aggregate.data.frame(tmp_l,by=list(tmp_l$id),FUN=min)

res <- merge(res_g,res_l)
res$t <- res$g_Frame > res$l_Frame

res <- res[,c("id","t","g_Frame","l_Frame")]

names(res) <- c("id","SP","SP_g_Frame","SP_l_Frame")

info <- merge(info,res)

table(info$FD)
pie(table(info$FD))


#=====================================================
# Ratio de ejército
#=====================================================

t$g_ <- t[,"ratioGroundUnitValue"]
t$l_ <- t[,"ratioGroundUnitValue"]
tmp <- t[,c("id","Frame","g_","l_")]

#tmp_g <- tmp[tmp$g_ < 0 & tmp$Frame > 0, c("id","Frame")]
tmp_g <- tmp[tmp$g_ > 15 & tmp$Frame > 0 , c("id","Frame")]
names(tmp_g) <- c("id","g_Frame")
res_g <- aggregate.data.frame(tmp_g,by=list(tmp_g$id),FUN=min)

tmp_l <- tmp[tmp$l_ < 0.5 & tmp$Frame > 0 , c("id","Frame")]
names(tmp_l) <- c("id","l_Frame")
res_l <- aggregate.data.frame(tmp_l,by=list(tmp_l$id),FUN=min)

res <- merge(res_g,res_l,all = TRUE)


res[is.na(res$g_Frame),"g_Frame"] <- .Machine$integer.max
res[is.na(res$l_Frame),"l_Frame"] <- .Machine$integer.max

res$t <- res$g_Frame < res$l_Frame

res <- res[,c("id","t","g_Frame","l_Frame")]

names(res) <- c("id","ARMY","ARMY_g_Frame","ARMY_l_Frame")

info <- merge(info,res)

table(info$FD)
pie(table(info$FD))





#=====================================================
# Ejercito
#=====================================================

t$g_ <- t[,"Winner_GroundUnitValue"]
t$l_ <- t[,"Loser_GroundUnitValue"]
tmp <- t[,c("id","Frame","g_","l_")]

#tmp_g <- tmp[tmp$g_ < 0 & tmp$Frame > 0, c("id","Frame")]
tmp_g <- tmp[tmp$g_ > 5000 & tmp$Frame > 0 , c("id","Frame")]
names(tmp_g) <- c("id","g_Frame")
res_g <- aggregate.data.frame(tmp_g,by=list(tmp_g$id),FUN=min)

tmp_l <- tmp[tmp$l_ > 5000 & tmp$Frame > 0 , c("id","Frame")]
names(tmp_l) <- c("id","l_Frame")
res_l <- aggregate.data.frame(tmp_l,by=list(tmp_l$id),FUN=min)

res <- merge(res_g,res_l,all = TRUE)

#res[is.na(res$g_Frame),"g_Frame"] <- .Machine$integer.max
#res[is.na(res$l_Frame),"l_Frame"] <- .Machine$integer.max

res$t <- res$g_Frame < res$l_Frame

table(res$t)
summary(res$t)

res <- res[,c("id","t","g_Frame","l_Frame")]

names(res) <- c("id","ARMY","ARMY_g_Frame","ARMY_l_Frame")

info <- merge(info,res)

table(info$FD)
pie(table(info$FD))
