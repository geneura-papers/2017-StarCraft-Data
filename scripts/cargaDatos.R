#
# CARGA DE DATOS
# 
require("data.table")
require("stringr")
prefix <- "/home/jmerelo/Asignaturas/TFM/StarCraft-winner-prediction/data/"

if(!file.exists("datos/normalizado.csv")){

                                        #Carga de los datos
    data.pvp <- read.csv(paste0(prefix,"data_pvp.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.pvp$Race = "Protoss"
    data.pvp$RaceEnemy = "Protoss"

    data.pvt <- read.csv(paste0(prefix,"data_pvt.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.pvt$Race = "Protoss"
    data.pvt$RaceEnemy = "Terran"
    
    data.pvz <- read.csv(paste0(prefix,"data_pvz.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.pvz$Race = "Protoss"
    data.pvz$RaceEnemy = "Zerg"
    
    data.tvt <- read.csv(paste0(prefix,"data_tvt.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.tvt$Race = "Terran"
    data.tvt$RaceEnemy = "Terran"
    
    data.tvz <- read.csv(paste0(prefix,"data_tvz.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.tvz$Race = "Terran"
    data.tvz$RaceEnemy = "Zerg"
    
    data.zvz <- read.csv(paste0(prefix,"data_zvz.csv"), colClasses=c("integer",rep("numeric",28),"factor"))
    data.zvz$Race = "Zerg"
    data.zvz$RaceEnemy = "Zerg"

#Concatenamos
    data <- rbindlist(list(data.pvp,data.pvt,data.pvp,data.tvt,data.tvz,data.zvz),use.names = TRUE, fill=TRUE)
    rm(data.pvp,data.pvt,data.pvz,data.tvt,data.tvz,data.zvz)
    
    
    data$WinnerRace <- ""
    data[data$Winner=="A","WinnerRace"] <- data[data$Winner=="A","Race"]
    data[data$Winner=="B","WinnerRace"] <- data[data$Winner=="B","RaceEnemy"]
    
    data$LoserRace <- ""
    data[data$Winner=="A","LoserRace"] <- data[data$Winner=="A","RaceEnemy"]
    data[data$Winner=="B","LoserRace"] <- data[data$Winner=="B","Race"]
    
    data$id <- paste(str_sub(data$Race,start=1,end=1),"v",str_sub(data$RaceEnemy,start=1,end=1),"-",data$ReplayID,sep = "")
    
                                        #Pre-procesamiento
    
    data$win <- FALSE
    data$win <- (data$Winner == "A")
    
                                        #Primer conjunto
    c1 <- data[data$win==TRUE,]
                                        #names(c1) <- c("RaceWinner","RaceLoser","Winner","WinnerRace","id","Frame","variable","value","win")
    names(c1) <- c("ReplayID","Duration","Frame",
                   "Winner_Minerals","Winner_Gas","Winner_Supply","Winner_TotalMinerals","Winner_TotalGas","Winner_TotalSupply",
                   "Winner_GroundUnitValue","Winner_BuildingValue","Winner_AirUnitValue",
                   "Winner_ObservedEnemyGroundUnitValue","Winner_ObservedEnemyBuildingValue","Winner_ObservedEnemyAirUnitValue",
                   "Loser_Minerals","Loser_Gas","Loser_Supply","Loser_TotalMinerals","Loser_TotalGas","Loser_TotalSupply",
                   "Loser_GroundUnitValue","Loser_BuildingValue","Loser_AirUnitValue",
                   "Loser_ObservedEnemyGroundUnitValue","Loser_ObservedEnemyBuildingValue","Loser_ObservedEnemyAirUnitValue",
                   "Winner_ObservedResourceValue","Loser_ObservedResourceValue",
                   "Winner","Winner_Race","Loser_Race","WinnerRace","LoserRace","id","win")
    
    c2 <- data[data$win==FALSE,]
                                        #names(c2) <- c("RaceLoser","RaceWinner","Winner","WinnerRace","id","Frame","variable","value","win")
    names(c2) <- c("ReplayID","Duration","Frame",
                   "Loser_Minerals","Loser_Gas","Loser_Supply","Loser_TotalMinerals","Loser_TotalGas","Loser_TotalSupply",
                   "Loser_GroundUnitValue","Loser_BuildingValue","Loser_AirUnitValue",
                   "Loser_ObservedEnemyGroundUnitValue","Loser_ObservedEnemyBuildingValue","Loser_ObservedEnemyAirUnitValue",
                   "Winner_Minerals","Winner_Gas","Winner_Supply","Winner_TotalMinerals","Winner_TotalGas","Winner_TotalSupply",
                   "Winner_GroundUnitValue","Winner_BuildingValue","Winner_AirUnitValue",
                   "Winner_ObservedEnemyGroundUnitValue","Winner_ObservedEnemyBuildingValue","Winner_ObservedEnemyAirUnitValue",
                   "Loser_ObservedResourceValue","Winner_ObservedResourceValue",
                   "Winner","Loser_Race","Winner_Race","WinnerRace","LoserRace","id","win")
    
    t <- rbind.data.frame(c1,c2[,c("ReplayID","Duration","Frame",
                                   "Winner_Minerals","Winner_Gas","Winner_Supply","Winner_TotalMinerals","Winner_TotalGas","Winner_TotalSupply",
                                   "Winner_GroundUnitValue","Winner_BuildingValue","Winner_AirUnitValue",
                                   "Winner_ObservedEnemyGroundUnitValue","Winner_ObservedEnemyBuildingValue","Winner_ObservedEnemyAirUnitValue",
                                   "Loser_Minerals","Loser_Gas","Loser_Supply","Loser_TotalMinerals","Loser_TotalGas","Loser_TotalSupply",
                                   "Loser_GroundUnitValue","Loser_BuildingValue","Loser_AirUnitValue",
                                   "Loser_ObservedEnemyGroundUnitValue","Loser_ObservedEnemyBuildingValue","Loser_ObservedEnemyAirUnitValue",
                                   "Winner_ObservedResourceValue","Loser_ObservedResourceValue",
                                   "Winner","Winner_Race","Loser_Race","WinnerRace","LoserRace","id","win")])
    rm(c1,c2)
    
    t <- t[,c("id","ReplayID","Duration","Frame","Winner_Race","Loser_Race",
              "Winner_Minerals","Winner_Gas","Winner_Supply","Winner_TotalMinerals","Winner_TotalGas","Winner_TotalSupply",
              "Winner_GroundUnitValue","Winner_BuildingValue","Winner_AirUnitValue",
              "Winner_ObservedEnemyGroundUnitValue","Winner_ObservedEnemyBuildingValue","Winner_ObservedEnemyAirUnitValue",
              "Loser_Minerals","Loser_Gas","Loser_Supply","Loser_TotalMinerals","Loser_TotalGas","Loser_TotalSupply",
              "Loser_GroundUnitValue","Loser_BuildingValue","Loser_AirUnitValue",
              "Loser_ObservedEnemyGroundUnitValue","Loser_ObservedEnemyBuildingValue","Loser_ObservedEnemyAirUnitValue",
              "Winner_ObservedResourceValue","Loser_ObservedResourceValue")]
    
                                        #Calculamos ratio vencedor/perdedor
    
    t$"ratioMineral" <- t$Winner_Minerals / t$Loser_Minerals
    t$"ratioGas" <- t$Winner_Gas / t$Loser_Gas
    t$"ratioSupply" <- t$Winner_Supply / t$Loser_Supply
    
    t$"ratioTotalMineral" <- t$Winner_TotalMinerals / t$Loser_TotalMinerals
    t$"ratioTotalGas" <- t$Winner_TotalGas / t$EnemyLoser_Gas
    t$"ratioTotalSupply" <- t$Winner_TotalSupply / t$Loser_TotalSupply
    
    t$"ratioGroundUnitValue" <- t$Winner_GroundUnitValue / t$Loser_GroundUnitValue
    t$"ratioBuildingValue" <- t$Winner_BuildingValue / t$Loser_BuildingValue
    t$"ratioAirUnitValue" <- t$Winner_AirUnitValue / t$Loser_AirUnitValue
    
    info <- unique(t[,c("id","Winner_Race","Loser_Race","Duration")])
    
    write.csv2("datos/normalizado.csv",x = t)

    rm(data)

}else{

    t <- read.csv2("datos/normalizado.csv")
  
}
