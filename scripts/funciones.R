crearvariables <- function(datos){
  datos$"_ratioMineral" <- datos$Minerals / datos$EnemyMinerals
  datos$"_ratioGas" <- datos$Gas / datos$EnemyGas
  datos$"_ratioSupply" <- datos$Supply / datos$EnemySupply
  
  datos$"_ratioTotalMineral" <- datos$TotalMinerals / datos$EnemyTotalMinerals
  datos$"_ratioTotalGas" <- datos$TotalGas / datos$EnemyTotalGas
  datos$"_ratioTotalSupply" <- datos$TotalSupply / datos$EnemyTotalSupply
  
  datos$"_ratioGroundUnitValue" <- datos$GroundUnitValue / datos$EnemyGroundUnitValue
  datos$"_ratioBuildingValue" <- datos$BuildingValue / datos$EnemyBuildingValue
  datos$"_ratioAirUnitValue" <- datos$AirUnitValue / datos$EnemyAirUnitValue
  
  return(datos)
}

megagrafico <- function(datos){
  
  
  datos$"_ratioMineral" <- datos$Minerals / datos$EnemyMinerals
  datos$"_ratioGas" <- datos$Gas / datos$EnemyGas
  datos$"_ratioSupply" <- datos$Supply / datos$EnemySupply
  
  datos$"_ratioTotalMineral" <- datos$TotalMinerals / datos$EnemyTotalMinerals
  datos$"_ratioTotalGas" <- datos$TotalGas / datos$EnemyTotalGas
  datos$"_ratioTotalSupply" <- datos$TotalSupply / datos$EnemyTotalSupply
  
  datos$"_ratioGroundUnitValue" <- datos$GroundUnitValue / datos$EnemyGroundUnitValue
  datos$"_ratioBuildingValue" <- datos$BuildingValue / datos$EnemyBuildingValue
  datos$"_ratioAirUnitValue" <- datos$AirUnitValue / datos$EnemyAirUnitValue
  
  
  d <- melt(datos,id.vars = c("ReplayID","Frame"))
  
  p <- ggplot(d,aes(x=as.numeric(Frame),y=as.numeric(value))) + 
    facet_wrap(~variable,scales = "free_y") + geom_point() + 
    stat_smooth(method = "lm",formula = y~x+I(x^2)+I(x^3)) +
    stat_poly_eq(parse=T, aes(label = ..eq.label..), formula = y~x+I(x^2)+I(x^3)) + 
    ggtitle(label = paste(datos[1,"id"]), subtitle = paste0("Victory: ", datos[1,"Winner"], " (", datos[1,"WinnerRace"],") -- ", datos[1,"Duration"], " turnos" ))
  
  return(p)
  
}

#Creamos modelos
crearmodelo <- function(d, atributo = "_ratioGroundUnitValue"){
  return (lm( as.data.frame(d)[,atributo] ~ d$Frame + I(d$Frame^2) + I(d$Frame^3)))
}
