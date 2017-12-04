
#=====================================================
# Ejercito de tierra
#=====================================================
name <- paste0("firstReach",n,"GroundUnitValue")

t$g_ <- t[,"Winner_GroundUnitValue"]
t$l_ <- t[,"Loser_GroundUnitValue"]
tmp <- t[,c("id","Frame","g_","l_")]

#tmp_g <- tmp[tmp$g_ < 0 & tmp$Frame > 0, c("id","Frame")]
tmp_g <- tmp[tmp$g_ > n & tmp$Frame > 0 , c("id","Frame")]
names(tmp_g) <- c("id","g_Frame")
res_g <- aggregate.data.frame(tmp_g,by=list(tmp_g$id),FUN=min)

tmp_l <- tmp[tmp$l_ > n & tmp$Frame > 0 , c("id","Frame")]
names(tmp_l) <- c("id","l_Frame")
res_l <- aggregate.data.frame(tmp_l,by=list(tmp_l$id),FUN=min)

res <- merge(res_g,res_l,all = TRUE)

#res[is.na(res$g_Frame),"g_Frame"] <- .Machine$integer.max
#res[is.na(res$l_Frame),"l_Frame"] <- .Machine$integer.max

res$t <- res$g_Frame < res$l_Frame

res <- res[,c("id","t","g_Frame","l_Frame")]

print(name)
print(summary(res$t))
pie(table(res$t),main=name)

names(res) <- c("id",name,paste0(name,"_g_Frame"),paste0(name,"_l_Frame"))

info <- merge(info,res)