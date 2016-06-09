# 3. faza: Izdelava zemljevida

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
#                             "OB/OB", encoding = "Windows-1250")

# Preuredimo podatke, da jih bomo lahko izrisali na zemljevid.
#druzine <- preuredi(druzine, zemljevid, "OB_UIME", c("Ankaran", "Mirna"))

# Izračunamo povprečno velikost družine.
#druzine$povprecje <- apply(druzine[1:4], 1, function(x) sum(x*(1:4))/sum(x))
#min.povprecje <- min(druzine$povprecje, na.rm=TRUE)
#max.povprecje <- max(druzine$povprecje, na.rm=TRUE)


source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
library(ggplot2)
library(dplyr)

pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                        "ne_110m_admin_0_countries")

svet <- pretvori.zemljevid(svet)
svet <- as.data.frame(svet)
names(GMs_per_capita)[1] <- "name"
svet1 <- merge(svet, GMs_per_capita, all.x=TRUE)
svet1 <- svet1[order(svet1[,1], svet1[,4]),]

zemljevid1 <- ggplot() + geom_polygon(data = svet1, aes(x=long, y=lat, group = group, fill=`GMs per million`), color="grey") +
  scale_fill_continuous(low = "#3742A8", high = "#ED1310") + xlab("") + ylab("")
zemljevid1

names(velemojstri)[1] <- "name"
svet2 <- merge(svet, velemojstri, all.x=TRUE)
svet2 <- svet2[order(svet2[,1], svet2[,4]),]
zemljevid2 <- ggplot() + geom_polygon(data = svet2, aes(x=long, y=lat, group = group, fill=`Active GMs`), color="grey") +
  scale_fill_continuous(low = "#52EADB", high = "#0C3A35") + xlab("") + ylab("")
zemljevid2

names(education_exp)[1] <- "name"
svet3 <- merge(svet, education_exp, all.x=TRUE)
svet3 <- svet3[order(svet3[,1], svet3[,4]),]
zemljevid3 <- ggplot() + geom_polygon(data = svet3, aes(x=long, y=lat, group = group, fill=`% of GDP`), color="grey") +
  scale_fill_continuous(low = "#86D9CF", high = "#3A14F6") + xlab("") + ylab("")
zemljevid3


