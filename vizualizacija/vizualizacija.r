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

zemljevid <- ggplot() + geom_polygon(data = svet, aes(x=long, y=lat, group = group))
