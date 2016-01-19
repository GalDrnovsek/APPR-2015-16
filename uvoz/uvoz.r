# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
#uvozi.druzine <- function() {
#  return(read.table("podatki/druzine.csv", sep = ";", as.is = TRUE,
#                      row.names = 1,
#                      col.names = c("obcina", "en", "dva", "tri", "stiri"),
#                      fileEncoding = "Windows-1250"))
#}

# Zapišimo podatke v razpredelnico druzine.
#druzine <- uvozi.druzine()

#obcine <- uvozi.obcine()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

require(dplyr)
require(rvest)
require(gsubfn)
require(ggplot2)

url1 <- "http://www.becomeawordgameexpert.com/stats.htm"
#Over 2 million chess matches.
link1 <- sprintf(url1)
stran1 <- html_session(link1) %>% read_html(encoding = "UTF-8")
tabela1 <- stran1 %>% html_nodes(xpath = "//table[1]") %>% html_table()
Chess_opening_statistics <- data.frame(tabela1)
Chess_opening_statistics$X5 <- NULL
names(Chess_opening_statistics)[1] <- "Opening name"
names(Chess_opening_statistics)[2] <- "White win (%)"
names(Chess_opening_statistics)[3] <- "Black win (%)"
names(Chess_opening_statistics)[4] <- "Draw (%)"
Chess_opening_statistics <- Chess_opening_statistics[-1,]
row.names(Chess_opening_statistics) <- 1:290
Chess_opening_statistics[,2] <- as.numeric(Chess_opening_statistics[,2])
Chess_opening_statistics[,3] <- as.numeric(Chess_opening_statistics[,3])
Chess_opening_statistics[,4] <- as.numeric(Chess_opening_statistics[,4])
Chess_opening_statistics <- filter(Chess_opening_statistics,!grepl("^-",`Opening name`))
Chess_opening_statistics <- Chess_opening_statistics[-33,]
Chess_opening_statistics <- Chess_opening_statistics[-33,]
Chess_opening_statistics <- Chess_opening_statistics[-50,]
Chess_opening_statistics <- Chess_opening_statistics[-50,]
Chess_opening_statistics <- Chess_opening_statistics[-52,]
Chess_opening_statistics <- Chess_opening_statistics[-52,]
row.names(Chess_opening_statistics) <- 1:60

for_white <- mutate(Chess_opening_statistics, "Points per 100 games" = (`White win (%)`)*1 + (`Draw (%)`)*0.5)
for_white <- for_white[order(-for_white$`Points per 100 games`),]
row.names(for_white) <- 1:60
for_white <- for_white[,c(1,5)]
top_for_white <- for_white[1:10,]

for_black <- mutate(Chess_opening_statistics, "Points per 100 games" = (`Black win (%)`)*1 + (`Draw (%)`)*0.5)
for_black <- for_black[order(-for_black$`Points per 100 games`),]
row.names(for_black) <- 1:60
for_black <- for_black[,c(1,5)]
top_for_black <- for_black[1:10,]

drawing_percentage <- Chess_opening_statistics[,c(1,4)]
drawing_percentage <- drawing_percentage[order(-drawing_percentage$`Draw (%)`),]
row.names(drawing_percentage) <- 1:60
most_draws <- drawing_percentage[1:10,]

url2 <- "https://en.wikipedia.org/wiki/List_of_chess_grandmasters_by_country"

link2 <- sprintf(url2)
stran2 <- html_session(link2) %>% read_html(encoding = "UTF-8")
tabela2 <- stran2 %>% html_nodes(xpath = "//table[2]") %>% html_table(fill=TRUE)
velemojstri <- data.frame(tabela2)
velemojstri <- velemojstri[-(1:67),]
velemojstri <- velemojstri[,1:3]
row.names(velemojstri) <- 1:66
names(velemojstri)[1] <- "Country"
names(velemojstri)[2] <- "Active GMs"
names(velemojstri)[3] <- "Overall GMs"
velemojstri <- velemojstri[-1,]
row.names(velemojstri) <- 1:65
velemojstri[,1] <- velemojstri[,1] %>% strapplyc("\\s*([[:alpha:]\\s&.,'-]+)") %>% unlist()
velemojstri[,1] <- gsub("(Â\\s)*","",velemojstri[,1])
velemojstri[,2] <- gsub("(\\[){1}(\\d){1,2}(\\]){1}", "", velemojstri[,2])
velemojstri[,2] <- as.numeric(velemojstri[,2])
velemojstri[,3] <- as.numeric(velemojstri[,3])
velemojstri$`Overall GMs` <- NULL
velemojstri[37,1] <- "Bosnia and Herzegovina"
topvelemojstri <- velemojstri[1:10,]

link3 <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"
stran3 <- html_session(link3) %>% read_html(encoding = "UTF-8")
prebivalstvo <- stran3 %>% html_nodes(xpath = "//div[@id='mw-content-text']/table") %>% .[[1]] %>% html_table(fill = TRUE)
prebivalstvo <- prebivalstvo[-1,]
names(prebivalstvo)[3] <- "Population" 
prebivalstvo[,2] <- gsub("^[^A-Z]+","",prebivalstvo[,2])
prebivalstvo[,2] <- gsub("(\\[){1}(\\w){1,2}(\\]){1}","",prebivalstvo[,2])
prebivalstvo <- prebivalstvo[grep("[0-9]+", prebivalstvo[,1]),]
prebivalstvo <- na.omit(prebivalstvo)
row.names(prebivalstvo) <- 1:196
prebivalstvo$Rank <- NULL
names(prebivalstvo)[1] <- "Country"
prebivalstvo[,2] <- gsub("(\\,)","",prebivalstvo[,2])
prebivalstvo[,2] <- as.numeric(prebivalstvo[,2])

link4 <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"
stran4 <- html_session(link4) %>% read_html(encoding = "UTF-8")
GDP_per_capita <- stran4 %>% html_nodes(xpath = "//div[@id='mw-content-text']/table") %>% .[[1]] %>% html_table(fill = TRUE)
GDP_per_capita <- GDP_per_capita[4:192,1:3]
GDP_per_capita <- GDP_per_capita[grep("[0-9]+", GDP_per_capita[,1]),]
GDP_per_capita$X1 <- NULL
GDP_per_capita[,1] <- gsub("(Â\\s)*","",GDP_per_capita[,1])
GDP_per_capita[,1] <- gsub("(\\[){1}(\\w){1}(\\]){1}(\\[){1}(\\w){1}(\\]){1}","",GDP_per_capita[,1])
row.names(GDP_per_capita) <- 1:187
GDP_per_capita[,2] <- gsub("(\\,)","",GDP_per_capita[,2])
GDP_per_capita[,2] <- as.numeric(GDP_per_capita[,2])
names(GDP_per_capita)[1] <- "Country"
names(GDP_per_capita)[2] <- "GDP pc"

link5 <- "https://www.cia.gov/library/publications/the-world-factbook/rankorder/2206rank.html"
stran5 <- html_session(link5) %>% read_html(encoding = "UTF-8")
tabela5 <- stran5 %>% html_nodes(xpath = "//table[1]") %>% html_table
education_exp <- data.frame(tabela5)
education_exp$Rank <- NULL
education_exp$Date.of.Information <- NULL
names(education_exp)[2] <- "% of GDP"

GMs_and_population <- merge(prebivalstvo,velemojstri)
GMs_and_population <- GMs_and_population[order(-GMs_and_population$`Active GMs`),]
row.names(GMs_and_population) <- 1:63

GMS_and_population_and_GDP <- merge(GMs_and_population,GDP_per_capita)
GMS_and_population_and_GDP <- GMS_and_population_and_GDP[order(-GMS_and_population_and_GDP$`Active GMs`),]
row.names(GMS_and_population_and_GDP) <- 1:61

GMs_per_capita <- mutate(GMs_and_population, pc = (`Active GMs`/Population)*1000000)
GMs_per_capita <- GMs_per_capita[order(-GMs_per_capita$pc),]
row.names(GMs_per_capita) <- 1:63
GMs_per_capita <- GMs_per_capita[,c(1,4)]
names(GMs_per_capita)[2] <- "GMs per million"
top_countries <- GMs_per_capita[1:10,]

GMs_per_capita_and_GDP <- merge(GMs_per_capita, GDP_per_capita)
GMs_per_capita_and_GDP <- GMs_per_capita_and_GDP[order(-GMs_per_capita_and_GDP$`GMs per million`),]
row.names(GMs_per_capita_and_GDP) <- 1:61

vse_skupaj1 <- merge(GMs_per_capita_and_GDP,education_exp)
vse_skupaj1 <- vse_skupaj1[order(-vse_skupaj1$`GMs per million`),]
row.names(vse_skupaj1) <- 1:55

vse_skupaj2 <- merge(GMS_and_population_and_GDP,education_exp)
vse_skupaj2 <- vse_skupaj2[order(-vse_skupaj2$`Active GMs`),]
row.names(vse_skupaj2) <- 1:55

graf_white <- ggplot(data=top_for_white, aes(x=`Opening name`,y=`Points per 100 games`)) + geom_bar(stat="identity",fill="white",colour="black") + coord_flip() + ggtitle("The best openings for white")
graf_black <- ggplot(data=top_for_black, aes(x=`Opening name`,y=`Points per 100 games`)) + geom_bar(stat="identity",fill="black") + coord_flip() + ggtitle("The best openings for black")
graf_draws <- ggplot(data=most_draws, aes(x=`Opening name`,y=`Draw (%)`)) + geom_bar(stat="identity",fill="blue") + coord_flip() + ggtitle("Most drawn openings")
graf_GMs <- ggplot(data=topvelemojstri, aes(x=Country,y=`Active GMs`)) + geom_bar(stat="identity",fill="darkgreen") + coord_flip() + ggtitle("Top GM countries")
graf_top_coutries <- ggplot(data=top_countries, aes(x=Country,y=`GMs per million`)) + geom_bar(stat="identity",fill="purple") + coord_flip() + ggtitle("Top GM per capita countries")


       
