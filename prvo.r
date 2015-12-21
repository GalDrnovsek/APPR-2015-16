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

tabela4 <- stran1 %>% html_nodes(xpath = "//table[2]") %>% html_table()
Best_white_openings <- data.frame(tabela4)

tabela5 <- stran1 %>% html_nodes(xpath = "//table[4]") %>% html_table()
Best_black_openings <- data.frame(tabela5)

tabela6 <- stran1 %>% html_nodes(xpath = "//table[6]") %>% html_table()
Most_drawn_openings <- data.frame(tabela6)

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
velemojstri[,1] <- gsub("(Â\\s)*","",velemojstri[,1])
velemojstri[,2] <- gsub("(\\[){1}(\\d){1,2}(\\]){1}", "", velemojstri[,2])

link3 <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"
stran3 <- html_session(link3) %>% read_html(encoding = "UTF-8")
prebivalstvo <- stran3 %>% html_nodes(xpath = "//div[@id='mw-content-text']/table") %>% .[[1]] %>% html_table(fill = TRUE)
prebivalstvo <- prebivalstvo[-1,]
names(prebivalstvo)[3] <- "Population" 
prebivalstvo[,2] <- gsub("(Â\\s)*","",prebivalstvo[,2])
prebivalstvo[,2] <- gsub("(\\[){1}(\\w){1,2}(\\]){1}","",prebivalstvo[,2])
prebivalstvo[,1] <- gsub("(\\â){1}(\\€){1}(\\”){1}",NA,prebivalstvo[,1])
prebivalstvo <- na.omit(prebivalstvo)
row.names(prebivalstvo) <- 1:196
prebivalstvo$Rank <- NULL

ggplot(data=Most_drawn_openings)
