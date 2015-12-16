require(dplyr)
require(rvest)
require(gsubfn)

url <- "http://czyzewski.org/chess/open-stats.html"

link <- sprintf(url)
stran <- html_session(link) %>% read_html(encoding = "UTF-8")
tabela <- stran %>% html_nodes(xpath ="//table[@height=3334]") %>% html_table()
prva_tabela <- data.frame(tabela)
prva_tabela$X5 <- NULL
names(prva_tabela)[1] <- "Opening name"
names(prva_tabela)[2] <- "White win (%)"
names(prva_tabela)[3] <- "Black win (%)"
names(prva_tabela)[4] <- "Draw (%)"  
prva_tabela <- prva_tabela[-1,]
row.names(prva_tabela) <- 1:137

url1 <- "http://www.becomeawordgameexpert.com/stats.htm"

link1 <- sprintf(url1)
stran1 <- html_session(link1) %>% read_html(encoding = "UTF-8")
tabela1 <- stran1 %>% html_nodes(xpath = "//table[1]") %>% html_table()
druga_tabela <- data.frame(tabela1)
druga_tabela$X5 <- NULL
names(druga_tabela)[1] <- "Opening name"
names(druga_tabela)[2] <- "White win (%)"
names(druga_tabela)[3] <- "Black win (%)"
names(druga_tabela)[4] <- "Draw (%)"
druga_tabela <- druga_tabela[-1,]
row.names(druga_tabela) <- 1:290

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
