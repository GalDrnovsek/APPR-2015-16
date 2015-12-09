require(dplyr)
require(rvest)
require(gsubfn)

url <- "http://czyzewski.org/chess/open-stats.html"

podatki <- data.frame()
link <- sprintf(url)
stran <- html_session(link) %>% read_html(encoding = "UTF-8")
tabela <- stran %>% html_nodes(xpath ="//table[@height=3334]") %>% html_table()
data.frame(tabela) -> prva_tabela