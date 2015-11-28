require(dplyr)
require(rvest)
require(gsubfn)

url <- "http://czyzewski.org/chess/open-stats.html"

podatki <- data.frame()
link <- sprintf(url)
stran <- html_session(link) %>% read_html(encoding = "UTF-8")