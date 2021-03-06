# Analiza šahovske statistike

Avtor: Gal Drnovšek

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

Analiziral bom statistiko šahovskih otvoritev-delež zmag igralcev z belimi figurami, črnimi figurami in remijev (za posamezne otvoritve). Nato bom pogledal še, katere države imajo največ velemojstrov na prebivalca. Cilj je izvedeti, katere šahovske otvoritve so statistično najboljše tako za igro z belimi kot za igro s črnimi figurami.
Podatke bom pridobil na: 

* http://www.becomeawordgameexpert.com/stats.htm 
* https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations) 
* https://en.wikipedia.org/wiki/List_of_chess_grandmasters_by_country
* https://www.cia.gov/library/publications/the-world-factbook/rankorder/2206rank.html

in morda še kje drugje.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
