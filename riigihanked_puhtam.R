data=read.csv("rhravaandmed.csv", sep=";")
data_min=data[grepl("ministeerium$", data$Hankija),]

#abimuuujad
library(rvest)
eeldatav_maksumus=c()
lepingu_maksumus=c()
tegelik_maksumus=c()
periood=c()
cpv=c()
#perioodi selectorid, mis loobib läbi
periood_selectors=c("#If_90", "#If_124","#If_126", "#If_128","#If_130", "#If_132",
                    "#If_134","#If_136","#If_138","#If_140","#If_144","#If_146",
                    "#If_150","#If_152","#If_164","#If_174","#If_186","#If_210")
#funktsioon lihtsaks kraapimiseks selectori lõikes
kraapija=function(selector, page=leht) {
  if(length(page %>% 
            html_nodes(selector) %>%
            html_text())==0) {
    return(NA)
  } else {
    page %>% 
      html_nodes(selector) %>%
      html_text()
  }
}
#puhastaja, eemaldab esmase sodi, täiesti puhtaks ei tee
puhastaja=function(sisend) {
  valjund=gsub("\t|\n|\r|Ā| ", "", sisend)
  valjund
}
#loobime kõik lingid läbi ja kirjutame andmed muutujatesse
for (i in 1:50) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
    url=paste0("https://riigihanked.riik.ee/register/hange/",
               data_min$Viitenumber[i])
    leht = read_html(url)
    #eeldatav maksumus
    eeldatav_maksumus[i]=puhastaja(kraapija(
      selector="tr:nth-child(9) > td:nth-child(2)"))
    #lepingu maksumus sõlmimisel
    lepingu_maksumus[i]=puhastaja(kraapija(
      selector="tr:nth-child(10) > td:nth-child(2)"))
    #tegelik maksumus
    tegelik_maksumus[i]=puhastaja(kraapija(
      selector="tr:nth-child(11) > td:nth-child(2)"))
    #cpv
    cpv[i]=puhastaja(kraapija(selector="#Any_17"))
    
    #perioodi selectorid loobime läbi
    for (j in 1:length(periood_selectors)) {
      selector=periood_selectors[j]
      tekst=leht %>% 
        html_nodes(selector) %>%
        html_text()
      tekst=puhastaja(tekst)
      if (length(tekst)!=0) {
        if (grepl("kuud|months|days", tekst)|
            grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                  tekst)) {
          periood[i]=puhastaja(tekst)
        }
      }
    }
    print(i)
}

#esmane puhas
esmane_sodine=data.frame(cpv, eeldatav_maksumus,lepingu_maksumus, 
                           tegelik_maksumus,periood)
