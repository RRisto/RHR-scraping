data=read.csv("rhravaandmed.csv", sep=";")
data_min=data[grepl("ministeerium$", data$Hankija),]

#töötav variant
library(rvest)
abimuutuja1=c()
abimuutuja2=c()
eeldatav_maksumus=c()
lepingu_maksumus=c()
tegelik_maksumus=c()
kestvus=c()
kestvus2=c()
kestvus3=c()
kestvus4=c()
kestvus5=c()
kestvus6=c()
kestvus7=c()
kestvus8=c()
kestvus9=c()
kestvus10=c()
kestvus11=c()
kestvus12=c()
kestvus13=c()
kestvus14=c()
kestvus15=c()
kestvus16=c()
kestvus17=c()
kestvus_paevades=c()
periood=c()
cpv=c()
for (i in 1000:nrow(data_min)) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  if (data_min$Elektroonilisele.teabele.URL[i]!=""|!is.na(data_min$Elektroonilisele.teabele.URL[i])) {
    url=paste0("https://riigihanked.riik.ee/register/hange/",
               data_min$Viitenumber[i])
    #html_source =url
    page = read_html(url)
    #eeldatav maksumus
    eeldatav_maksumus[i]=page %>% 
      html_nodes("tr:nth-child(9) > td:nth-child(2)") %>%
      html_text()
    #lepingu maksumus sõlmimisel
    if(length(page %>% 
             html_nodes("tr:nth-child(10) > td:nth-child(2)") %>%
             html_text())==0) {
      lepingu_maksumus[i]=NA
    } else {
      lepingu_maksumus[i]=page %>% 
        html_nodes("tr:nth-child(10) > td:nth-child(2)") %>%
        html_text()
    }
      
    #tegelik maksumus
    if (length(page %>% 
               html_nodes("tr:nth-child(11) > td:nth-child(2)") %>%
               html_text())==0) {
      tegelik_maksumus[i]=NA
    } else {
      tegelik_maksumus[i]=page %>% 
        html_nodes("tr:nth-child(11) > td:nth-child(2)") %>%
        html_text()
    }
    #peruioodina esitatud lepingu pikkua
    if (length(page %>% 
               html_nodes("#If_128") %>%
               html_text())==0) {
      periood[i]=NA
    } else {
      periood[i]=page %>% 
        html_nodes("#If_128") %>%
        html_text()
    }
     #kestvus, mis on esitatud kuudes (ka mõni päevane on sisse eksinud)
    if (length(page %>% 
               html_nodes("#If_126") %>%
               html_text())==0) {
      kestvus[i]=NA
    } else {
      kestvus[i]=page %>% 
        html_nodes("#If_126") %>%
        html_text()
    } 
    #kestvus päevades
    if (length(page %>% 
               html_nodes("#If_124") %>%
               html_text())==0) {
      kestvus_paevades[i]=NA
    } else {
      kestvus_paevades[i]=page %>% 
        html_nodes("#If_124") %>%
        html_text()
    } 
    #kestvus kuudes, kuid teise selectoriga
    if (length(page %>% 
               html_nodes("#If_174") %>%
               html_text())==0) {
      kestvus2[i]=NA
    } else {
      kestvus2[i]=page %>% 
        html_nodes("#If_174") %>%
        html_text()
    } 
    #kestvus kuudes, kuid veelgi teise selectoriga
    if (length(page %>% 
               html_nodes("#If_140") %>%
               html_text())==0) {
      kestvus3[i]=NA
    } else {
      kestvus3[i]=page %>% 
        html_nodes("#If_140") %>%
        html_text()
    } 
    #kestvus kuudes, kuid jällegi teise selectoriga
    if (length(page %>% 
               html_nodes("#If_132") %>%
               html_text())==0) {
      kestvus4[i]=NA
    } else {
      kestvus4[i]=page %>% 
        html_nodes("#If_132") %>%
        html_text()
    } 
    #kestvus perioodina, kuid jällegi teise selectoriga, vbla on ka kuudes
    if (length(page %>% 
               html_nodes("#If_130") %>%
               html_text())==0) {
      kestvus5[i]=NA
    } else {
      kestvus5[i]=page %>% 
        html_nodes("#If_130") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_90") %>%
               html_text())==0) {
      kestvus6[i]=NA
    } else {
      kestvus6[i]=page %>% 
        html_nodes("#If_90") %>%
        html_text()
    } 
    #kestvus perioodina, kuid jällegi teise selectoriga, vbla on ka kuuna
    if (length(page %>% 
               html_nodes("#If_134") %>%
               html_text())==0) {
      kestvus7[i]=NA
    } else {
      kestvus7[i]=page %>% 
        html_nodes("#If_134") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_138") %>%
               html_text())==0) {
      kestvus8[i]=NA
    } else {
      kestvus8[i]=page %>% 
        html_nodes("#If_138") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_210") %>%
               html_text())==0) {
      kestvus9[i]=NA
    } else {
      kestvus9[i]=page %>% 
        html_nodes("#If_210") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_144") %>%
               html_text())==0) {
      kestvus10[i]=NA
    } else {
      kestvus10[i]=page %>% 
        html_nodes("#If_144") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_150") %>%
               html_text())==0) {
      kestvus11[i]=NA
    } else {
      kestvus11[i]=page %>% 
        html_nodes("#If_150") %>%
        html_text()
    }
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_146") %>%
               html_text())==0) {
      kestvus12[i]=NA
    } else {
      kestvus12[i]=page %>% 
        html_nodes("#If_146") %>%
        html_text()
    } 
    #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_164") %>%
               html_text())==0) {
      kestvus13[i]=NA
    } else {
      kestvus13[i]=page %>% 
        html_nodes("#If_164") %>%
        html_text()
    } #kestvus kuudena, kuid jällegi teise selectoriga, vbla on ka perioodina
    if (length(page %>% 
               html_nodes("#If_152") %>%
               html_text())==0) {
      kestvus14[i]=NA
    } else {
      kestvus14[i]=page %>% 
        html_nodes("#If_152") %>%
        html_text()
    } 
    if (length(page %>% 
               html_nodes("#If_136") %>%
               html_text())==0) {
      kestvus15[i]=NA
    } else {
      kestvus15[i]=page %>% 
        html_nodes("#If_136") %>%
        html_text()
    } 
    if (length(page %>% 
               html_nodes("#If_186") %>%
               html_text())==0) {
      kestvus16[i]=NA
    } else {
      kestvus16[i]=page %>% 
        html_nodes("#If_186") %>%
        html_text()
    } 
    #CPV kood
    cpv[i]=page %>% 
      html_nodes("#Any_17") %>%
      html_text()
    
    print(i)
    
  } else {
    eeldatav_maksumus[i]=NA
    lepingu_maksumus[i]=NA
    tegelik_maksumus[i]=NA
    periood[i]=NA
    kestvus[i]=NA
    cpv[i]=NA
    kestvus2[i]=NA
    kestvus3[i]=NA
    kestvus4[i]=NA
    kestvus5[i]=NA
    kestvus6[i]=NA
    kestvus7[i]=NA
    kestvus8[i]=NA
    kestvus9[i]=NA
    kestvus10[i]=NA
    kestvus11[i]=NA
    kestvus12[i]=NA
    kestvus13[i]=NA
    kestvus14[i]=NA
    kestvus15[i]=NA
    kestvus16[i]=NA
    kestvus_paevades[i]=NA
    
    print(i)
  }
  #abimuutujad kala tabamiseks
  abimuutuja1[i]=length(cpv)
  abimuutuja2[i]=length(kestvus10)
}

#funktsioon puhastamiseks
puhastaja=function(sisend) {
  valjund=gsub("\t|\n|\r|Ā| ", "", sisend)
  valjund
}

cpv_puhas=puhastaja(cpv)
eeldatav_maksumus_puhas=puhastaja(eeldatav_maksumus)
lepingu_maksumus_puhas=puhastaja(lepingu_maksumus)
tegelik_maksumus_puhas=puhastaja(tegelik_maksumus)
periood_puhas=puhastaja(periood)
kestvus_puhas=puhastaja(kestvus)
kestvus2_puhas=puhastaja(kestvus2)
kestvus3_puhas=puhastaja(kestvus3)
kestvus4_puhas=puhastaja(kestvus4)
kestvus5_puhas=puhastaja(kestvus5)
kestvus6_puhas=puhastaja(kestvus6)
kestvus7_puhas=puhastaja(kestvus7)
kestvus8_puhas=puhastaja(kestvus8)
kestvus9_puhas=puhastaja(kestvus9)
kestvus10_puhas=puhastaja(kestvus10)
kestvus11_puhas=puhastaja(kestvus11)
kestvus12_puhas=puhastaja(kestvus12)
kestvus13_puhas=puhastaja(kestvus13)
kestvus14_puhas=puhastaja(kestvus14)
kestvus15_puhas=puhastaja(kestvus15)
kestvus16_puhas=puhastaja(kestvus16)
kestvus_paevades_puhas=puhastaja(kestvus_paevades)
#osaliselt puhas stuff kokku
andmed_puhtad=data.frame(cpv_puhas, eeldatav_maksumus_puhas, 
                         lepingu_maksumus_puhas, tegelik_maksumus_puhas, 
                         periood_puhas, kestvus_puhas,kestvus2_puhas,kestvus3_puhas,
                         kestvus4_puhas, kestvus_paevades_puhas, kestvus5_puhas,kestvus6_puhas,
                         kestvus7_puhas, kestvus8_puhas,kestvus9_puhas, kestvus10_puhas,
                         kestvus11_puhas,kestvus12_puhas, kestvus13_puhas,kestvus14_puhas,
                         kestvus15_puhas, kestvus16_puhas,stringsAsFactors = F)

#puhastame perioodi ära, seal kus on toodu kuudena kestvus
kuudes=ifelse(grepl("kuud|months|days", andmed_puhtad$periood_puhas), 
                                    andmed_puhtad$periood_puhas, NA)
kuudes2=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus2_puhas), 
               andmed_puhtad$kestvus2_puhas, NA)
kuudes3=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus3_puhas), 
               andmed_puhtad$kestvus3_puhas, NA)
kuudes4=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus4_puhas), 
               andmed_puhtad$kestvus4_puhas, NA)
kuudes5=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus5_puhas), 
               andmed_puhtad$kestvus5_puhas, NA)
kuudes6=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus6_puhas), 
               andmed_puhtad$kestvus6_puhas, NA)
kuudes7=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus7_puhas), 
               andmed_puhtad$kestvus7_puhas, NA)
kuudes8=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus8_puhas), 
               andmed_puhtad$kestvus8_puhas, NA)
kuudes9=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus9_puhas), 
               andmed_puhtad$kestvus9_puhas, NA)
kuudes10=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus10_puhas), 
               andmed_puhtad$kestvus10_puhas, NA)
kuudes11=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus11_puhas), 
                andmed_puhtad$kestvus11_puhas, NA)
kuudes12=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus12_puhas), 
                andmed_puhtad$kestvus12_puhas, NA)
kuudes13=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus13_puhas), 
                andmed_puhtad$kestvus13_puhas, NA)
kuudes14=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus14_puhas), 
                andmed_puhtad$kestvus14_puhas, NA)
kuudes15=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus15_puhas), 
                andmed_puhtad$kestvus15_puhas, NA)
kuudes16=ifelse(grepl("kuud|months|days", andmed_puhtad$kestvus16_puhas), 
                andmed_puhtad$kestvus16_puhas, NA)


#paneme kokku
for (i in 1:nrow(andmed_puhtad)) {
  if (!is.na(kuudes[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes[i]
  } else if (!is.na(kuudes2[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes2[i]
  } else if (!is.na(kuudes3[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes3[i]
  } else if (!is.na(kuudes4[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes4[i]
  }else if (!is.na(kuudes5[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes5[i]
  }else if (!is.na(kuudes6[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes6[i]
  }else if (!is.na(kuudes7[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes7[i]
  }else if (!is.na(kuudes8[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes8[i]
  }else if (!is.na(kuudes9[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes9[i]
  }else if (!is.na(kuudes10[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes10[i]
  }else if (!is.na(kuudes11[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes11[i]
  }else if (!is.na(kuudes12[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes12[i]
  }else if (!is.na(kuudes13[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes13[i]
  }else if (!is.na(kuudes14[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes14[i]
  }else if (!is.na(kuudes15[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes15[i]
  }else if (!is.na(kuudes16[i])) {
    andmed_puhtad$kestvus_puhas2[i]=kuudes16[i]
  }else {
    andmed_puhtad$kestvus_puhas2[i]=NA
    #next
  }
}

for (i in 1:nrow(andmed_puhtad)) {
  if (grepl("kuud|months|days", andmed_puhtad$kestvus_puhas[i])) {
    andmed_puhtad$kestvus_puhas2[i]=andmed_puhtad$kestvus_puhas[i]
  } else {
    next
  }
}

#teeme aja päevade osas kuudeks
paev_temp=ifelse(grepl("days|päeva", andmed_puhtad$kestvus_puhas2),
                 andmed_puhtad$kestvus_puhas2, NA )
paev_temp=as.numeric(gsub("[^[:digit:]]", "", paev_temp))/30
#seal, kus oli ainult päevades teeme ka puhtaks
paev_temp2=ifelse(grepl("days",andmed_puhtad$kestvus_paevades_puhas),
                  andmed_puhtad$kestvus_paevades_puhas, NA)
paev_temp2=as.numeric(gsub("[^[:digit:]]", "", paev_temp2))/30
#mergime kokku
for(i in 1:length(paev_temp)) {
  if (!is.na(paev_temp2[i])) {
    paev_temp[i]=paev_temp2[i]
  }
}
paev_temp=as.character(paev_temp)
#paev_temp=c(paev_temp, paev_temp2)
#paneme päevadena esitatu kuude juurde
for (i in 1:nrow(andmed_puhtad)) {
  if (!is.na(paev_temp[i])) {
    andmed_puhtad$kestvus_puhas2[i]=paev_temp[i]
  } else {
    next
  }
}
#puhastame teksti kuudest välja
andmed_puhtad$kestvus_puhas2=gsub("[A-z]", "", 
                                  andmed_puhtad$kestvus_puhas2)

######töötleme neid, mis esitati kuupäevade vahemikuna
andmed_puhtad$periood_puhas2=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                                          andmed_puhtad$periood_puhas), 
                                    andmed_puhtad$periood_puhas, NA)
#ka selles võib olla periood
periood2=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus2_puhas), andmed_puhtad$kestvus2_puhas, NA)
periood3=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus3_puhas), andmed_puhtad$kestvus3_puhas, NA)
periood4=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus4_puhas), andmed_puhtad$kestvus4_puhas, NA)
periood5=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus5_puhas), andmed_puhtad$kestvus5_puhas, NA)
periood6=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus6_puhas), andmed_puhtad$kestvus6_puhas, NA)
periood7=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus7_puhas), andmed_puhtad$kestvus7_puhas, NA)
periood8=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus8_puhas), andmed_puhtad$kestvus8_puhas, NA)
periood9=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                      andmed_puhtad$kestvus9_puhas), andmed_puhtad$kestvus9_puhas, NA)
periood10=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus10_puhas), andmed_puhtad$kestvus10_puhas, NA)
periood11=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus11_puhas), andmed_puhtad$kestvus11_puhas, NA)
periood12=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus12_puhas), andmed_puhtad$kestvus12_puhas, NA)
periood13=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus13_puhas), andmed_puhtad$kestvus13_puhas, NA)
periood14=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus14_puhas), andmed_puhtad$kestvus14_puhas, NA)
periood15=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus15_puhas), andmed_puhtad$kestvus15_puhas, NA)
periood16=ifelse(grepl("[0-9]{2,}.[0-9]{2,}.[0-9]{4,}-[0-9]{2,}.[0-9]{2,}.[0-9]{4,}", 
                       andmed_puhtad$kestvus16_puhas), andmed_puhtad$kestvus16_puhas, NA)

#loobime juurde perioodid eri veergudest
for (i in 1:nrow(andmed_puhtad)) {
  if (!is.na(periood2[i])) {
    andmed_puhtad$periood_puhas2[i]=periood2[i]
  } else if (!is.na(periood3[i])) {
    andmed_puhtad$periood_puhas2[i]=periood3[i]
  }else if (!is.na(periood4[i])) {
    andmed_puhtad$periood_puhas2[i]=periood4[i]
  }else if (!is.na(periood5[i])) {
    andmed_puhtad$periood_puhas2[i]=periood5[i]
  }else if (!is.na(periood6[i])) {
    andmed_puhtad$periood_puhas2[i]=periood6[i]
  }else if (!is.na(periood7[i])) {
    andmed_puhtad$periood_puhas2[i]=periood7[i]
  }else if (!is.na(periood8[i])) {
    andmed_puhtad$periood_puhas2[i]=periood8[i]
  }else if (!is.na(periood9[i])) {
    andmed_puhtad$periood_puhas2[i]=periood9[i]
  }else if (!is.na(periood10[i])) {
    andmed_puhtad$periood_puhas2[i]=periood10[i]
  }else if (!is.na(periood11[i])) {
    andmed_puhtad$periood_puhas2[i]=periood11[i]
  }else if (!is.na(periood12[i])) {
    andmed_puhtad$periood_puhas2[i]=periood12[i]
  }else if (!is.na(periood13[i])) {
    andmed_puhtad$periood_puhas2[i]=periood13[i]
  }else if (!is.na(periood14[i])) {
    andmed_puhtad$periood_puhas2[i]=periood14[i]
  }else if (!is.na(periood15[i])) {
    andmed_puhtad$periood_puhas2[i]=periood15[i]
  }else if (!is.na(periood16[i])) {
    andmed_puhtad$periood_puhas2[i]=periood16[i]
  }
}

andmed_puhtad$periood_puhas2=gsub("[A-z]", "", andmed_puhtad$periood_puhas2)
perioodialgus=strsplit(andmed_puhtad$periood_puhas2, split="-")
ajadf=do.call(rbind.data.frame, perioodialgus)
andmed_puhtad$perioodialgus=ajadf[,1]
andmed_puhtad$perioodilopp=ajadf[,2]
andmed_puhtad$perioodialgus=gsub("[.]", "-", andmed_puhtad$perioodialgus)
andmed_puhtad$perioodilopp=gsub("[.]", "-", andmed_puhtad$perioodilopp)

#teeme ajavahemiku kuudeks
library(lubridate)
aeg=difftime(dmy(andmed_puhtad$perioodilopp),
             dmy(andmed_puhtad$perioodialgus),units="days")/30
#paneme ühte veergu juurde
for (i in 1:nrow(andmed_puhtad)) {
  if (!is.na(aeg[i])) {
    andmed_puhtad$kestvus_puhas2[i]=round(aeg[i])
  } else {
    next
  }
}
##eemaldame veerud, mida pole vaja
andmed_puhtad$perioodilopp=NULL
andmed_puhtad$perioodialgus=NULL
andmed_puhtad$periood_puhas2=NULL
andmed_puhtad$kestvus_puhas=NULL
andmed_puhtad$kestvus2_puhas=NULL
andmed_puhtad$kestvus3_puhas=NULL
andmed_puhtad$kestvus4_puhas=NULL
andmed_puhtad$kestvus5_puhas=NULL
andmed_puhtad$kestvus6_puhas=NULL
andmed_puhtad$kestvus7_puhas=NULL
andmed_puhtad$kestvus8_puhas=NULL
andmed_puhtad$kestvus9_puhas=NULL
andmed_puhtad$kestvus10_puhas=NULL
andmed_puhtad$kestvus11_puhas=NULL
andmed_puhtad$kestvus12_puhas=NULL
andmed_puhtad$kestvus13_puhas=NULL
andmed_puhtad$kestvus14_puhas=NULL
andmed_puhtad$kestvus15_puhas=NULL
andmed_puhtad$kestvus16_puhas=NULL
andmed_puhtad$periood_puhas=NULL
andmed_puhtad$kestvus_paevades_puhas=NULL
#ümbernimetamine
names(andmed_puhtad)[ncol(andmed_puhtad)]="kestvus_kuudes"

#puhastame maksumuse veerud
#eemaldab tähed
tahedValja=function(data) {
  data=as.data.frame(lapply(data,function(x) as.character(x)))
  gsub("[A-z]|-|", "", data)
}
#
#proov=as.data.frame(lapply(andmed_puhtad[,2:4], function(x) gsub("[A-z]","", x)))
andmed_puhtad2=as.data.frame(lapply(andmed_puhtad, as.character))
andmed_puhtad2[]=as.data.frame(lapply(andmed_puhtad2, function(x) gsub("[A-z]|-","", x)), 
                               stringsAsFactors = F)
#teeme characterist numercikuks, ja komad asendame punktiga
andmed_puhtad2[,2:4]=as.data.frame(lapply(andmed_puhtad2[,2:4], function(x) 
  gsub(",",".", x)), stringsAsFactors = F)
andmed_puhtad2[,2:5]=lapply(andmed_puhtad2[,2:5], as.numeric, as.character)
#tühjad teeme NAks
andmed_loplik=as.data.frame(lapply(andmed_puhtad2, function(x) ifelse(x=="", NA, x)))
#CPV asendame, kuna seal on ka "-" vaja
andmed_loplik$cpv_puhas=andmed_puhtad$cpv_puhas
andmed_loplik$kestvus_kuudes=round(andmed_loplik$kestvus_kuudes)

write.table(andmed_loplik, "andmed_loplik2.csv", sep=";", row.names = F)
