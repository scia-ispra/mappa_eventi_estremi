rm(list=objects())
library("tidyverse")

ANNO<-2020
INCLUDI_NOAA<-FALSE

nomeFile<-"2020_Eventi_estremi_Italia_per_rapporto_ISPRA_NOAA_07_07_21.csv"

read_delim(nomeFile,delim=";",col_names = TRUE,locale=locale(decimal_mark = "."),col_types = cols(Lon=col_character(),Lat=col_character())) %>%
  mutate(Lon=round(as.double(Lon),2),
         Lat=round(as.double(Lat),2),
         Icona=str_trim(tolower(Icona),side = "both"))->eventi

### check Lon e Lat
which(is.na(eventi$Lat))->righe
if(length(righe)) message("NA nel campo latitudine!")

which(is.na(eventi$Lon))->righe
if(length(righe)) message("NA nel campo longitudine!")


#I dati NOAA non hanno il campo Stagione
STAGIONE_NOAA<-"Annuale"
eventi[is.na(eventi$Stagione),]$Stagione<-STAGIONE_NOAA

#controllo sul campo stagione
which(is.na(eventi$Stagione==STAGIONE_NOAA) & (eventi$Fonte=="ISPRA"))->righe
if(length(righe)) message("Controllare che tutti i dati ISPRA abbiano il campo stagione non NA!")

#
stopifnot(unique(eventi$Fonte) %in% c("ISPRA","NOAA"))
stopifnot(unique(eventi$Stagione) %in% c("Primavera","Estate","Autunno e dicembre","Inverno",STAGIONE_NOAA))

eventi %>%
  #mutate(Sottotitolo=iconv(Sottotitolo,from="ISO-8859-15",to="UTF-8")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"l[:punct:]","l&#39;")) %>%    
  mutate(Sottotitolo=str_replace(Sottotitolo,"\\?","&#45;")) %>%  
  mutate(Sottotitolo=str_replace(Sottotitolo,"\\.","&#45;")) %>%   
  mutate(Sottotitolo=str_replace(Sottotitolo,"\\(","")) %>%  
  mutate(Sottotitolo=str_replace(Sottotitolo,"\\)","")) %>%    
  mutate(Sottotitolo=str_replace(Sottotitolo," +-","&#45;")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"- +","&#45;")) %>%  
  mutate(Sottotitolo=str_replace(Sottotitolo,"-"," &#45; ")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"([0-9]+) *&#45; *([0-9]+)","\\1 &#45; \\2")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"([eo])&#45;","\\1 &#45;")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"0([1-9])","\\1")) %>%  
  mutate(Sottotitolo=str_replace(Sottotitolo,"gennaio","Gennaio")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"febbraio","Febbraio")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"marzo","Marzo")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"aprile","Aprile")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"maggio","Maggio")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"giugno","Giugno")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"luglio","Luglio")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"agosto","Agosto")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"settembre","Settembre")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"ottobre","Ottobre")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"novembre","Novembre")) %>%
  mutate(Sottotitolo=str_replace(Sottotitolo,"dicembre","Dicembre"))->eventi

eventi %>%
  mutate(Sottotitolo=ifelse(is.na(Sottotitolo),glue::glue("Sintesi {tolower(STAGIONE_NOAA)}, fonte NOAA"),Sottotitolo))->eventi

eventi %>%
  #mutate(Testo=iconv(Testo,from="ISO-8859-15",to="UTF-8")) %>%
  mutate(Testo=str_replace_all(Testo,"'","&#39;")) %>%
  mutate(testo=glue::glue("<h3 style='font-weight: 500;'>{Titolo}</h3><h4>{Sottotitolo}</h4><span style='line-height: 1.5em;'>{Testo}</span>")) %>%
  mutate(testo=str_replace_all(testo,"([lL])\\?","\\1&#39;")) %>%
  mutate(testo=str_replace_all(testo,"([dD])\\?","\\1&#39;")) %>%
  mutate(testo=str_replace_all(testo,"\\?C","&#176;C")) %>%
  mutate(testo=str_replace_all(testo,"([[:digit:]])\\?","\\1&#176;")) %>%
  mutate(testo=str_replace_all(testo,"\\.? *\\.?<\\/span> *$","")) %>%
  mutate(testo=str_replace_all(testo,"$",".</span>")) %>%
  mutate(testo=str_replace_all(testo,"a\\?","a&#39;")) %>%
  mutate(testo=str_replace_all(testo,"un\\?","un&#39;")) %>%
  mutate(testo=str_replace_all(testo,"pi\\?","piu&#39;")) %>%
  mutate(testo=str_replace_all(testo,"piu\\?","piu&#39;")) %>%
  mutate(testo=str_replace_all(testo," \\? "," &#45; ")) %>%
  mutate(testo=str_replace_all(testo," ([0-9]+),([0-9]+) "," \\1.\\2 ")) %>%
  mutate(testo=str_replace_all(testo,"mslm","m slm")) %>%
  mutate(testo=str_replace_all(testo,"slm","s.l.m.")) %>%
  mutate(testo=str_replace_all(testo,"foehn","<a href='https://it.wikipedia.org/wiki/Favonio' title='favonio' target='_blank'>foehn</a>")) %>%
  mutate(testo=str_replace_all(testo,"([Tt]empo di ritorno)","<a href='https://it.wikipedia.org/wiki/Tempo_di_ritorno' title='tempo di ritorno' target='_blank'>\\1</a>")) %>%
  mutate(testo=str_replace_all(testo,"(Goni)","<a href='https://it.wikinew.wiki/wiki/Typhoon_Goni_(2020)' title='tifone Goni' target='_blank'>\\1</a>"))->eventi

### Paletta dei colori
print(unique(eventi$Icona))
stopifnot(eventi$Icona %in% c("termorosso", "vento", "secco", "neve", "termoblu", "pioggia", "sabbia", "alluvione", "pioggia","mareggiata"))


#ColorBrewer diverging palette``
eventi %>%
  mutate(colori=case_when(Icona=="termorosso" ~ "#e31a1c",
                          Icona=="sabbia" ~ "#b15928",
                          Icona=="termoblu" ~ "#1f78b4",
                          Icona=="pioggia" ~ "#b2df8a",
                          Icona=="neve" ~ "#a6cee3",
                          Icona=="secco" ~ "#ffff99",
                          Icona=="vento" ~ "#cab2d6",
                          Icona=="mareggiata" ~ "#8d97cb",
                          Icona=="alluvione" ~ "#33a02c",
                          TRUE ~ "green"))->eventi

if(!INCLUDI_NOAA) eventi %>% filter(Fonte=="ISPRA")->eventi

write_delim(eventi,file=glue::glue("{ANNO}_eventi_estremi.csv"),delim=";",col_names=TRUE)