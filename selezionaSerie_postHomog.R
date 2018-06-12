#28 Marzo 2018: questo programma va utilizzato dopo aver proceduto con l'omogeneizzazione delle serie.
#
#I file di input possono essere:
# - le serie prodotte da Climatol (quindi, le serie giornaliere omogeneizzate e ricostruite e i corrispondenti file con i flag)
# - i dati giornalieri originali salvati nei file .RDS al momenti in cui si è iniziata l'omogeneizzazione delle serie con Climatol (passando dal dato giornaliero a quello mensile)

#
# 1) se vengono trovati i file con le serie originali (raw ovvero pre-omogeneizzate) queste serie vengono elaborate
# 2) se vengono trovati i file flg prodotti da climatol, i corrispendenti file dati (serie omogeneizzate) vengono elaborate

#In ogni caso il programma richiede la presenza dei file con le anagrafiche delle stazioni per clusters. "cluster1_tmax_anagrafica.csv" etc etc
#Questi file infatti servono per poter contare il numero di stazioni che nei vari clusters, anno per anno, contribuiscono al calcolo delle anomalie
rm(list=objects())
library("tidyverse")
library("stringr")
library("sp")
library("rgdal")
library("purrrlyr")
library("geosphere")
library("zyp")
source("ClimateData.R")
options(error=recover,warn = 2)

# from Daily to Annual ----------------------------------------------------
fromDailyToAnnual<-function(x,param){
  
  names(x)[4]->nomeStazione
  
  ClimateData(x,param = param)->serieDaily
  if(!nrow(serieDaily)) return(NULL)
  
  #aggrega a livello mensile
  aggregaCD(serieDaily,max.na = 5,rle.check = TRUE,max.size.block.na = 3)->serieMonthly
  
  #aggrega a livello annuale
  aggregaCD(serieMonthly,ignore.par=FALSE,max.na = 3,rle.check = TRUE,max.size.block.na = 3,seasonal=TRUE)->serieAnnual  
  
  #la serie è abbastanza completa e continua??
  checkSeriesValidity2(x = serieAnnual,max.size.block.na = 4,percentualeAnniPresenti = 90,minLen = 40)->ris  
  if(is.null(ris)) {return(NULL)}
  
  #verifica che gli ultimi 3 anni non siano tutti NA
  c(coredata(ris))->serie
  if(all(is.na(serie[(length(serie)-2):(length(serie))]))) {return(NULL)}

  names(ris)<-nomeStazione

  list(annual=as.data.frame(ris),monthly=as.data.frame(serieMonthly))
  
}#fine fromDailyToAnnual
#  ------------------------------------------------------------------------

################################
############### Inizio programma
################################

PARAM<-"Tmin"
QUANTI_CLUSTERS<-10
REFILL<-FALSE #se vogliamo le serie con i dati ricostruiti da Climatol, mettere REFILL<-TRUE
#ANNOI e ANNOF vengono usati solo per scrivere i nomi dei file di output. I file di partenza debbono già essere limitati al range ANNOI - ANNOF
ANNOI<-1961
ANNOF<-2015


try(file.remove("LISTA_FILE_DATI.log"))

#file anagrafica clusters: cluster1_parametro---
list.files(pattern=paste0("anagrafica_cluster_[0-9]{1,2}_",tolower(PARAM),".csv$"))->fileClusters
stopifnot(length(fileClusters)==QUANTI_CLUSTERS)

#codice è il codice utilizzato per l'omogeneizzazione: e.g., Abruzzo_1771
#ana è l'anagrafica delle stazioni che contiene la suddivisione per clusters. Se la clusterizzazione aveva definito 4 clusters, l'appartenenza ai vari clusters
#per ciascuna stazione si trova sotto "cluster"
purrr::map(fileClusters,.f=function(nomeFile){

  stringr::str_split(nomeFile,"_")[[1]][[3]]->codiceCluster

  read_delim(nomeFile,delim=";",col_names = TRUE) %>% 
    mutate(cluster=codiceCluster) %>%
      mutate(codice=paste0(regione,"_",SiteId))
  
}) %>% reduce(rbind) ->ana


#questi sono i file prodotti da Climatol e contengono i flag: 0, 1 e 2 (0 dato originale, 1 dato riempito, 2 dato corretto con omogeneizzazione)
list.files(pattern="^.+flg.csv$")->ffile

#corrispondenti codici stazioni usati per omogeneizzazione (ovvero == codice in ana). Se questi file non esistono significa che nella directory non ci sono
#i risultati dell'omogeneizzazione e stiamo lavorando solo sulle serie "raw". 
if(length(ffile)) codiciStazioni<-str_replace(str_extract(ffile,"_[A-Za-z]+_[0-9]+"),"_","") 

#codiciStazioni sono i codici delle stazioni che abbiamo messo da parte (ad esempio le 68 stazioni utilizzate a suo tempo per la selezione delle serie per il Rapporto Indicatori)
#questi codiciStazioni sono identificati dai nomi dei file di output di Climatol. Se questi file non ci sono e quindi codiciStazioni non esiste, tutte le serie originali
#(ovvero pre-omogeneizzazione) verranno considerate (ma comunque filtrate in base a quanto riportato in fromDailyToAnnual)

#selezioniamo le serie originali, pre-omogeneizzazione. Usiamo i codici stazione in "ffile"
#Le serie originali stanno nei file RDS. Se i file RDS non ci sono, questa parte viene saltata.
list.files(pattern=paste0("^.+_raw.RDS"))->fileRDS

if(length(fileRDS)){
  
      
      clusters<-str_extract(fileRDS,"cluster[0-9]{1,}")
      
      purrr::map(fileRDS,.f=function(nomeFile){
        
        readRDS(nomeFile)->dati
        
        tryCatch({
          dati[,names(dati) %in% c("yy","mm","dd",codiciStazioni)] 
          #caso in cui esiste codiciStazioni: stiamo selezionando solo le serie originali con codice in codiciStazioni. Ovvero: stiamo selezionando le serie originali che hanno un file "flg" corrispondente
        },error=function(e){
          dati 
          #caso in cui stiamo lavorando in una directory con solo i file .RDS delle serie originali (pre omogeneizzazione) e mancano i file di output di Climatol, ovvero non abbiamo serie omogeneizzate come riferimento
          #in questo caso prendiamo tutte le serie originali
        })
        
      }) %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"))->serieRaw

      write_delim(serieRaw,paste0(PARAM,"_serieGiornaliereRaw.csv"),delim=";",col_names = TRUE)      
      #aggrega le serie originali originali e crea le serie annuali
      #reduce(listaDatiOriginali,left_join)->serieRaw
      
      purrr::imap(serieRaw %>% select(-yy,-mm,-dd),.f=function(.x,.y){
        
        serieRaw %>% select(yy,mm,dd) %>% mutate(value=.x)->tmp
        names(tmp)[4]<-.y
        
        fromDailyToAnnual(x=as_tibble(tmp),param=tolower(PARAM)) #step per passare dalle serie giornaliere a quelle annuali
      
      }) %>% compact->listaOut
      
      purrr::map(listaOut,"annual") %>% reduce(left_join)->serieAnnualiRaw
      purrr::map(listaOut,"monthly") %>% reduce(left_join)->serieMensiliRaw      
      #scrittura del file con le serie annuali "raw"..pre-homog
      write_delim(serieAnnualiRaw,paste0(PARAM,"_serieAnnualiRaw.csv"),delim=";",col_names = TRUE)
      write_delim(serieMensiliRaw,paste0(PARAM,"_serieMensiliRaw.csv"),delim=";",col_names = TRUE)
      
}#fine if sul fileRDS      
      
#questa parte riguarda la selezione ed elaborazione delle serie omogeneizzate: partiamo dalle serie giornaliere e creiamo le serie annuali
if(length(codiciStazioni)){

    #selezioniamo solo le serie che abbiamo omogeneizzato
    purrr::map(ffile,.f=function(ff){
      
      #file Climatol con flag
      read_delim(ff,delim=",",col_names=TRUE) %>% rename(flag=Value)->flag

      str_replace(str_replace(str_extract(ff,"_[A-Za-z]+_[0-9]+-"),"_",""),"-","")->nomeStazione
      if(is.na(nomeStazione) || nchar(nomeStazione)==0) stop("Problema con nomeStazione")
      
      #i file dati corrispondenti ai file flag (-flg) possono avere estensione: codice-2.csv oppure codice.csv
      #Il pattern qui sotto serve a descrivere entrambi i casi. I file "-2.csv" sono (dovrebbero essere) le serie omogeneizzate 
      #I file senza "-2" dovrebbero essere le serie che non sono state omogeneizzate (quindi il corrispoindente file flag non dovrebbe avere mai flag 2... ma non è così :( )
      list.files(pattern=paste0("^.+_",nomeStazione,"(-2)?\\.csv"))->fileDati 
      if(length(fileDati)>1) stop("Impossibile")
      #se non trovo il file con suffisso "-2" povo a cercare il file senza suffisso
      SCRIVI_LOG<-FALSE
      
      if(length(fileDati)!=1) browser() #fermati e vedi che succede
      
      if(1==0){
            
            if(length(fileDati)!=1) {SCRIVI_LOG<-TRUE; list.files(pattern=paste0("^.+_",nomeStazione,".csv"))->fileDati} #SCRIVO IL NOME FILE SOLO SE SI TRATTA DI UN FILE SENZA SUFFISSO "-2"
            if(length(fileDati)!=1) {stop(sprintf("Non trovato file dati corrispondente al file flg per %s!!!",nomeStazione))}
            
            #teniamo traccia del nome del file di dati...questo per verificare se un file dati che non ha estensione "-2" corrisponde a una serie che ne Climatol ne ACMANT3 hanno elaborato
            #(ovvero serie che magari non avevano limitrofe) o sono serie per cui ACMANT e Climatol hanno trovato breakpoints ma che poi nono sono state omogeneizzate (questo può accadere se stiamo 
            #considerando le serie omogeneizzate con i breakpoints comuni tra ACMANT e Climatol..i due metodi trovano breakpoints per una serie ma non corrispondenti). In pratica: questa informazione ci permette
            #di capire se nel calcolo delle serie stiamo utilizzando serie non omogeneizzate perchè "omogenee" (ovvero perchè non abbiamo trovato breakpoints comuni tra CAMANT e Climatol) o serie che non è stato possibile
            #omogeneizzare. Ovviamente se prendiamo in esame i file omogeneizzati usando tutti i breakpoint di Climatol la distinzione tra serieomogeneizzate e non dovrebbe essere più chiara (tutti i file dati hanno suffisso -2 ???)
            if(SCRIVI_LOG){
              sink("LISTA_FILE_DATI.log",append=TRUE)
              cat(paste0(fileDati,"\n"))
              sink()
            }
      }#fine 1==0
      
      #lettura del file dati
      read_delim(fileDati,delim=",",col_names = TRUE)->dati
    
      #unisco dati e flag
      left_join(dati,flag,by=c("Date"="Date"))->ldati
      rm(dati)
      rm(flag)
      
      #manteniamo anche i dati ricostruiti come output
      ldati %>%
        tidyr::separate(col=Date,into=c("yy","mm","dd"),sep="-") %>%
        select(-flag)->tmp_ricostruiti
      names(tmp_ricostruiti)[4]<-nomeStazione  
      
      #se ci sono flag==1 (dati ricostruiti da Climatol) e REFILL==FALSE, invalido i dati ricostruiti
      which(ldati$flag==1)->indice
      if(length(indice) && !REFILL) ldati[indice,]$Value<-NA
      
      #contiamo quanti valori sono stati omogeneizzati (codice 2), quanti sono NA (flag 1) e quanti sono originali
      #numeroAnni contiene una stima approssimativa del numero di dati (espressi in anni) "originali" (flag 0) e dati homg (flag2)
      ldati %>% group_by(flag) %>% summarise(numeroAnni=n()/365) %>% mutate(codice=nomeStazione)->conteggio
      
      ldati %>%
        tidyr::separate(col=Date,into=c("yy","mm","dd"),sep="-") %>%
        select(-flag)->tmp
    
      names(tmp)[4]<-nomeStazione  
      fromDailyToAnnual(tmp,param = tolower(PARAM))->annuale

      if(is.null(annuale)) return(NULL)
  
      list("annual"=annuale[["annual"]],"monthly"=annuale[["monthly"]],"daily"=tmp,"daily_ricostruiti"=tmp_ricostruiti,"conteggio"=conteggio)
      
    }) %>% compact->listaOut
  
    purrr::map(listaOut,"annual") %>% reduce(left_join,by=c("yy"="yy"))->serieAnnualiHomog
    purrr::map(listaOut,"monthly") %>% reduce(left_join,by=c("yy"="yy","mm"="mm"))->serieMensiliHomog
    purrr::map(listaOut,"daily") %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"))->serieGiornaliereHomog  
    purrr::map(listaOut,"daily_ricostruiti") %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"))->serieGiornaliereHomogRico    
    purrr::map(listaOut,"conteggio") %>% reduce(rbind)->conteggioFlag    
  
    write_delim(serieAnnualiHomog,paste0(PARAM,"_serieAnnualiHomog.csv"),delim=";",col_names = TRUE)
    write_delim(serieMensiliHomog,paste0(PARAM,"_serieMensiliHomog.csv"),delim=";",col_names = TRUE)    
    write_delim(serieGiornaliereHomog,paste0(PARAM,"_serieGiornaliereHomog.csv"),delim=";",col_names = TRUE)        
    write_delim(serieGiornaliereHomogRico,paste0(PARAM,"_serieGiornaliereHomog_ricostruiti.csv"),delim=";",col_names = TRUE)        
    write_delim(conteggioFlag,paste0(PARAM,"_conteggioFlag.csv"),delim=";",col_names = TRUE)  

}#fine if su codiciStazioni

