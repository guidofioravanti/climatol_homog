#3 ottobre 2017
#Prepara i dati nel formato richiesto da climatol per l'omogeneizzazione delle serie
rm(list=objects())
library("purrr")
library("readr")
library("broom")
library("stringr")
library("imputeTS")
source("ClimateObjects.R")
source("ClimateData_24febbraio2017.R")
source("climatol_help.R")



# Funzioni utili per il programma -----------------------------------------

# lettura anagrafica
leggiAna<-function(nomeFile){
  
  
  #lettura del file con le stazioni da processare
  tryCatch({
    read_delim(file=nomeFile,delim=";",col_names=TRUE)
  },error=function(e){
    stop(sprintf("Errore lettura file anagrafica %s",nomeFile))
  })->ana
  
  #dobbiamo aggiustare i nomi nel file???
  if("County" %in% names(ana) && !("Regione" %in% names(ana))){
    names(ana)[grep("County",names(ana))]<-"Regione"
  }
  
  ana  
  
  
}#fine leggiAna


# Funzione riduci ---------------------------------------------------------
#applica reduce distinguendo tra dati giornalieri e mensili
riduci<-function(listaOut,monthlyJoin=TRUE){
  
  if(monthlyJoin){
    c("yy"="yy","mm"="mm")->colJoin
  }else{
    c("yy"="yy","mm"="mm","dd")->colJoin
  }
  
  listaOut %>% reduce(full_join,by=colJoin)  
  
}#fine riduci


# Funzione per riempire gli NA mensili: da evitare ------------------------

# funzione per riempireNA -------------------------------------------------
riempiNA<-function(x){
  
  # Identificazione anni in cui tutte le stazioni sono NA -------------------
  #somma va applicata solo alle colonne dei dati (siano essere serie mensili o giornaliere)
  apply(x[,3:ncol(x)],1,FUN =function(val){
    
    length(val)->numeroDati
    which(is.na(val))->qualiNA
    length(qualiNA)->numeroNA
    
    #cerchiamo solo le righe (anni) in cui almeno metà delle serie ha NA,sono gli anni che vanno 
    #interpolati perchè altrimenti climatol si blocca
    if(numeroNA>floor(numeroDati/2)) return(NA)
    
    sum(val,na.rm=TRUE)    
    
  })->vettoreSomma
  
  #indexNA identifica i mesi all'interno di un anno che sono NA in tutte le serie in esame. 
  which(is.na(vettoreSomma))->indexNA
  
  #per interpolare usiamo il pacchetto imputeTS: interpoliamo solo le righe (gli anni di ciascuna serie) "problematiche"
  #(ovvero quelle identificate dalla funzione somma)
  map_df(daScrivere,.f=function(serie){
    
    for(ii in indexNA){
      
      if(is.na(serie[ii])){
        
        na.interpolation(serie,option="linear")->serieInterpolata
        serie[ii]<-serieInterpolata[ii]
        
      }
      
      
    }#fine ciclo for
    
    serie
    
  })
  
} #fine riempiNA    


# INIZIO PROGRAMMA --------------------------------------------------------
REGIONE<-"Cluster1" #potrebbe anche essere regione Lazio...
FILE_ANAGRAFICA<-"cluster1_tmax_anagrafica.csv"

#directory in cui si trovano le varie subdirectory con le serie giornaliere da elaborare.
#Si tratta (ad esempio) delle directory in cui sono stati eseguiti i controlli spaziali. Ogni directory ha
#il nome di una regione italiana (compresa Aeronautica) e contiene i file.txt (ad esempio i file prodotti dai controlli spaziali)
DIRSERIE<-"spatial_controls"

#Quindi dentro DIRSERIE si trovano le varie subdirectory con le serie da elaborare.Perchè una struttura così complessa e non mettere i file tutti in una directory?
#Perchè le serie sono identificate da un numero id che non è unico "123456.txt". Se mettessimo tutto in una directory i file verrebbero sovrascritti e perderemmo
#l'informazione sulla fonte del dato (da che regione proviene?).

PARAM<-c("tmax","tmin")[1]
ANNOI<-1961
ANNOF<-2015

#Se vogliamo in output serie mensili:MONTHLY è TRUE, se vogliamo serie giornaliere allora MONTHLY è FALSE
MONTHLY<-TRUE

#Se vogliamo riempire gli NA mensili quando nessuna stazione ha valori disponibili dobbiamo porre: MONTHLY<-TRUE && REFILL_NA_MONTHLY<-TRUE
#In generale: evitare riempir perche si potrebbero riempire anni conuno stesso identico valore
REFILL_NA_MONTHLY<-FALSE

#Crea grafico serie mensili? Sia che si producano in output le serie giornaliere che quelle mensili, un grafico  delle serie
#mensili (o annuali) può aiutare a capire i risultati dell'omogeneizzazione
GRAFICO_SERIE_ANNUALI<-TRUE

#overloading della funzione
purrr::partial(...f=creaClimatolData,annoI=ANNOI,annoF=ANNOF)->creaClimatolData_ii_ff

#la colonna percorsoSerie contiene la posizione (directy) della serie in anagrafica
#Facciamo affidamento che le subdirectory in DIRSERIE abbiano un nome analogo a quello che compare in anagrafica (Regione)
leggiAna(nomeFile = FILE_ANAGRAFICA) %>% 
  mutate(percorsoSerie=paste0(DIRSERIE,"/",Regione,"/",SiteID,".txt"))->ana

#listaOut contiene serie mensili o serie giornaliere. Sono serie abbastanza continue e complete (vedere codice). In pratica abbiamo filtrato
#le serie, eliminando serie con troppi buchi
purrr::map(ana$percorsoSerie,.f=creaClimatolData_ii_ff,parametro=PARAM,returnMonthly=MONTHLY) %>% compact->listaOut

if(!length(listaOut)) stop("Nessuna serie idonea")
riduci(listaOut,monthlyJoin=MONTHLY)->daScrivere

# Solo se si vogliono serie mensili in output -----------------------------

#Climatol non accetta valori mensili che sono NA in tutte le serie in esame. Questi valori li dobbiamo in qualche modo riempire.
#Prima di tutto identifichiamo quei mesi all'interno di un anno in cui tutte le serie sono NA e poi li riempiamo. Questo problema non si pone 
#con le serie giornaliere. Per le serie giornaliere Climatol dispone di strumenti per il refill dei dati. Lo stesso dicasi quando utilizzando 
#le serie giornaliere e gli strumenti di Climatol si passa alle serie mensili. In sintesi: i mesi problematici vanno riempiti solo
#quando a Climatol si passano serie mensili calcolate senza utilizzare gli strumenti del pacchetto.
if(MONTHLY && REFILL_NA_MONTHLY) riempiNA(daScrivere)->daScrivere

# Scrittura file dei dati -------------------------------------------------
nome_dat<-paste0(Hmisc::capitalize(PARAM),"_",ANNOI,"-",ANNOF)
scriviFile_dat(x=daScrivere,nomeOut=nome_dat,file_monthly=MONTHLY)

#scrivi gli stessi dati in formato RDS, come un normale dataframe invece che nel formato richiesto da climatol
#Questi dati risulteranno utili nel momento in cui vorremo confrontare i dati omogeneizzati con quelli di partenza
saveRDS(daScrivere,file=paste0(nome_dat,"_raw.RDS"))

# Scrittura del file delle coordinate: ------------------------------------
nome_est<-nome_dat
scriviFile_est(x=daScrivere,ana=ana,nomeOut=nome_est,file_monthly=MONTHLY)



# Nel caso in cui si voglia il grafico delle serie ------------------------
nome_pdf<-nome_dat
if(GRAFICO_SERIE_ANNUALI) grafico(x=daScrivere,parametro=PARAM,nomeOut = paste0(nome_pdf,"_serieAnnuali"))   
