rm(list=objects())
library("climatol")
library("stringr")
library("purrr")
library("dplyr")
library("tidyr")
#funzioni extra per climatol
source("climatol_help.R")
source("ClimateData.R")
source("ClimateObjects.R")
source("wmo.R")


param<-"Tmax"
ANNOI<-1961
ANNOF<-2015

#parametri per homogen
WD<-c(200,200,100)
SNHT1<-25
SNHT2<-25
SWA<-60

#Se vogliamo creare un file di breakpoints senza i breakpoints troppo vicini ad ANNOI e ANNOF allora mettere ELIMINA_BRK_ESTREMI_SERIE==TRUE
#In realtà questo filtro può essere evitato se si confrontano i breakpoints di Climatol con i breakpoints di ACMANT3. Il programma che mette insieme
#i breakpoints dei due programmi include un filtro sui breakpoints troppo vicini agli estremi della serie. Inoltre il filtro qui implementato 
#funziona solo per i breakpoints vicini ad ANNOI o ANNOF. Ma se una serie inizia con tutti NA e poi inizia (ad esempio) nel 1967, il filtro qui implementato
#non eliminerebbe breakpoints troppo vicini al 1967 (mentre il filtro nel programma che mette insieme i risultati di Climatol e ACMANT3 considera l'inizio effettivo della serie,
#non quello nominale)
ELIMINA_BRK_ESTREMI_SERIE<-FALSE

#correggi va a FALSE se vogliamo omogeneizzare le serie, va a true se una volta omogeneizzate le serie mensili vogliamo correggere le serie giornaliere
correggi<-FALSE
creaMensili<-TRUE

paste0(param,"_",ANNOI,"-",ANNOF)->stringaBase

if(!correggi){
  
  if(creaMensili) dd2m(varcli =param ,anyi = ANNOI,anyf = ANNOF,valm = 2,namax = 10,na.strings = NA,homog = FALSE,ini=paste0(ANNOI,"-01-01"))
  
  sink("parametri_homogen.txt",append=FALSE)
    cat(("###################################\n"))
    cat(("###################################\n"))  
    cat(("Eseguo homogen con i parametri:\n"))
    cat((sprintf("Parametro wd: %d, %d, %d\n",WD[1],WD[2],WD[3])))
    cat((sprintf("Parametro snht1: %d\n",SNHT1)))
    cat((sprintf("Parametro snht2: %d\n",SNHT2)))  
    cat((sprintf("Parametro swa: %d\n",SWA))) 
    cat(("###################################\n"))
    cat(("###################################\n"))
  sink()

  homogen(varcli = paste0(param,"-m"),anyi =ANNOI,anyf = ANNOF,wd=WD,snht1 = SNHT1,snht2 = SNHT2,swa=SWA)
  
  #vogliamo creare un file dei breakpoint analogo a quello creato dal programma ma senza i breakpoint agli estremi della serie
  #Infatti tenere i breakpoint troppo vicini all'inizio o alla fine del periodo in esame genera in alcuni casi correzioni eccessive
  #(non realistiche) delle serie
  if(ELIMINA_BRK_ESTREMI_SERIE){
    paste0(str_replace(stringaBase,paste0("^",param), paste0(param,"-m")),"_brk.csv")->nomeBrk
    readr::read_delim(nomeBrk,delim=",",col_names=TRUE)->breakPoints
    
    breakPoints %>% dplyr::filter(Date >= as.Date(paste0(ANNOI+5,"-01-01")) & Date<= as.Date(paste0(ANNOF-5,"-12-31")) )->newBrk
    #nuovo file con breakpoints filtrati. Toccherà all'utente nei passi successivi decidere quale file di breakpoints utilizzare
    if(nrow(newBrk)) readr::write_delim(newBrk,path =str_replace(nomeBrk,"\\.csv","_filtrati.csv"),delim=",",col_names=TRUE)
  }

}else{
  
  #qui salviamo i parametri utilizzati per homogen a livello mensile

    cat(print("###################################\n"))
    cat(print("###################################\n"))  
    cat(print("Eseguo homogen per correggere le serie giornaliere")) 
    cat(print("###################################\n"))
    cat(print("###################################\n"))    

  
  #correction of daily series
  homogen(varcli = param,anyi =ANNOI,anyf = ANNOF,metad = TRUE)
  #dopo la correzione crea i file di testo con le serie che funzionano nell'ultimo periodo, utilizza la funzione mydahstat
  #che risolve possibili caratteri non validi nei file di testo
  mydahstat(param,ANNOI,ANNOF,stat="series",last=TRUE)
  #assembla le serie omogeneizzate in un unico dataframe. Se si vuole
  #il file assemblato è mascherato rispetto al flag 1. I dati prodotti dall'omogeneizzazione hanno:
  #-flag 0 (dato originale),
  #-flag 1 per gli NA riempiti 
  #-flag 2 per i dati corretti
  assemblaHomog(param,ANNOI,ANNOF,mask=TRUE)->dfHomo
  #dfHomo è un data.frame che contiene le serie omogeneizzate (solo le serie che arrivano alla fine del periodo di analisi)
  
  fileSerieOriginali<-paste0(stringaBase,"_raw.RDS")
  nome_pdf<-paste0(stringaBase,"_serieAnnuali_homo.pdf")
  
  #grafico delle serie omogeneizzate. Se esiste il file RDS con le serie originali "raw" allora facciamo il grafico della serie
  #originale (in blu) e della corrispondente serie omogeneizzata (in nero)
  if(file.exists(fileSerieOriginali)){
    
    readRDS(fileSerieOriginali)->dfRaw
    #param va passato in lower case per farlo riconoscere a ClimateData
    grafico(x=dfHomo,parametro=tolower(param),nomeOut = nome_pdf,y=dfRaw)
    
  }else{

    #serie originali non disponibili, facciamo grafico delle sole serie omogeneizzate
    grafico(x=dfHomo,parametro=tolower(param),nomeOut = nome_pdf)    
    
  }
    
  
}#fine if
