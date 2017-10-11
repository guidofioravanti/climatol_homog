rm(list=objects())
library("climatol")
library("purrr")
library("dplyr")
library("tidyr")
#funzioni extra per climatol
source("climatol_help.R")


param<-"Tmax"
ANNOI<-1961
ANNOF<-2015
#correggi va a FALSE se vogliamo omogeneizzare le serie, va a true se una volta omogeneizzate le serie mensili vogliamo correggere le serie giornaliere
correggi<-TRUE
creaMensili<-FALSE


if(!correggi){
  if(creaMensili) dd2m(varcli =param ,anyi = ANNOI,anyf = ANNOF,valm = 2,namax = 10,na.strings = NA,homog = FALSE,ini="1961-01-01")
  homogen(varcli = paste0(param,"-m"),anyi =ANNOI,anyf = ANNOF,wd=c(200,200,100),snht1 = 25,snht2 = 25)
}else{
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
  assembla(param,ANNOI,ANNOF,mask=TRUE)
  
  
}
