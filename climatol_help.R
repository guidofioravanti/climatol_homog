library("dplyr")
library("stringr")
library("purrr")
library("readr")
#se vogliamo fare l'omogeneizzazione delle serie mensili abbiamo bisogno di serie mensili e quindi returnMonthly=TRUE
#Se invece vogliamo che creaClimatolData scriva in  output le serie giornaliere (perchè vogliamo fare l'omogeneizzazione direttamente sulle serie mensili
#oppure perche vogliamo utilizzare le funzioni del pacchetto climatol per passare dalle serie gionaliere a quelle mensili) returnMonthly=FALSE
creaClimatolData<-function(nomeFile,parametro,annoI,annoF,returnMonthly=TRUE)
{
  
  stopifnot(parametro %in% c("prcp","tmax","tmin","prpc"))
  
  #formato colonne
  cols(year=col_integer(),month=col_integer(),day=col_integer(),prcp=col_number(),tmax=col_number(),tmin=col_number())->tipiColonne
  
  tryCatch({
    read_delim(nomeFile,delim=",",col_names = TRUE,col_types=tipiColonne)->temp
    stopifnot(ncol(temp)!=1)
    temp
  },error=function(e){
    read_delim(nomeFile,delim=";",col_names=TRUE,col_types=tipiColonne)->temp
    stopifnot(ncol(temp)!=1)
    temp
  })->dati
  
  #year, month and day columns + colonna parametro
  c(grep("^y.+",names(dati)),grep("^m.+",names(dati)),grep("^d.+",names(dati)),grep(parametro,names(dati)))->colonne

  stopifnot(length(colonne)==4)
  names(dati)[colonne]<-c("yy","mm","dd",parametro)

  #crea ClimateObject
  ClimateData(dati[,colonne] %>%  filter(yy>=annoI & yy<=annoF),param=parametro)->serieDaily
  if(!nrow(serieDaily)) return(NULL)
  
  #aggrega a livello mensile
  aggregaCD(serieDaily,max.na = 10,rle.check = TRUE,max.size.block.na = 3)->serieMonthly

  #aggrega a òivello annuale
  aggregaCD(serieMonthly,ignore.par=FALSE,max.na = 10,rle.check = TRUE,max.size.block.na = 10,seasonal=TRUE)->serieAnnual  
  
  #la serie è abbastanza completa e continua??
  checkSeriesValidity2(x = serieAnnual,max.size.block.na = 10,percentualeAnniPresenti = 80,minLen = 10)->ris
  
  if(is.null(ris)) return(NULL)
  
  year(index(ris))->anni
  
  #stringa per oggetto xts
  range(anni)->anniIF

  #controllo che siano due numeri finiti (no NaN, NULL, NA)
  stopifnot(all(anniIF>1800))

  if(returnMonthly){
    as.data.frame(serieMonthly[paste(anniIF,collapse ="/")])->df
    cc<-3
  }else{
    as.data.frame(serieDaily[paste(anniIF,collapse ="/")])->df    
    cc<-4
  }

  #dobbiamo rinominare la colonna dei dati. Questa colonna può essere la terza (serie mensili) o la quarta (serie giornaliere)
  str_split(nomeFile,"/")->splitted
  length(splitted[[1]])->len
  str_replace(paste(splitted[[1]][(len-1):len],collapse ="_"),"\\.txt$","")->nomeSerie
  names(df)[cc]<-nomeSerie

  df
  
}#fine creaClimatolData




# Scrittura file serie mensili/giornaliere, file .dat ---------------------
#prependAnno va TRUE se si vogliono scrivere i dati per climatol
scriviFile_dat<-function(x,nomeOut,file_monthly=TRUE,acmant=FALSE){
  
  unique(x$yy)->anni
  
  ifelse(file_monthly,3,4)->primaColonna
  
  ifelse(acmant,".txt",".dat")->estensione

  sink(paste0(nomeOut,estensione),append=FALSE)

  tryCatch({
      
      #header per acmant
      if(acmant) cat(paste(names(x)[3],"\n"))

      purrr::walk(as_tibble(x[,primaColonna:ncol(x)]),.f=function(serie){
      
        if(acmant) serie[is.na(serie)]<- -999.9

        for(yy in anni){
      
          paste(paste(as.character(round(serie[x$yy==yy],1)),collapse = " "),"\n")->stringa
          
          #formato climatol senza anno iniziale
          if(!acmant){
           cat(stringa)
          }else{
            #formato per ACMANT3, stesso di climatol ma con anno iniziale
            cat(paste(yy,stringa,sep=" "))
          }#su prependAnno
          
        }#fine ciclo for
        
      })
      
      sink()
  },error=function(e){
      sink()
  }) #fine tryCatch
      
}#fine scriviFile_dat

############################################################################
#variazione della funzione sopra per scrittura dei file acmant in formato giornaliero
# Scrittura file serie mensili/giornaliere, file .dat ---------------------
#prependAnno va TRUE se si vogliono scrivere i dati per climatol
############################################################################
scriviFile_dat_acmant_daily<-function(x,nomeOut){
  
  unique(x$yy)->anni
  
  primaColonna<-4
  
  "d.txt"->estensione
  
  sink(paste0(nomeOut,estensione),append=FALSE)

  tryCatch({
    
    #header per acmant
    cat(paste(names(x)[4],"\n"))
    
    serie<-x[[4]]
    serie[is.na(serie)]<- -999.9
    mesi<-stringr::str_pad(seq(1,12),pad="0",side="left",width=2)  
    
      for(yy in anni){
        
        for(mm in mesi){
        
          paste(paste(as.character(round(serie[x$yy==yy & x$mm==mm ],1)),collapse = " "),"\n")->stringa
          #formato per ACMANT3, stesso di climatol ma con anno iniziale
          cat(paste(yy,as.integer(mm),stringa,sep=" "))

        }#ciclo for in mese    
        
      }#fine ciclo for
      
    sink()
  },error=function(e){
    sink()
  }) #fine tryCatch
  
}#fine scriviFile_dat_acmant_daily



############################################################################
# Scrivi file coordinate, file .est ---------------------------------------
############################################################################
scriviFile_est<-function(x,ana,nomeOut,file_monthly=TRUE){
  
  ifelse(file_monthly,3,4)->primaColonna  
  
  sink(paste0(nomeOut,".est"),append=FALSE)
  
  purrr::walk(names(x)[primaColonna:ncol(x)],.f=function(codice){
    
    str_split(codice,"_")[[1]]->splitted
    
    #devo avere solo regione e codice stazione, quindi due elementi
    stopifnot(length(splitted)==2)

    
    which(ana$SiteId==as.integer(splitted[2]) & ana$regione==splitted[1])->riga
    #nel data.frame dell'anagrafica devo avere solo una riga corrispondente a Regione e codice
    stopifnot(length(riga)==1)
    #if(length(riga)!=1) browser()
    
    ana[riga,]$Longitude->lon   
    ana[riga,]$Latitude->lat    
    ana[riga,]$Elevation->quote    
    
    paste0("\"",ana[riga,]$SiteName,"\"")->name    
    #ana[riga,]$SiteID->id   
    
    #cat(paste(paste(c(lon,lat,quote,name,paste0(REGIONE,".",codice)),collapse = " "),"\n"))
    cat(paste(paste(c(lon,lat,quote,codice,name),collapse = " "),"\n"))
  })
  
  sink()
  
} #fine scriviFile_est 



# programma per il grafico delle serie annuali e trend --------------------
# Se insieme alle serie annuali originali si vuole fare il grafico delle serie annuali post omogeneizzazione
# passare il parametro y.
# x e y debbono avere le stesse serie (stessi nomi) e gli stessi anni

disegna<-function(fun,serie,anni,nomeSerie,xlab,ylab){

   if(fun=="points"){
    points(x=anni,y=serie,type="l",lty=2,col="blue")    
    list(linea=0.5,nomeSerie=paste0(nomeSerie,"_homog"),at=anni[1],col="blue")->lista
  }else{
    plot(x=anni,y=serie,type="l",xlab="",ylab=ylab)
    list(linea=2,nomeSerie=paste0(nomeSerie,""),at=anni[1],col="red")->lista  
  }
  
  lm(serie~anni)->lmOut
  abline(a=coefficients(lmOut)[1],b=coefficients(lmOut)[2],col=lista$col,lty=2)
  
  #otteniamo i p-values
  broom::tidy(lmOut)->tabella
  tabella[tabella$term=="anni",]$p.value->myp
  mtext(text = paste0(lista$nomeSerie," - pvalue: ",round(myp,2)),side = 3,line = lista$linea,at =lista$at,adj=0,family="Lato Black",cex = 0.8)

}#  fine disegna
  
grafico<-function(x,parametro,nomeOut,...){
  
  ClimateData(x,param=parametro)->serieDaily
  
  if(!nrow(serieDaily)) return(NULL)
  
  range(x$yy)->anniIF
  
  #aggrega a livello mensile
  aggregaCD(serieDaily,max.na = 10,rle.check = TRUE,max.size.block.na = 3)->serieMonthly
  
  #aggrega a livello annuale
  aggregaCD(serieMonthly,ignore.par=FALSE,max.na = 3,rle.check = TRUE,max.size.block.na = 3,seasonal=TRUE)->serieAnnual  
  
  if((length(list(...))!=0) && grepl("y",names(list(...)))){
    
    list(...)$y->y
    ClimateData(y,param=parametro)->yserieDaily
    aggregaCD(yserieDaily,max.na = 10,rle.check = TRUE,max.size.block.na = 3)->yserieMonthly
    aggregaCD(yserieMonthly,ignore.par=FALSE,max.na = 3,rle.check = TRUE,max.size.block.na = 3,seasonal=TRUE)->yserieAnnual  
    
  }#fine if grepl
  
  #gli anni della serie
  year(index(serieAnnual))->anni
  
  #etichette
  ifelse(grepl("^tm..$",parametro),"°C","mm")->ylab
  
  cairo_pdf(filename = paste0(nomeOut,".pdf"),width=12,height=8,onefile = TRUE)
  
  #clipping della retta
  par(xpd=F)

  tryCatch({
    
    purrr::walk(2:ncol(serieAnnual),.f=function(colonna){
      
      names(serieAnnual)[colonna]->nomeSerie
      
      coredata(serieAnnual[,colonna])->serie
      disegna(fun="plot",anni=anni,serie=serie,nomeSerie=nomeSerie,xlab="",ylab=ylab)        

      if(exists("yserieAnnual")){

        grep(paste0("^",nomeSerie,"$"),names(yserieAnnual))->ycolonna
        
        if(length(ycolonna)){
          coredata(yserieAnnual[,ycolonna])->seriey
          disegna(fun="points",anni=anni,serie=seriey,nomeSerie=nomeSerie)        
        }else{
          warning(sprintf("Serie %s non trovata in dati raw",nomeSerie))
        }#if su length(ycolonna)
        
      } 
      
    }) #fine walk
    
    dev.off()
    
  },error=function(e){
    print("Errore grafico")
    dev.off()
  })
  
}#fine funzione grafico



# funzione mydahstat.R ----------------------------------------------------

#funzione analoga a dahstat del pacchetto climatol ma corregge i nomi dei file di output che generano
#un errore (converte gli slash / in -)
mydahstat<-function (varcli, anyi, anyf, anyip = anyi, anyfp = anyf, stat = "me", 
                     ndc = 1, vala = 2, cod = NULL, mnpd = 0, mxsh = 0, prob = 0.5, 
                     last = FALSE, long = FALSE, mh = FALSE, pernys = 100, ini = NA, 
                     estcol = 4, sep = ",", dec = ".", eol = "\n") 
{
  if (anyi > anyf) 
    stop("First year of data greater than the last year!")
  if (anyip < anyi) 
    stop("Asked initial year before first year of data!")
  if (anyfp > anyf) 
    stop("Asked final year beyond last year of data!")
  fun <- c("mean", "median", "max", "min", "sd", "quantile")[which(c("me", 
                                                                     "mdn", "max", "min", "std", "q", "tnd") == stat)]
  if (!length(fun) & stat != "series") 
    stop(sprintf("Option stat='%s' not recognized!", stat))
  estvar <- c("X", "Y", "Z", "Code", "Name", "pod", "ios", 
              "ope", "snht")
  mes3 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
            "Aug", "Sep", "Oct", "Nov", "Dec")
  if (!mh) 
    load(sprintf("%s_%d-%d.rda", varcli, anyi, anyf))
  else {
    dah <- scan(sprintf("%s-mh_%d-%d.dat", varcli, anyi, 
                        anyf))
    est.c <- read.table(sprintf("%s-mh_%d-%d.est", varcli, 
                                anyi, anyf), colClasses = c("numeric", "numeric", 
                                                            "numeric", "character", "character", "numeric", "numeric", 
                                                            "numeric", "numeric"))
    ne <- nrow(est.c)
    nd <- length(dah)/ne
    est.b <- read.table(sprintf("%s-m_%d-%d.est", varcli, 
                                anyi, anyf), colClasses = c("numeric", "numeric", 
                                                            "numeric", "character", "character"))
    nei <- nrow(est.b)
    nm <- 12
    na <- anyf - anyi + 1
    if (nd != na * nm) 
      stop(sprintf("Number of data (%d) is not number of months (%d) times number of years (%d)", 
                   nd, nm, na))
    dim(dah) <- c(nm, na, ne)
    if (stat == "series") {
      dat <- scan(sprintf("%s-m_%d-%d.dat", varcli, anyi, 
                          anyf))
      dim(dat) <- c(nm, na, nei)
    }
    else rm(est.b)
  }
  ndec <- ndc
  if (nm > 0) 
    na <- anyf - anyi + 1
  else na <- nd
  if (nm < 2) {
    dim(dah) <- c(1, na, ne)
    if (stat == "series") 
      dim(dat) <- c(1, na, nei)
  }
  if (nm < 2 | stat == "series") 
    vala <- 0
  else {
    if (vala < 0 | vala > 4) 
      vala <- 2
    funa <- c("sum", "mean", "max", "min")[vala]
  }
  if (!is.null(cod)) {
    ksel <- which(est.c[, 4] %in% cod)
    ksel <- which(est.c[, 7] %in% ksel)
    dah <- dah[, , ksel]
    if (nm < 2) 
      dim(dah) <- c(1, na, length(ksel))
    est.c <- est.c[ksel, ]
  }
  esel <- est.c[, 6] >= mnpd
  if (mxsh > 0) 
    esel <- esel & est.c[, 9] <= mxsh
  if (last) 
    esel <- esel & as.logical(est.c[, 8])
  else if (long) {
    lsel <- rep(TRUE, length(esel))
    for (ko in 1:nei) {
      kest <- which(est.c[, 7] == ko)
      if (length(kest) > 1) {
        kmax <- which.max(est.c[kest, 6])
        lsel[kest[-kmax]] <- FALSE
      }
    }
    esel <- esel & lsel
  }
  ne <- sum(esel)
  if (ne == 0) 
    stop("No station selected: No output")
  dah <- dah[, , esel]
  dim(dah) <- c(max(c(1, nm)), na, sum(esel))
  est.c <- est.c[esel, ]
  iest <- est.c[, 7]
  if (vala) {
    aval <- as.vector(apply(dah, 2:3, funa))
    dim(dah) <- c(nm, na * ne)
    dah <- rbind(dah, aval)
    nm <- nm + 1
    dim(dah) <- c(nm, na, ne)
  }
  if (stat != "series") 
    val <- matrix(NA, ne, max(1, nm))
  if (nm > 0) {
    x <- anyip:anyfp
    xk <- x - anyi + 1
    pernum <- pernys
  }
  else {
    xk <- 1:nd
    if (is.na(ini)) 
      ini <- sprintf("%d-01-01", anyi)
    x <- seq(as.Date(ini), length.out = nd, by = "1 day")
    pernum <- pernys * 365.25
  }
  if (stat == "tnd") {
    pval = val
    for (i in 1:ne) {
      if (nm < 2) {
        aj <- lm(dah[1, xk, i] ~ x)
        val[i, ] <- round(aj$coefficients[2] * pernum, 
                          ndec)
        pval[i, ] <- round(summary(aj)$coefficients[2, 
                                                    4], 3)
      }
      else {
        for (j in 1:nm) {
          aj <- lm(dah[j, xk, i] ~ x)
          val[i, j] <- round(aj$coefficients[2] * pernum, 
                             ndec)
          pval[i, j] <- round(summary(aj)$coefficients[2, 
                                                       4], 3)
        }
      }
    }
  }
  else if (stat == "series") {
    for (kest in 1:ne) {
      dh <- dah[, xk, kest]
      ik <- iest[kest]
      if (mh) 
        ik <- which(est.b[, 4] == est.c[ik, 4])
      do <- dat[, xk, ik]
      df <- abs(dh - do) < 1e-09
      df <- as.numeric(df)
      df[df == 0] <- 2
      df[df == 1] <- 0
      df[is.na(df)] <- 1
      dim(df) <- dim(dh)
      ard <- sprintf("%s_%d-%d_%s.csv", varcli, anyip, 
                     anyfp, est.c[kest, 4])
      arf <- sprintf("%s_%d-%d_%s-flg.csv", varcli, anyip, 
                     anyfp, est.c[kest, 4])
      if (nm > 1) {
        dh <- cbind(x, t(dh))
        df <- cbind(x, t(df))
      }
      else {
        dh <- cbind(format(x), dh)
        df <- cbind(format(x), df)
      }
      if (nm == 12) 
        colnames(dh) <- c("Year", mes3)
      else if (nm > 1) 
        colnames(dh) <- c("Year", as.character(1:nm))
      else colnames(dh) <- c("Date", "Value")
      colnames(df) <- colnames(dh)
      if (stat == "series") {
        stringr::str_replace(ard,"/","-")->ard
        stringr::str_replace(arf,"/","-")->arf              
        write.csv(dh, ard, row.names = FALSE, quote = FALSE)
        write.csv(df, arf, row.names = FALSE, quote = FALSE)
      }
    }
    cat(sprintf("Homogenized values written to %s_%d-%d_*.csv,\nwith flags in %s_%d-%d_*-flg.csv:\n", 
                varcli, anyip, anyfp, varcli, anyip, anyfp))
    cat("  0: Observed data\n")
    cat("  1: Missing data (filled)\n")
    cat("  2: Corrected data\n")
    ars <- paste(varcli, "_", anyip, "-", anyfp, ".pval", 
                 sep = "")
    return(invisible())
  }
  else {
    for (i in 1:ne) {
      if (nm < 2) {
        if (stat == "q") 
          val[i, ] <- round(eval(call(fun, dah[, xk, 
                                               i], prob)), ndec)
        else val[i, ] <- round(eval(call(fun, dah[, xk, 
                                                  i])), ndec)
      }
      else {
        if (stat == "q") 
          val[i, ] <- round(apply(dah[, xk, i], 1, fun, 
                                  prob), ndec)
        else val[i, ] <- round(apply(dah[, xk, i], 1, 
                                     fun), ndec)
      }
    }
  }
  if (stat == "me") 
    cat("Mean")
  else if (stat == "mdn") 
    cat("Median")
  else if (stat == "max") 
    cat("Maximum")
  else if (stat == "min") 
    cat("Minimum")
  else if (stat == "std") 
    cat("Standard deviation")
  else if (stat == "q") 
    cat(prob, "prob. quantile")
  else if (stat == "tnd") 
    cat("Trend")
  cat(" values of ", varcli, " (", anyip, "-", anyfp, ")", 
      sep = "")
  if (stat == "tnd") 
    cat(", expressed in units per ", pernys, " years,", sep = "")
  dahs <- data.frame(cbind(est.c[estcol], val))
  if (nm == 12) 
    ndf <- c(estvar[estcol], mes3)
  else if (nm == 13) 
    ndf <- c(estvar[estcol], mes3, "Annual")
  else if (nm < 2) 
    ndf <- c(estvar[estcol], "Value")
  else ndf <- c(estvar[estcol], 1:nm)
  names(dahs) <- ndf
  if (stat == "q") 
    ars <- sprintf("%s_%d-%d_%s%d.csv", varcli, anyip, anyfp, 
                   stat, 100 * prob)
  else ars <- sprintf("%s_%d-%d_%s.csv", varcli, anyip, anyfp, 
                      stat)
  write.csv(dahs, ars, row.names = FALSE, quote = FALSE)
  cat("\n  written to", ars, "\n")
  if (stat == "tnd") {
    dahs2 <- data.frame(cbind(est.c[estcol], pval))
    names(dahs2) <- ndf
    ars <- sprintf("%s_%d-%d_pval.csv", varcli, anyip, anyfp)
    write.csv(dahs2, ars, row.names = FALSE, quote = FALSE)
    cat("P-values written to", ars, "\n")
  }
}



# Crea un unico data frame con le serie omogeneizzate 
# eventualmente mascherate rispetto al flag 1 (ovvero riassegnando NA ai valori che nelle serie omogeneizzate hanno flag 1)  --------
assemblaHomog<-function(param,annoi,annof,mask=TRUE){
  
  #filePattern contiene la descrizione dei file .csv contenente i dati omogeneizzati. I file con i flag
  #hanno lo stesso nome con estensione "-flg.csv" invece di ".csv"
  filePattern<-paste0("^",param,"_",annoi,"-",annof,"_.+[^-flg]\\.csv")
  
  list.files(pattern =filePattern )->nomiFile
  #file _out.csv se presente va eliminato
  nomiFile[!grepl(paste0("^",param,"_",annoi,"-",annof,"_out\\.csv"),nomiFile)]->nomiFile
  
  #ricaviamo i corrispondenti file con i flag
  stringr::str_replace(nomiFile,"\\.csv","-flg.csv")->nomiFlagFile

  purrr::map2(.x=nomiFile,.y=nomiFlagFile,.f = function(nserie,nflag){
    
    tryCatch({
      readr::read_delim(nserie,delim=",",col_names=TRUE)
    },error=function(e){
      stop(sprintf("Errore lettura file %s",nserie))
    })->datiHomo
    
    tryCatch({
      readr::read_delim(nflag,delim=",",col_names=TRUE)   
    },error=function(e){
      stop(sprintf("Errore lettura file %s",nflag))
    })->datiFlag 
    
    if(nrow(datiHomo)!=nrow(datiFlag)) stop("File dati e file flag hanno lunghezze differenti!")
    
    #i file dati vengono mascherati solo se mask==TRUE altrimenti vengono restituiti così come sono.
    #Di default vogliamo mascherare i dati ricostruiti da climatol e rimetterli come NA
    if(mask){
      
      #convertiamo il flag 1 (valore infilled in un altro valore ad esempio 9) e poi convertiamo
      #il flag 0 e 2 in "1". Il nostro obiettivo è moltiplicare i valori della serie per il flag in modo che:
      # laddove il flag vale 1 il valore nella serie non cambia
   
      datiFlag %>% 
        mutate(Value2=ifelse(Value==1,9,Value)) %>%
        mutate(Value3=ifelse(Value2!=9,1,9)) %>%
        mutate(Value4=ifelse(Value3==9,NA,1))->maschera
       
      datiHomo$Value*maschera$Value4->datiHomo$Value
    
    }#fine mask
    
    #estraiamo il codice della serie
    stringr::str_replace(stringr::str_replace(stringr::str_replace(nserie,paste0("^",param,"_",annoi,"-",annof,"_"),""),"-[0-9]+",""),"\\.csv","")->codiceSerie
    names(datiHomo)[2]<-codiceSerie
    print(codiceSerie)
    datiHomo
      
  }) %>% reduce(left_join,by=c("Date"="Date")) %>% separate(col=Date,into=c("yy","mm","dd"),sep="-")->dfOut

  
  #return dfOut: contiene yy mm dd e le serie
  dfOut
  
  
}#fine assembla



# Prepara file per ACMANT3 partendo dal file dei dati mensili -------------
# Acmant3 richiede per l'omogeneizzazione dei mensili file dati analoghi al file ".dat" di climatol, ma:
# -in climatol il file ".dat" contiene i dati di tutte le stazioni, mentre ACMANT3 richiede un file per ogni stazione
# -in climatol ogni riga del file ".dat" non contiene l'informazione dell'anno a cui la riga si riferisce, in ACMANT3 ogni riga ha compe primo dato l'anno
#nomeRete deve essere una stringa di 4 caratteri

#Il file di input è il file dei dati mensili prodotto da Climatol
toACMANT3monthly<-function(fileClimatol,annoi,annof,nomeRete,splitRete=TRUE){


  #nomeRete lo fissiamo a 4 perchè poi aggiungiamo il numero della sottorete, per un totale di 5 caratteri come richiesto da
  #ACMANT
  stopifnot( ( is.numeric(annoi) && is.numeric(annof) ) &&
               is.character(fileClimatol) &&
               is.character(nomeRete) &&
               is.logical(splitRete )) 
  
  if(nchar(nomeRete)!=4 ) stop("nomeRete deve essere una stringa di 4 caratteri!")
  
  #per ACMANT3 vogliamo lavorare con le stesse serie mensili di climatol, quindi carichiamo il file ".dat" prodotto da climatol  
  if(!file.exists(fileClimatol)){
    warning(sprintf("File %s non trovato",fileClimatol))
    return()
  }
  
  #lettura dati
  dat <- scan(fileClimatol, na.strings = NA)

    
  numeroAnni<-annof-annoi+1
  length(dat)->numeroDati
  numeroSerie<-numeroDati/(numeroAnni*12)
  
  #numeroSerie deve essere un intero
  stopifnot(floor(numeroSerie)==numeroSerie)

  #Lettura del file anagrafica, file ".est" di climatol 
  est <- scan(stringr::str_replace(fileClimatol,"\\.dat","\\.est") , na.strings = NA,what="character")
  
  #le colonne nel file est sono 5
  length(est)/5->numeroRighe
  #il numero delle righe nel file .est deve == numeroSerie
  stopifnot(numeroRighe==numeroSerie)
  
  as.numeric(est[seq(1,by =5,length.out = numeroSerie)])->longitude
  as.numeric(est[seq(2,by =5,length.out = numeroSerie)])->latitude
  as.integer(est[seq(3,by =5,length.out = numeroSerie)])->quota
  as.character(est[seq(4,by =5,length.out = numeroSerie)])->codice
  as.character(est[seq(5,by =5,length.out = numeroSerie)])->nome
  
  #creiamo area fittizia, assegnando ad area nomeRete: se splitRete ==FALSE allora tutte le stazioni appartengono 
  #alla medesima area e il programma produce file con un nome file che non distingue tra sottoaree distinte
  area<-rep(nomeRete,numeroSerie)
  
  #se splitRete ==TRUE allora area identifichierà ogni stazione in nord centro e sud, utilizzando il file anagrafica dell'aeronautica
  #Sulla base dell'appartenenza di una serie all'area nord, centro e sud, distingueremo i nomi dei file delle serie. Perchè utilizzare
  #l'anagrafica dell'aeronautica? Perchè è l'unico modo per associare a una stazione dell'aeronautica un'area nord, centro e sud
  
  
  #ACMANT3 richiede per ogni rete un numero di stazioni tra 4 e 99. Quando la rete preparata per climatol
  #contiene un numero superiore di stazioni, dobbiamo creare delle sottoreti. Se splitRete==TRUE
  #cerchiamo un file che ha come nome -> fileClimatol ma come estensione non ".est" (anagrafica per climatol)
  #ma acmant. Questo file è uguale al file ".est" ma con una colonna in più: questa colonna indica il codice
  #della sottorete. Ad esempio: se la rete Cluster1 ha 146 stazioni dobbiamo spezzarla. Per spezzarla
  #cerchiamo il file con estensione ".climatol" e cerchiamo l'ultima colonna che riportà "1" per tutte le stazioni
  #della rete Cluster1 che appartengono alla prima sottorete, "2" per tutte le stazioni della rete Cluster1 che appartengono
  #alla seconda sottorete. Se splitRete==FALSE, allora verrà cercato il file ".est" perchè in tal caso mi servono solo i metadati
  #delle stazioni
  if(splitRete){    
      
      readr::read_delim("reg.aeronautica.info.csv",delim=";",col_names = TRUE)->Aero
      purrr::map_chr(codice,function(cod){
      
        stringr::str_replace(tolower(cod),"_.+$","")->regione
        
        if(regione!="aeronautica"){
        
          grep(regione,tolower(Aero$County))->righe
          #nord centro o sud?
          unique(Aero$area[righe][!is.na(Aero$area[righe])])->area
          if(length(area)!=1){
            if(regione=="bolzano"){
              area<-"nord"
            }else{  
              browser()
            }
          }  
            
        }else{
          
          unlist(stringr::str_split(cod,"_"))[2]->siteID
          which(Aero$SiteId==siteID)->riga
          stopifnot(length(riga)==1)
          Aero$area[riga]->area
                              
        }

        area
          
  })->area

  #area a questo punto contiene "nord centro o sud" oppure contiene il nome della rete per ogni stazione
  stopifnot(all(!is.na(area)))    

          
  } #if splitRete
    
  #crea data frame "out" con le serie mensili, partendo dai valori mensili creati da climatol
  cbind(data.frame(yy=rep(seq(annoi,annof),each=12),mm=rep(seq(1,12),times=numeroAnni)),
  purrr::map(seq(1,by=(numeroAnni*12),length.out = numeroSerie),.f=~(dat[.:(.+(numeroAnni*12)-1)])) %>% 
    reduce(cbind) %>% 
      as.data.frame) ->out  
  
  names(out)<-c("yy","mm",codice)

  #nomiArea può essere un solo nome (nomeRete) o potrebbe essere ad esempio nord centro e sud
  unique(area)->nomiArea
  
  for(ii in 1:length(nomiArea) ){
      
      which(area==nomiArea[ii])->qualiColonne
      stopifnot(length(qualiColonne)!=0)
      #per essere in linea con names(out) che contiene yy e mm
      qualiColonne<-qualiColonne+2
      progressivo<-1
      
      #A questo punto possiamo utilizzare la funzione "scriviFile_dat" per creare i file (uno per ogni serie) in formato "climatol" con l'aggiunta
      #dell'anno (prependAnno=TRUE)
      purrr::walk(qualiColonne,.f=function(colonna){

        #ogni nome file inizia con una stringa di 5 caratteri--> nomeRete
        #a cui segue un indice crescente 01, 02, 03....
        paste0(nomeRete,ii,str_pad(progressivo,width = 2,side = "left",pad ="0"))->nomeAcmant
#        if(nomeAcmant=="clu1201") browser()
        scriviFile_dat(out[,c(1,2,colonna)],nomeOut = nomeAcmant,file_monthly =TRUE,acmant = TRUE)
        progressivo<<-progressivo+1
        
      })#fine purrr::walk      
  
  }#fien ciclo for  
    
    
}#fine toACMANT3monthly





# Prepara file per ACMANT3 partendo dal file dei dati giornalieri -------------
#ACMANT3 lavora sia sui file a livello mensile che a livello giornaliero
#Per l'omogeneizzazione dei giornalieri file dati analoghi ai file mensili di ACMANT3:
#ogni riga ha come primi due dati l'anno e il mese

#Il file di input è un data frame con colonne yy,mm,dd e poi colonne dati giornalieri per stazione
toACMANT3daily<-function(df,annoi,annof,nomeRete,fileClimatol){
  
  
  #nomeRete lo fissiamo a 4 perchè poi aggiungiamo il numero della sottorete, per un totale di 5 caratteri come richiesto da
  #ACMANT
  stopifnot( ( is.numeric(annoi) && is.numeric(annof) ) &&
               is.character(fileClimatol) &&
               is.character(nomeRete) )
  
  if(nchar(nomeRete)!=5 ) stop("nomeRete deve essere una stringa di 5 caratteri!") #qui passiamo nomeRete già con 5 caratteri
  
  #per ACMANT3 vogliamo lavorare con il file ".est" prodotto da climatol, che contiene l'anagrafica delle stazioni 
  #In realtà si chiama fileClimatol per omogeneità con il programma toACMANT3monthly. Basta che sia
  #un file con l'anagrafica delle stazioni
  if(!file.exists(fileClimatol)){
    warning(sprintf("File .est %s non trovato",fileClimatol))
    return()
  }
  
  
  #Lettura del file anagrafica, file ".est" di climatol 
  est <- scan(fileClimatol, na.strings = NA,what="character")  

  #le colonne nel file est sono 5
  length(est)/5->numeroRighe  
    
  #il numero delle righe nel file .est deve == numeroSerie
  numeroSerie<-numeroRighe
  
  numeroAnni<-annof-annoi+1
  
  as.numeric(est[seq(1,by =5,length.out = numeroSerie)])->longitude
  as.numeric(est[seq(2,by =5,length.out = numeroSerie)])->latitude
  as.integer(est[seq(3,by =5,length.out = numeroSerie)])->quota
  as.character(est[seq(4,by =5,length.out = numeroSerie)])->codice #tutti i codici delle stazioni
  as.character(est[seq(5,by =5,length.out = numeroSerie)])->nome
  
  #creiamo area fittizia, assegnando ad area nomeRete: se splitRete ==FALSE allora tutte le stazioni appartengono 
  #alla medesima area e il programma produce file con un nome file che non distingue tra sottoaree distinte
  area<-rep(nomeRete,numeroSerie)
  
  
  purrr::iwalk(codice,.f=function(.x,.y){
    
    .x->codiceStazione
    .y->progressivo
    
    #cerco nel dataframe la stazione che ha codice "qualeCodice"
    which(names(df) %in% codiceStazione)->qualeColonna
    #non trovo la colonna nel file dati, passo oltre
    if(!length(qualeColonna)) {sprintf("Il codice $s non è nel file dati, per questa stazione non verrà prodotto output!",codiceStazione); return()}
    if(length(qualeColonna)>1) stop(sprintf("Il codice $s è presente in più colonne nel file dati, mi fermo!",codiceStazione))


    #ogni nome file inizia con una stringa di 5 caratteri--> nomeRete
    #a cui segue un indice crescente 01, 02, 03....
    paste0(nomeRete,str_pad(progressivo,width = 2,side = "left",pad ="0"))->nomeAcmant
    #scriviFile_dat_acmant_daily analoga a scriviFile_dat ma riscritta per tener conto anche del giorno
    scriviFile_dat_acmant_daily(df[,c(1,2,3,qualeColonna)],nomeOut = nomeAcmant)

  })#fine purrr::walk      

  
}#fine toACMANT3daily

