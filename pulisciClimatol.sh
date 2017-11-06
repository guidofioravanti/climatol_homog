#!/bin/bash
#pulisce la directory di lavoro per una nuova sessione di Climatol

#elimina i file pdf
rm -rf *.pdf

#rimuovi i file con i flag
rm -rf *flg.csv

#rimuovi i file per ogni singola stazione
rm -rf Tmax_*.csv
rm -rf Tmin_*.csv

#rimuovi i file per ogni singola stazione
rm -rf Tmax*.txt
rm -rf Tmin*.txt

#rimuovi i file .rda
rm -rf *.rda

#rimuovi i file dei breakpoints
rm -rf *brk.csv

#rimuovi il file con gli outliers identificato da Climatol
rm -rf Tm[ai][nx]*_out.csv
