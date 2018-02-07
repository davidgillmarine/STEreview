setwd('C:/Users/LocalAdmin/Documents/Smith/Lit review')

library(bibliometrix)
library(data.table)
library(tm)
library(stringr)
library(tidyr)
library(dplyr)

#D <- readFiles("savedrecs.bib")
#M <- convert2df(D, dbsource = "isi", format = "bibtex")
#head(M)

#read in sample file, search titles for now
WoS1 <- readFiles("WoSsavedrecs1-500_15Feb17.bib")
WoS2 <- readFiles("WoSsavedrecs501-1000_15Feb17.bib")
WoS3 <- readFiles("WoSsavedrecs1001-1408_15Feb17.bib")

WoS1 <- convert2df(WoS1, dbsource = "isi", format = "bibtex")
WoS2 <- convert2df(WoS2, dbsource = "isi", format = "bibtex")
WoS3 <- convert2df(WoS3, dbsource = "isi", format = "bibtex")

WoS <- rbind(WoS1,WoS2,WoS3)
WoS$Record.Number <- seq(1,nrow(WoS),1)
WoS$Source <- 'Web of Science'
names(WoS)

#test strings
SresultsE <- WoS[ grepl(paste(Intervention,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                  & grepl(paste(Int.adj,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                  & grepl(paste(Marine,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                  & grepl(paste(Outcome,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                  & grepl(paste(STE,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                  ,]

SresultsE$TI

Sresults1 <- WoS[ grepl(paste(Intervention,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                   & grepl(paste(Int.adj,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                   & grepl(paste(Marine,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                   & grepl(paste(Outcome,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                   & grepl(paste(STE,collapse = '|'),paste(WoS$TI), ignore.case = T,perl=T)
                   ,]

Sresults1$TI

#My failed attempts
specialstring <- '(communit)+(\\w+)\\s(\\w+)\\s(\\w+)\\s(\\w+)\\s(manag)+(\\w+)'


Sresults <-M[grepl(Intervention,M$TI,ignore.case=T),]
Sresults <-M[grepl("COMMUN(.+ +| +)(([a-z]+ +){0,3})MANAG(. +|.| +|$)",M$TI,ignore.case = TRUE,perl=T),]
Sresults$TI

Sresults <-WoS[grepl('sea ',WoS$TI,ignore.case=T,perl=T),]
Sresults1 <-WoS[grepl('sea\\b',WoS$TI,ignore.case=T,perl=T),]
Sresults$TI
Sresults1$TI[!Sresults1$TI%in%Sresults$TI]

Sresults <-Unscrn[grepl('communit((\\w+)(\\W+)){1,4}manag',Unscrn$Title,ignore.case=T,perl=T),]
Sresults <-Unscrn[grepl('estuar+',Unscrn$Title,ignore.case=T,perl=T),]
Sresults$Title

#When we get it all figured out

Eresults[5,]
Sresults1 <- Eresults[ grepl(paste(STE,collapse = '|'),paste(Eresults$Title[5],Eresults$Abstract.merged[5]), ignore.case = T,perl=T)
                   ,]
Sresults1$TI



