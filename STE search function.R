setwd('C:/Users/LocalAdmin/Documents/Smith/Lit review')

library(bibliometrix)
library(data.table)
#library(tm)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)

#############################################################################################################################
#search string, written for perl=T
Intervention <- c("conserv", "protect", "management\\b", "awareness\\b", "law\\b",'laws\\b', 
                  "policy", "policies\\b","reserve\\b","reserves\\b", "park", "no-take", "\\bMPA\\b","\\bMPAs\\b", 
                  "natural monument", "wilderness area", "govern", 
                  "capacity-build", "train", "regulation", "enclosure",
                  "payment for ecosystem service", "\\bPES\\b", "certification\\b", 
                  "eco-label", "Marine Stewardship Council\\b", "\\bMSC\\b", "ecotourism\\b", 
                  "sustainable use\\b", ("communit((\\w+)(\\W+)){1,3}management\\b"), 
                  "conserv((\\w+)(\\W+)){1,3}management\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), 
                  ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
                  "comanagement\\b", "co-management\\b") 
Int.adj <-c("ecosystem", "species\\b", "habitat", "biodiversity\\b", "sustainab" 
            , "ecolog", "integrated\\b", "natural resource", "conserv")  
Marine <- c("marine\\b", "coastal\\b", "\\bsea\\b", "\\bseas\\b", "seascape\\b", "coral reef"
            , "reef","ocean\\b", "seagrass", "mangrove", "estuar") 
Outcome <- c("human", "people\\b", "person", "household", "communit",
             "fisher", "touris", "diver\\b", "divers\\b", "stakeholder", 
             "collaborative\\b", "wellbeing\\b", "well-being\\b", "well being\\b", 
             "ecosystem service", "nutrition\\b", "skill", "empower", 
             "clean water\\b", "livelihood", "(food) security\\b", "catch per unit effort\\b" 
             , "CPUE\\b", "yield\\b" , "landings\\b", "catch\\b", "spillover\\b", "cultur",
             "education\\b", "service" , "resilience", "vulnerability\\b"
             , "(social) capital\\b", "attitude", "perception", "(human) health", "human capital\\b"
             , "traditional knowledge\\b", "\\bTEK\\b", "poverty\\b", "tenure\\b")
STE <-  c("synerg", "tradeoffs\\b", "tradeoff\\b", "trade-off\\b", "trade-offs\\b", "equit" 
          , "inequit", "equalit", "inequalit", "co-benefit","conflict", "joint product", 
          "benefit sharing\\b", "constrain")
#############################################################################################################################
#read in Ernest's data 
Edata<- read.csv("ErnestEtAl_library.csv", header = TRUE, stringsAsFactors = FALSE)
Edata$Keywords <- 'NA'
Edata$Source <- 'Ernest et al'
names(Edata)
#read in Sam's data, 
Sdata<- read.csv("McKinnonEtAl_16Feb17.csv", header = TRUE, stringsAsFactors = FALSE)
Sdata$Source <- 'McKinnon et al'
Sdata$Journal <- 'Unknown'
names(Sdata)[c(1:7,54)]
#merge the two datasets
names(Edata[,c(1,30,2,4,3,10,5,128,29)])
names(Edata)[c(1,30,2,4,3,10,5,128,29)] <- 
c("Record.Number","Reference.Type","Author","Year","Title","Journal","Abstract","Keywords","Source" )

SEdata <- rbind(Sdata[,c("Record.Number","Reference.Type","Author","Year","Title","Journal","Abstract","Keywords","Source" )],
                Edata[,c("Record.Number","Reference.Type","Author","Year","Title","Journal","Abstract","Keywords","Source" )])


#############################################################################################################################
# Function that converts 
# (1) special letters to normal letter (e.g. ä to a) 
# (2) removes punctuation except -()
# (3) removes double spaces

fun.clean <- function(x) {
  x <- iconv(x, "latin1", "ASCII//TRANSLIT") # possibly latin1 needs to be changed depending on local language settings
#  x <- gsub("[^-()[:alnum:]]"," ",x) # replaces punctuation except -() with space
  x <- gsub(" +"," ",x) # removes double space
  x
}
SEdata$Title <- fun.clean(SEdata$Title)
SEdata$Abstract <- fun.clean(SEdata$Abstract)
#############################################################################################################################
# run search

SEresults <- SEdata[ grepl(paste(Intervention,collapse = '|'),paste(SEdata$Title,SEdata$Abstract), ignore.case = T,perl=T)
              & grepl(paste(Int.adj,collapse = '|'),paste(SEdata$Title,SEdata$Abstract), ignore.case = T,perl=T)
              & grepl(paste(Marine,collapse = '|'),paste(SEdata$Title,SEdata$Abstract), ignore.case = T,perl=T)
              & grepl(paste(Outcome,collapse = '|'),paste(SEdata$Title,SEdata$Abstract), ignore.case = T,perl=T)
              & grepl(paste(STE,collapse = '|'),paste(SEdata$Title,SEdata$Abstract), ignore.case = T,perl=T)
              ,]
#View(SEresults[1:20,c('Title','Abstract')])

#############################################################################################################################
#read in and add WoS data
WoS1 <- readFiles("WoSsavedrecs1-500_1Mar17.bib")
WoS2 <- readFiles("WoSsavedrecs501-1000_1Mar17.bib")
WoS3 <- readFiles("WoSsavedrecs1001-1302_1Mar17.bib")

WoS1 <- convert2df(WoS1, dbsource = "isi", format = "bibtex")
WoS2 <- convert2df(WoS2, dbsource = "isi", format = "bibtex")
WoS3 <- convert2df(WoS3, dbsource = "isi", format = "bibtex")

WoS <- rbind(WoS1,WoS2,WoS3)
WoS$Record.Number <- seq(1,nrow(WoS),1)
WoS$Source <- 'Web of Science'

#merge the two datasets (WOS columns: "Record.Number", "DT", "AU", "PY","TI","SO","AB","DE","Source")
names(WoS)[c(22,8,1,15,2,3,11,9,23)]
names(WoS)[c(22,8,1,15,2,3,11,9,23)]<- names(SEresults)
SEW_results <- rbind(SEresults,WoS[,c(22,8,1,15,2,3,11,9,23)])

#############################################################################################################################
#read in mined bibliography data
BibMined <- read.csv("Mined_bibliographies_16Feb17.csv", header = TRUE, stringsAsFactors = FALSE)
names(BibMined)
BibMined$Journal="Unknown"
All_results <- rbind(SEW_results,BibMined[,c("Record.Number","Reference.Type","Author","Year","Title","Journal","Abstract","Keywords","Source")])

#Find and remove duplicates
#All_results$Title <- tolower(All_results$Title)
#All_results$Abstract <- tolower(All_results$Abstract)
All_results <- distinct(All_results,Title, .keep_all = TRUE)
All_results <- arrange(All_results,Title)

#All_results$C1.Marine <- 0; #All_results$C2.MPA <- 0; #All_results$C2.PES <- 0
#All_results$C2.Cert <- 0; #All_results$C2.CBNRM <- 0; #All_results$C3.Outcomes <- 0
#All_results$C4.Int.outcomes <- 0; #All_results$C5.STE <- 0; #All_results$Reviewer <- NA
#All_results$Comments <- NA

#test: read in Ernest's already screened data
#E_scrned <-  read_excel("Ernest et al Marine coding.xlsx")
#E_scrned_marine <- subset(E_scrned,Marine==1)

#All_results$Reviewer[All_results$Source=='Ernest et al' & All_results$Record.Number%in%E_scrned_marine$ID] <- 'Ernest et al 2014'
#nrow(subset(All_results,Reviewer=='Ernest et al 2014'))
#unique(All_results$Reviewer)
#write.csv(All_results,'TitleAbstractCoding_19May17.csv',row.names = F)

# Clean up reference types
All_results <- read.csv('TitleAbstractCoding_19May17.csv',stringsAsFactors = F, header = T)

All_results1 <- All_results
unique(All_results1$Reference.Type)
Articles <- c(unique(All_results1$Reference.Type[grep(c('Article'),All_results1$Reference.Type,ignore.case = T)]),
unique(All_results1$Reference.Type[grep('J',All_results1$Reference.Type)]))
All_results1$Reference.Type[All_results1$Reference.Type%in%Articles] <- 'Article'
All_results1$Reference.Type[All_results1$Reference.Type%in%c('B','Book')] <- 'Book'
All_results1$Reference.Type[All_results1$Reference.Type%in%c('Book Section')] <- 'InBook'
All_results1$Reference.Type[All_results1$Reference.Type%in%c('S','Serial')] <- 'Misc'
All_results1$Reference.Type[All_results1$Reference.Type%in%c('Conference Proceedings','INPROCEEDINGS ')] <- 'InProceedings'
All_results1$Reference.Type[All_results1$Reference.Type%in%c('INCOLLECTION ')] <- 'InCollection'
unique(All_results1$Reference.Type)

# Fill missing author data with NAs
All_results1$Author[All_results1$Author==''|is.na(All_results1$Author)] <- 'NA'

#Add missing fields
#All_results1$journal <- 'unknown'
All_results1$editor <- 'NA';All_results1$type <- 'NA';All_results1$institution <- 'NA';
All_results1$translator <- 'NA';All_results1$booktitle <- 'NA';All_results1$maintitle <- 'NA'
names(All_results1)
names(All_results1) <- tolower(names(All_results1))
names(All_results1)[2] <- 'bibtype'

#Create Key
#All_results1$key <- paste0(stringr::str_sub(All_results1$author, 1, 6),All_results1$year) # create year variable
#All_results1$key <- gsub(" ", "", All_results1$key, fixed = TRUE)
#All_results1$key <- gsub(".", "", All_results1$key, fixed = TRUE)
#head(All_results1$key)

All_results2 <- All_results1[c('record.number','bibtype','author','year','title','abstract','keywords',
                               'journal','source','editor','type','institution','translator',
                               'booktitle','maintitle')]
write.csv(All_results2,'TitleAbstractCoding_21Oct17.csv',row.names = F)
## END
#############################################################################################################################

n <- c(2:(nrow(All_results2)))
start <- All_results2[1,]
fullbib <- as.BibEntry(start)

unique(All_results2$bibtype)
for (i in n){
  sub <- All_results2[i,]
  bib <- as.BibEntry(sub)
  fullbib <- c(fullbib,bib)
}

 WriteBib(fullbib, file="TitleAbstractCoding_19May17.bib")


#will it re-import?

bibtest <- ReadBib('Test_csv_convert.bib')
bibtest <- ReadBib('McKinnon_mod.bib')


bib <- c(bibtype = "article", key = "mclean2014", title = "My New Article",
         author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01")
as.BibEntry(bib)

#############################################################################################################################


#Filter for unscreened abstracts only
#Eunscrn<- Edata[is.na(Edata$T..BDC)&
#                  is.na(Edata$T..Intervention)&
#                  is.na(Edata$T..Outcome)&
#                  is.na(Edata$T..Causality)&
#                  is.na(Edata$T..PA),]
