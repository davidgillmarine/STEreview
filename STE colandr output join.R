# This code was used to clean up the Colandr output data where some of the data in the fields were lost

library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
options(scipen=999,stringsAsFactors = FALSE)
setwd('C:/Users/LocalAdmin/Documents/OneDrive - Conservation International 1/Smith/Lit review')

# Read in the original dataset and the colandr post-fulltext screening output
All_results <- read.csv('TitleAbstractCoding_21Oct17.csv',header = T)
colandr_out <- read.csv('Marine STE 2 20Oct17.csv',header = T)
names(colandr_out)
apply(colandr_out, 2, function(x) sum(is.na(x)))
apply(All_results, 2, function(x) sum(is.na(x)))


# Clean up titles to join both datasets
colandr_out$citation_title <- tolower(iconv(colandr_out$citation_title,to = "latin1"))
All_results$title <- tolower(iconv(All_results$title,to = "latin1"))

# titles that don't match up exactly
bad_titles <- colandr_out$citation_title[!colandr_out$citation_title%in%All_results$title]

for (i in 1:length(bad_titles)){
  colandr_out$citation_title[colandr_out$citation_title==bad_titles[i]] <- All_results$title[agrep(bad_titles[i],All_results$title)] 
}

# join tables, match by title
All_results1 <- All_results %>%
  distinct(title,.keep_all=T)  %>% 
  inner_join(filter(colandr_out),by =c("title"="citation_title")) %>%
  mutate(author=tolower(author),
         keywords=tolower(keywords),
         abstract=tolower(abstract),
         journal=tolower(journal)) %>%
  select(study_id,bibtype:maintitle,deduplication_status:data_extraction_screening_status,
         fulltext_exclude_reasons,citation_pub_year)

included.results <- All_results1 %>%
                filter(fulltext_screening_status=="included")

# See if numbers match up with Colandr    
table(All_results1$fulltext_screening_status) # 108 included, 163 excluded (4 could not be screened)
table(All_results1$citation_screening_status) # 275 included, 2,274 excluded

# Allow some pub years don't match, these were all duplicates (distinct function removed the other duplicate)
View(All_results1 %>%
       filter(citation_pub_year!=year))

# ensure that all the included studies were included
col.include <- filter(colandr_out,fulltext_screening_status=="included")
col.include[!col.include$study_id%in%included.results$study_id,'deduplication_status'] # should be 0

write.csv(All_results1,'fulltext_results_20Oct17.csv',row.names = F)    
write.csv(included.results,'included_results_20Oct17.csv',row.names = F)    

# END
##########################################################################################################    

  



#check to ensure that authors approximately match up
for (i in 1:length(All_results1$author)){
  results <- data.frame(x=0)
  test.author1 <- tolower(gsub("[^-()[:alnum:]]"," ",All_results1$citation_authors)) # replaces punctuation except -() with space
  test.author1 <- gsub(" +"," ",test.author) # removes double space
  test.author2 <- tolower(gsub("[^-()[:alnum:]]"," ",All_results1$author)) # replaces punctuation except -() with space
  test.author2 <- gsub(" +"," ",test.author) # removes double space  
 # results[i] <- agrep(test.author2[i],test.author,fixed = T,max.distance = 0.1)-i
  print(agrep(test.author2[i],test.author,fixed = T,max.distance = 0.5)-i)
}



#Fill missing titles with first 10 words of abstract
apply(colandr_out, 2, function(x) sum(is.na(x)))
missing.title <- is.na(colandr_out$citation_title)
View(colandr_out[missing.title,])
colandr_out$citation_abstract <- tolower(iconv(colandr_out$citation_abstract, from="UTF-8", to = "latin1"))
colandr_out$citation_title <- ifelse(is.na(colandr_out$citation_title),
                                     stringr::word(colandr_out$citation_abstract, start = 1, end = 10),
                                     colandr_out$citation_title)
View(colandr_out[missing.title,])
All_results$abstract <- tolower(iconv(All_results$abstract,from="UTF-8", to = "latin1"))
All_results$title <- ifelse(is.na(All_results$title),
                            stringr::word(All_results$abstract, start = 1, end = 10),
                            All_results$title)
apply(All_results, 2, function(x) sum(is.na(x)))
