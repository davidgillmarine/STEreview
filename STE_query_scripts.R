library(tidyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(extrafont)
library(ggthemes)
library(d3Network)
library(treemap)
library(maptools)
library(rio)
library(countrycode)
library(reshape)
library(circlize)
library(dplyr)

# setwd("~/Documents/github/marine_ste/")
# map_data_final <- readRDS("data/map_data_final_4_24.rds")
# load("data/STE_Evidence_Map_2_10_2018.RData")
# Directories
dropbox <- "C:/Users/dgill6/Dropbox/SynergiesTradeoffs.Review/Data extraction/"
datadir <- "C:/Users/dgill6/Dropbox/data/analysis/STEreview/"

##On Sam's comp
dropbox <- "~/Dropbox/SynergiesTradeoffs.Review/Data extraction/"
datadir <- "~/Documents/github/marine_ste/data/"

# Projections
wgs84 <- CRS("+proj=longlat +ellps=WGS84")

ext.table <- import(paste0(dropbox,'STE extracted results to date.xlsx'), sheet = 1) 
# paste0('Are article and outcome exclusion values are identical? ',identical(ext.table$`05 Exclude`,ext.table$`40 Exclude`))
# if (!identical(ext.table$`05 Exclude`,ext.table$`40 Exclude`)){
#   View(ext.table[ext.table$`05 Exclude`!=ext.table$`40 Exclude`,])
#   ext.table <- ext.table[ext.table$`05 Exclude`==ext.table$`40 Exclude`,]
# }
# clean up variable names
names(ext.table) <- gsub(" ","_",names(ext.table))
ext.data <- ext.table %>% 
  rename(aid='Data_extraction_questionnaire.01_Article_ID',
         Assessor="02_Name_of_Assessor",
         Assess_date="04_Date_of_assessment",
         exclude='05_Exclude',
         Pub_type='11_Publication_type',
         Authors="12_Author",
         Pub_year='13_Year_of_publication',
         Title="14_Title",
         Journal='15_Journal', 
         source='110_Source', 
         auth_aff='17_Affiliation_type_of_first_author',
         Funding="18_Funding_source",
         DOI="19_DOI",
         Source="110_Source",
         Bib_notes="111_notes_on_bibliography",
         Int_type='22_Type_of_intervention',
         int_scale='24_Geographic_scale_of_intervention',           
         int_time='25_Timeframe_of_intervention',           
         int_auth='27_Managing_authority',           
         Int_location='28_Intervention_location',           
         Int_area='29_Intevention_area',
         Stated_int="21_Stated_intervention",
         Desired_out="23_Desired_outcome_of_intervention",
         Int_dur="25_Timeframe_of_intervention",
         Int_funding="26_Funding_source",
         Int_notes="210_notes_on_intervention",
         Study_goal="31_Stated_goal_of_the_study",
         Study_type='32_Study_type',
         Design.control_comp='33_Study_includes_a_comparison/control_group',
         Comp_type='34_Type_of_comparison_(control_group)',
         Data_source='35_Data_source',
         Study_location="36_Location_of_study",
         Study_country='37_Country_of_study',
         stdy_scl='38_Study_scale',           
         Biome.='39_Biome', 
         eval_aff='310_Affliation_of_evaluator',           
         Unit_analysis='311_Social_unit_of_analysis', 
         Sample_Size='312_Sample_size_(maximum)', 
         Study_duration='313_Duration_of_study',
         Study_notes="314_notes_on_study_type",
         Distributive_equity='51_Distributive_equity', 
         Recognitional_equity='52_Recognitional_equity', 
         Contextual_equity='53_Contextual_equity', 
         Procedural_equity='54_Procedural_equity',
         Equity_notes="55_Equity_notes",
         Causal_pathway='61_Causal_pathway/theory_of_change', 
         Explicit_mechanism='62_Explicit_mechanism', 
         Pathway_notes="63_Pathway_notes",
         oid='OutcomeID',
         Stated_outcomes='41_Stated_outcome_measured',
         Out_type='42_Outcome_domain',
         Out_subtype='43_Outcome_attribute',
         Indicators='44_Outcome_indicator',
         Out_definition='45_Perceptions_vs_observed_outcome',
         Outcome.data_type='46_Outcome_data-type',
         Resource_type='47_Resource_use_type',
         Outcome.direction='48_Overall_outcome_direction',
         Pluralism='49_Pluralism_(who_defines_STEs?)',
         Out_subgroup='410_Outcome_sub-group',
         STE_domain='411_STE:_social_domain/HWB_outcomes',
         STE_econ='412_STE:_(economic_status)',
         STE_social='413_STE:__(social_status)',
         STE_sex='414_STE:__(sex)',
         STE_ethnic='415_STE:__(ethnic_group/race)',
         STE_age='416_STE:__(age)',           
         STE_occupation='417_STE:__(occupation)',
         STE_social_org='418_STE:_level_of_social_organization',
         STE_spatial='419_STE:_spatial_units',
         STE_temporal_unit='420_STE:_temporal_units',
         STE_temporal_scale='421_STE:_temporal_scales', 
         Outcome_notes="422_Outcome_notes" 
  )

# remove open-ended or unneeded variables
unwanted.var <- grep('(\\d)',names(ext.data),value = T) # variables that still contain a number in the name
data <- ext.data %>% 
  select(-one_of(unwanted.var)) %>%
  mutate(Comp_type=gsub("Pre-treatment/post-treatment","Pre/post treatment",Comp_type),
         Int_type=gsub("CBNRM, MPA","CBNRM_MPA",Int_type),
         Affil_type.Academic=ifelse(grepl("Academic",auth_aff),1,0),
         Affil_type.Public_sector=ifelse(grepl("Public",auth_aff),1,0),
         Affil_type.Research_Institute=ifelse(grepl("Research",auth_aff),1,0),
         Affil_type.Consultant=ifelse(grepl("Consultant",auth_aff),1,0),
         Affil_type.Non_profit=ifelse(grepl("profit",auth_aff),1,0),
         Affil_type.Private_sector=ifelse(grepl("Private",auth_aff),1,0),
         Affil_type.International_Organization=ifelse(grepl("International",auth_aff),1,0),
         Int_type.MPA=ifelse(grepl("MPA",Int_type),1,0),
         Int_type.PES=ifelse(grepl("PES",Int_type),1,0),
         Int_type.CBNRM=ifelse(grepl("CBNRM",Int_type),1,0),
         Int_type.Certification=ifelse(grepl("Certification",Int_type),1,0),
         Int_geo.local=ifelse(grepl("Local",int_scale),1,0),
         Int_geo.region=ifelse(grepl("Region",int_scale),1,0),
         Int_geo.national=ifelse(grepl("National",int_scale),1,0),
         Int_geo.subnational=ifelse(grepl("Sub-national",int_scale),1,0),
         Int_geo.international=ifelse(grepl("International",int_scale),1,0),
         Mang_auth.Government=ifelse(grepl("Government",int_auth,ignore.case = T),1,0),
         Mang_auth.Shared_governance=ifelse(grepl("shared",int_auth,ignore.case = T),1,0),
         Mang_auth.Private_governance=ifelse(grepl("private",int_auth,ignore.case = T),1,0),
         Mang_auth.Indigenous_community_governance=ifelse(grepl("Indigenous",int_auth,ignore.case = T),1,0),
         Mang_auth.multiple=ifelse(grepl("multiple",int_auth),1,0),  
         Mang_auth.mixed=ifelse(grepl("mixed",int_auth,ignore.case = T),1,0)) %>% 
        filter(exclude==FALSE)
         
# View(select(ext.data,auth_aff,Affil_type.Academic:Affil_type.International_Organization))
# View(select(ext.data,Int_type,Int_type.MPA:Int_type.Certification))
# View(select(ext.data,int_scale,Int_geo.local:Int_geo.international))
# View(select(ext.data,int_auth,Mang_auth.Government:Mang_auth.mixed))

##########################################
io_counts <- data %>% 
        filter(exclude==FALSE) %>% 
        group_by(Int_type,Out_type) %>% 
        count() %>% 
        spread(key=Int_type, value=n) %>% 
        mutate(PES=0) %>%
        gather("Int_type","n",CBNRM:PES) %>% 
        mutate(n=ifelse(is.na(n),0,as.integer(n)))

# out_type <- c("Economic well-being","Health","Political empowerment","Social capital","Education","Culture")
# int_type <- c("MPA","PES","CBNRM","CBNRM_MPA","Certification")
# 
# io_counts = matrix(nrow=6, ncol=5)
# rownames(io_counts) <- out_type
# colnames(io_counts) <-int_type
# 
# for (i in int_type){
#   for (j in out_type){
#     subset <- filter(data, Out_type == j, Int_type == i)
#     io_counts[j,i] <- n_distinct(subset$aid)
#   }
# }
# 
# rownames(io_counts) <- out_type
# colnames(io_counts) <- int_type
# io_counts <- as.data.frame(io_counts)
# io_counts$Out_type <- rownames(io_counts)
# io_counts <- gather(io_counts,"Int_type","n",1:5)

pdf(file=paste0(datadir,"STE_Int_Outcome_Heatmap_articles_test.pdf"), width=11, height=8.5)
ggplot(io_counts, aes(x=Out_type,y=Int_type,fill=n)) +
  geom_tile(color="gray90",size=0.1) +
  geom_text(aes(label=n, color="white")) +
  scale_fill_gradient(low="#f7fbff",high="#084594",name="# Cases",na.value="gray90") +
  coord_equal() +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_line(size=0.4)) +
  theme(axis.text=element_text(size=12)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title.align=1) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(1, "cm")) +
  theme(legend.key.width=unit(1, "cm")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12))
dev.off()

##BY STE TYPE AND OUTCOME DOMAIN - HEATMAP

ste_outcomes <- select(data,aid,oid,Int_type,Out_type,STE_domain,STE_econ,STE_social,STE_sex,STE_ethnic,STE_age,STE_occupation,STE_social_org,STE_spatial,STE_temporal_unit,STE_temporal_scale) %>% distinct() 

ste_outcomes2 <- ste_outcomes %>% gather("ste_type","ste_interaction",5:15) %>% arrange(Int_type) %>% filter(ste_interaction != "NA") %>% distinct() %>% filter(!is.na(Out_type)) %>% filter(!is.na(Int_type))

n_distinct(ste_outcomes2$aid)
n_distinct(ste_outcomes2$oid)

ste_outcomes3 <- ste_outcomes2 %>% select(-aid)

ste_types <- c("STE_domain","STE_econ","STE_social","STE_sex","STE_ethnic","STE_age","STE_occupation","STE_social_org","STE_spatial","STE_temporal_unit","STE_temporal_scale")

matrix <- count(ste_outcomes3,Out_type,Int_type,ste_type)
matrix2 <- matrix %>% ungroup() %>% mutate(Int_type=factor(Int_type, levels=c("MPA","CBNRM","CBNRM_MPA","Certification")))
matrix2$n <- as.numeric(matrix2$n)

all_poss <- expand.grid(Out_type=unique(matrix2$Out_type),ste_type=unique(matrix2$ste_type),Int_type=unique(matrix2$Int_type))
all_merge <- merge(matrix2,all_poss,by=c("Out_type","ste_type","Int_type"),all=TRUE) %>% arrange(Int_type)

gg <- ggplot(all_merge, aes(x=ste_type,y=Out_type,fill=n)) + 
  geom_tile(color="gray90",size=0.1) +
  geom_text(aes(label=n, color="white")) +
  scale_fill_gradient(low="#f7fbff",high="#084594",name="# Cases",na.value="gray90") +
  coord_equal() +
  facet_wrap(~Int_type, ncol=3) +
  labs(x="STE Type",y=NULL,title="STEs observed in domains of human well-being amongst marine conservation interventions (193 cases in 77 articles)") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_line(size=0.4)) +
  theme(axis.text=element_text(size=7)) +
  theme(panel.border=element_rect(fill=NA,size=0.1)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(panel.spacing.x=unit(0.5, "cm")) +
  theme(panel.spacing.y=unit(0.5, "cm")) +
  theme(legend.title=element_text(size=6)) +
  theme(legend.text=element_text(size=6)) +
  theme(legend.title.align=1) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(0.2, "cm")) +
  theme(legend.key.width=unit(1, "cm")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=7))

pdf(file=paste0(datadir,"STE_type_Int_Outcome_Heatmap_5_11.pdf"), width=11, height=8.5)
gg
dev.off()

#############################################################################

##Plot countries
# #load in full country list
country <- read.csv(paste0(datadir,"allcountries.csv"), head=TRUE, sep=",")
names(country) <- c("Study_country", "Region", "Code", "Subregion")
regions <- country
regions <- arrange(regions,Region)

#Fix aids with multiple countries


ctry <- data %>% 
  filter(exclude=='FALSE') %>% 
  group_by(aid,Study_country) %>% 
  summarise() %>% 
  separate(Study_country,c('ctry1','ctry2'),sep=",",remove = F) %>%
  filter(ctry2 != " United Republic Of") %>%
  filter(ctry2 != " Province Of China") %>%
  gather(key=ctry.col,value=ctry,ctry1:ctry2) %>%
  select(aid,ctry)

colnames(ctry) <- c("aid","Study_country")

for_plotting <- select(data,aid,Study_country)
ctry <- bind_rows(ctry,for_plotting)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(regions), ncol=2)
rownames(country_count) <- regions$Study_country
colnames(country_count) <- c("Study_country", "counts")
#Calculate in for loop and write to blank matrix
for (c in regions$Study_country){
  subset <- filter(ctry, Study_country == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries_only <- inner_join(country_count,regions,by="Study_country")
countries_only <- filter(countries_only, Code != "")

countries_only$counts <- as.numeric(countries_only$counts)
countries_only <- as.data.frame(countries_only)
countries_zero <- filter(countries_only, counts == 0)

map <- readShapeSpatial(paste0(datadir,"TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"))
plot(map)
map <- fortify(map, region="ISO3")

pdf(file=paste0(datadir,"STE_Country_Map_dark_5_11.pdf"), width=16, height=8.5)
ggplot() +
  geom_map(data=countries_only, aes(map_id=Code, fill=counts),map=map) +
  geom_map(data=countries_zero, aes(map_id=Code),fill="#f0f0f0",map=map) +
  expand_limits(x=map$long,y=map$lat) +
  theme(panel.background = element_rect(fill = "darkgray", colour = "darkgray"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_gradient2(low="#f7fbff",mid="#6baed6",high="#08306b",midpoint=(max(countries_only$counts)/2),limits=c(0,max(countries_only$counts)))
dev.off()

# 
# 
# 
# # Summarise data by country (ignore warnings)
# ctry <- data %>% 
#   filter(exclude=='FALSE') %>% 
#   group_by(aid,Study_country) %>% 
#   summarise() %>% 
#   separate(Study_country,c('ctry1','ctry2'),sep=",",remove = F) %>% 
#   gather(key=ctry.col,value=ctry,ctry1:ctry2) %>% 
#   filter(!is.na(ctry) & !ctry%in%c(" United Republic Of"," Province Of China")) %>% 
#   mutate(ctry=gsub(" Philippines","Philippines",ctry),
#          ctry=gsub(" Mexico","Mexico",ctry),
#          iso3=countrycode(ctry, "country.name", "iso3c")) %>% 
#   group_by(ctry,iso3) %>% 
#   count() %>% 
#   ungroup() 
#   
# nrow(ctry)
# 
# pdf(file=paste0(datadir,"STE_Country_Map_dark_5_11.pdf"), width=16, height=8.5)
# ggplot() + 
#   geom_map(data=country, aes(map_id=code),fill="#f0f0f0",alpha=0.25,map=map) + 
#   geom_map(data=ctry, aes(map_id=iso3, fill=n),map=map) + 
#   expand_limits(x=map$long,y=map$lat) + 
#   theme(panel.background = element_rect(fill = "darkgray", colour = "darkgray"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   scale_fill_gradient2(low="#f7fbff",mid="#6baed6",high="#08306b",midpoint=(max(ctry$n)/2),limits=c(1,max(ctry$n)))
# dev.off()

########
## Trying with chord diagram instead

###########################
##FOR MPAS
###########################
mpa_outcomes <- filter(data,Int_type == "MPA") %>% distinct()
syn <- filter(mpa_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
tradeoff <- filter(mpa_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
both <- filter(mpa_outcomes,STE_domain == "Both Syn/Trd") %>% arrange(Out_type,aid)

#Create input for tradeoffs
aid <- as.list(unique(tradeoff$aid))

links <- matrix(nrow=1,ncol=3)
colnames(links) <- c("source","target","aid")
links <- as.data.frame(links)

for(i in aid){
  sub <- filter(tradeoff, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links <- bind_rows(links,sub2)
  } else 
    links
}

links <- slice(links,-1)

##Count links 
counts <- count(links,source,target)
colnames(counts) <- c("source","target","value") 
counts <- as.data.frame(counts)

##For synergies

aid2 <- as.list(unique(syn$aid))

links2 <- matrix(nrow=1,ncol=3)
colnames(links2) <- c("source","target","aid")
links2 <- as.data.frame(links2)

for(i in aid2){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links2 <- bind_rows(links2,sub2)
  } else 
    links2
}

links2 <- slice(links2,-1)

##Count links 
counts2 <- count(links2,source,target)
colnames(counts2) <- c("source","target","value") 
counts2 <- as.data.frame(counts2)

#Create input for both
aid3 <- as.list(unique(both$aid))

links3 <- matrix(nrow=1,ncol=3)
colnames(links3) <- c("source","target","aid")
links3 <- as.data.frame(links3)

for(i in aid3){
  sub <- filter(both, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links3 <- bind_rows(links3,sub2)
  } else 
    links3
}

links3 <- slice(links3,-1)

##Count links 
counts3 <- count(links3,source,target)
colnames(counts3) <- c("source","target","value") 
counts3 <- as.data.frame(counts3)

grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
pdf(file=paste0(datadir,"STE by Domain - MPA_4_24.pdf"),width=11,height=8.5)
par(mfrow=c(1,2),oma=c(0,0,2,0))
chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
title("Tradeoffs (n=16 articles, x=46 cases)",line=-3.5)
chordDiagram(as.data.frame(counts3), transparency = 0.3, grid.col=grid.col)
title("Both (n=16 articles, x=46 cases)",line=-3.5)
chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
title("Synergies (n=26 articles, x=70 cases)",line=-3.5)
mtext("STE by outcome domain type in MPAs",outer=TRUE,cex=1.5)
dev.off()

###########################
##for CBNRM
###########################
cbnrm_outcomes <- filter(data,Int_type %in% c("CBRNM_MPA","CBNRM")) %>% distinct()
syn <- filter(cbnrm_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
tradeoff <- filter(cbnrm_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
both <- filter(cbnrm_outcomes,STE_domain == "Both Syn/Trd")

#Create input for tradeoffs
aid <- as.list(unique(tradeoff$aid))

links <- matrix(nrow=1,ncol=3)
colnames(links) <- c("source","target","aid")
links <- as.data.frame(links)

for(i in aid){
  sub <- filter(tradeoff, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links <- bind_rows(links,sub2)
  } else 
    links
}

links <- slice(links,-1)

##Count links 
counts <- count(links,source,target)
colnames(counts) <- c("source","target","value") 
counts <- as.data.frame(counts)

##For synergies

aid2 <- as.list(unique(syn$aid))

links2 <- matrix(nrow=1,ncol=3)
colnames(links2) <- c("source","target","aid")
links2 <- as.data.frame(links2)

for(i in aid2){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links2 <- bind_rows(links2,sub2)
  } else 
    links2
}

links2 <- slice(links2,-1)

##Count links 
counts2 <- count(links2,source,target)
colnames(counts2) <- c("source","target","value") 
counts2 <- as.data.frame(counts2)

##For both

aid3 <- as.list(unique(syn$aid))

links3 <- matrix(nrow=1,ncol=3)
colnames(links3) <- c("source","target","aid")
links3 <- as.data.frame(links3)

for(i in aid3){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub3 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub3) <- c("source","target")
    sub3$aid <- c(i)
    links3 <- bind_rows(links3,sub3)
  } else 
    links3
}

links3 <- slice(links3,-1)

##Count links 
counts3 <- count(links3,source,target)
colnames(counts3) <- c("source","target","value") 
counts3 <- as.data.frame(counts3)

grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
pdf(file=paste0(datadir,"STE by Domain - CBNRM_4_24.pdf"),width=11,height=8.5)
par(mfrow=c(1,2),oma=c(0,0,2,0))
chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
title("Tradeoffs (n=8 articles, x=18 cases)",line=-3.5)
chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
title("Synergies (n=8 articles, x=21 cases)",line=-3.5)
chordDiagram(as.data.frame(counts3), transparency = 0.3, grid.col=grid.col)
title("Both (n=8 articles, x=21 cases)",line=-3.5)
mtext("STE by outcome domain type in CBNRM",outer=TRUE,cex=1.5)
dev.off()

##########
#Treemapping to look at outcome subtypes within outcome domains
outcomes <- select(data,aid,oid,Int_type,Out_type,Out_subtype)
foo <- data.frame(do.call('rbind', strsplit(as.character(outcomes$Out_subtype),', ',fixed=TRUE)))
outcomes <- bind_cols(outcomes,foo)
outcomes <- select(outcomes,-Out_subtype)
outcomes2 <- gather(outcomes,"X","Out_subtype",5:8)
outcomes2 <- select(outcomes2,-X) %>% distinct()
mpa_out <- filter(outcomes2,Int_type == "MPA") %>% select(oid,Out_type,Out_subtype)
cbnrm_out <- filter(outcomes2,Int_type == "CBNRM")
cbnrm_mpa_out <- filter(outcomes2,Int_type == "CBNRM_MPA")
mpa_count <- count(mpa_out,Out_type,Out_subtype)
cbnrm_count <- count(cbnrm_out,Out_type,Out_subtype)
cbnrm_mpa_count <- count(cbnrm_mpa_out,Out_type,Out_subtype)

pdf(file=paste0(datadir,"Outcome_attr_by_domain_MPA.pdf"))
treemap(mpa_count,index=c("Out_type","Out_subtype"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="MPAs")
dev.off()
pdf(file=paste0(datadir,"Outcome_attr_by_domain_CBNRM.pdf"))
treemap(cbnrm_count,index=c("Out_type","Out_subtype"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="CBNRM")
dev.off()

pdf(file=paste0(datadir,"Outcome_attr_by_domain_CBNRM_MPA.pdf"))
treemap(cbnrm_mpa_count,index=c("Out_type","Out_subtype"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="CBNRM_MPA")
dev.off()

# ###########################################################
# ##d3 Force network - node colors indicate outcome domain, each node is an individual outcome
# 
# #Filter dataset to those that only look at STE by domain
# subset <- filter(data, STE_domain != "NA") %>% distinct()
# mpa_outcomes <- filter(subset, Int_type == "MPA") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# cbnrm_outcomes <- filter(subset, Int_type == "CBNRM") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# cert_outcomes <- filter(subset, Int_type == "Certification") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# #Create input
# aid <- as.list(unique(mpa_outcomes$aid))
# 
# links <- matrix(nrow=1,ncol=3)
# colnames(links) <- c("source","target","aid")
# links <- as.data.frame(links)
# 
# for(i in aid){
#   sub <- filter(mpa_outcomes, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links <- bind_rows(links,sub2)
#   } else 
#     links
# }
# 
# links <- slice(links,-1)
# 
# ##Count links 
# counts <- count(links,source,target)
# colnames(counts) <- c("source","target","value") 
# counts <- as.data.frame(counts)
# counts$source=as.numeric(as.factor(counts$source))-1
# counts$target=as.numeric(as.factor(counts$target))-1
# ##Define node IDs
# # nodes <- data.frame(name=out_type,group=c(1:6))
# # nodes <- as.data.frame(nodes)
# 
# nodes <- select(mpa_outcomes,aid,Out_type)
# nodes <- as.data.frame(nodes)
# colnames(nodes) <- c("name","group")
# 
# #Simple network
# simple <- select(links,-aid)
# d3SimpleNetwork(simple, file="SimpleNetwork.html")
# 
# #Force network
# d3ForceNetwork(Links=counts, Nodes=nodes, Source="source",Target="target",Value="value",NodeID="name",Group="group",opacity=0.9,file="ForceNetwork.html")

###########################
##for Certification
###########################
# syn <- filter(cert_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
# tradeoff <- filter(cert_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
# pos_neu <- filter(cert_outcomes,STE_domain == "Pos/neutral")
# 
# #Create input for tradeoffs
# aid <- as.list(unique(tradeoff$aid))
# 
# links <- matrix(nrow=1,ncol=3)
# colnames(links) <- c("source","target","aid")
# links <- as.data.frame(links)
# 
# for(i in aid){
#   sub <- filter(tradeoff, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links <- bind_rows(links,sub2)
#   } else 
#     links
# }
# 
# links <- slice(links,-1)
# 
# ##Count links 
# counts <- count(links,source,target)
# colnames(counts) <- c("source","target","value") 
# counts <- as.data.frame(counts)
# 
# ##For synergies
# 
# aid2 <- as.list(unique(syn$aid))
# 
# links2 <- matrix(nrow=1,ncol=3)
# colnames(links2) <- c("source","target","aid")
# links2 <- as.data.frame(links2)
# 
# for(i in aid2){
#   sub <- filter(syn, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links2 <- bind_rows(links2,sub2)
#   } else 
#     links2
# }
# 
# links2 <- slice(links2,-1)
# 
# ##Count links 
# counts2 <- count(links2,source,target)
# colnames(counts2) <- c("source","target","value") 
# counts2 <- as.data.frame(counts2)
# 
# grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
# pdf(file="STE by Domain - Cert_4_24.pdf",width=11,height=8.5)
# par(mfrow=c(1,2),oma=c(0,0,2,0))
# chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
# title("Tradeoffs (n=1 articles, x=2 cases)",line=-3.5)
# chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
# title("Synergies (n=2 articles, x=4 cases)",line=-3.5)
# mtext("STE by outcome domain type in Certification",outer=TRUE,cex=1.5)
# dev.off()