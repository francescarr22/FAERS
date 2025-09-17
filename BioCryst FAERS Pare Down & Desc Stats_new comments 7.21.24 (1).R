#### BioCryst FAERS Data Pare Down & Analyses 2014-2024Q1 ####

#Packages needed:
# - Amelia
# - arsenal
# - dplyr
# - EcotoneFinder
# - faersquarterlydata
# - ggcorrplot
# - ggplot2
# - grid
# - plyr
# - pscl
# - questionr
# - readxl
# - stringr
# - summarytools
# - tidyr
# - vcd


### All years tables ###

## Drug ##

#Add report year variable#
drug2014$rpt_year <- 2014
drug2015$rpt_year <- 2015
drug2016$rpt_year <- 2016
drug2017$rpt_year <- 2017
drug2018$rpt_year <- 2018
drug2019$rpt_year <- 2019
drug2020$rpt_year <- 2020
drug2021$rpt_year <- 2021
drug2022$rpt_year <- 2022
drug2023$rpt_year <- 2023
drug24q1$rpt_year <- 2024

#Only drugs listed as primary suspect#
drug2014_ps <- subset(drug2014, drug2014$role_cod == "PS")
drug2015_ps <- subset(drug2015, drug2015$role_cod == "PS")
drug2016_ps <- subset(drug2016, drug2016$role_cod == "PS")
drug2017_ps <- subset(drug2017, drug2017$role_cod == "PS")
drug2018_ps <- subset(drug2018, drug2018$role_cod == "PS")
drug2019_ps <- subset(drug2019, drug2019$role_cod == "PS")
drug2020_ps <- subset(drug2020, drug2020$role_cod == "PS")
drug2021_ps <- subset(drug2021, drug2021$role_cod == "PS")
drug2022_ps <- subset(drug2022, drug2022$role_cod == "PS")
drug2023_ps <- subset(drug2023, drug2023$role_cod == "PS")
drug24q1_ps <- subset(drug24q1, drug24q1$role_cod == "PS")


drug_all_ps <-rbind(drug2014_ps,drug2015_ps,drug2016_ps,drug2017_ps,drug2018_ps,drug2019_ps,drug2020_ps,drug2021_ps,drug2022_ps,drug2023_ps,drug24q1_ps)
View(drug_all_ps)

freq(drug_all_ps$drugname)

#Confirm there are no dups#
drug_all_ps %>% select("primaryid") %>% n_distinct()

#Only Drugs of Interest (two ways to pull to confirm all rows were included)#
drug_all_ps_doi <- subset(drug_all_ps, drug_all_ps$drugname %in% c("orladeyo", "Orladeyo", "ORLADEYO", "takhzyro", "Takhzyro", "TAKHZYRO", 
                                                                    "haegarda", "Haegarda", "HAEGARDA", "cinryze", "Cinryze", "CINRYZE", 
                                                                    "berotralstat", "Berotralstat", "BEROTRALSTAT", "lanadelumab", "Lanadelumab", 
                                                                    "LANADELUMAB", "lanadelumab-flyo", "Lanadelumab-flyo", "LANADELUMAB-FLYO", 
                                                                    "HUMAN C1-ESTERASE INHIBITOR",  "C1 ESTERASE INHIBITOR (HUMAN)"))

View(drug_all_ps_doi)

drug_all_ps_doi2 <- drug_all_ps %>% filter(str_detect(drug_all_ps$drugname, 'orladayo|Orladeyo|ORLADEYO|takhzyro|Takhzyro|TAKHZYRO|haegarda|Haegarda|HAEGARDA|
                                                                      cinryze|Cinryze|CINRYZE|berotralstat|Berotralstat|BEROTRALSTAT|lanadelumab|Lanadelumab| 
                                                                      LANADELUMAB|Esterase|esterase|ESTERASE|inhibitor|Inhibitor|INHIBITOR (HUMAN)'))
View(drug_all_ps_doi2)

unique(drug_all_ps_doi$drugname)
unique(drug_all_ps_doi2$drugname)

freq(drug_all_ps_doi2$drugname)

#Remove drug not of interest#
drug_all_ps_doi2 <- subset(drug_all_ps_doi2, drug_all_ps_doi2$drugname != ".ALPHA.1-PROTEINASE INHIBITOR HUMAN")
View(drug_all_ps_doi2)

#Combined both data pulls and remove dups#
drug_all_ps_doi3 <- rbind(drug_all_ps_doi,drug_all_ps_doi2)
View(drug_all_ps_doi3)

summary(duplicated(drug_all_ps_doi3))

drug_all_ps_doi3 <- distinct(drug_all_ps_doi3)

#Confirm DOI after merge#
unique(drug_all_ps_doi3$drugname)


#write.csv(drug_all_ps_doi3, "Drug DOI.csv")

## Therapy ##

ther_all <- rbind(ther2014, ther2015, ther2016, ther2017, ther2018, ther2019, ther2020, ther2021, ther2022, ther2023, ther24q1)
View(ther_all)

## Indication ##

indi_all <- rbind(indi2014, indi2015, indi2016, indi2017, indi2018, indi2019, indi2020, indi2021, indi2022, indi2023, indi24q1)
View(indi_all)

##Merge Drug with Ther & Indi##

drug_merge_ther <- merge(drug_all_ps_doi3, ther_all, by.x = c("primaryid", "drug_seq"), by.y = c("primaryid", "dsg_drug_seq"), all.x = TRUE, all.y = FALSE)
View(drug_merge_ther)

drug_merge <- merge(drug_merge_ther, indi_all, by.x = c("primaryid", "drug_seq"), by.y = c("primaryid", "indi_drug_seq"), all.x = TRUE, all.y = FALSE)
View(drug_merge)

summary(duplicated(drug_merge))

#write.csv(drug_merge, "Drug DOI_Therapy_Indication 6.30.24.csv")

#drug_all2 <- read.csv("Drug DOI_Therapy_Indication.csv")
#View(drug_all2)


## Demographic ##

#Create age_yr variable for consistency#
demo2014$age_yr <- ifelse(demo2014$age_cod == "DEC", demo2014$age*10,
                          ifelse(demo2014$age_cod == "DY", demo2014$age/365,
                                 ifelse(demo2014$age_cod == "HR", demo2014$age/(24*365),
                                        ifelse(demo2014$age_cod == "MON", demo2014$age/12,
                                               ifelse(demo2014$age_cod == "WK", demo2014$age/52,
                                                      ifelse(demo2014$age_cod == "YR", demo2014$age,NA))))))

demo2015$age_yr <- ifelse(demo2015$age_cod == "DEC", demo2015$age*10,
                          ifelse(demo2015$age_cod == "DY", demo2015$age/365,
                                 ifelse(demo2015$age_cod == "HR", demo2015$age/(24*365),
                                        ifelse(demo2015$age_cod == "MON", demo2015$age/12,
                                               ifelse(demo2015$age_cod == "WK", demo2015$age/52,
                                                      ifelse(demo2015$age_cod == "YR", demo2015$age,NA))))))

demo2016$age_yr <- ifelse(demo2016$age_cod == "DEC", demo2016$age*10,
                          ifelse(demo2016$age_cod == "DY", demo2016$age/365,
                                 ifelse(demo2016$age_cod == "HR", demo2016$age/(24*365),
                                        ifelse(demo2016$age_cod == "MON", demo2016$age/12,
                                               ifelse(demo2016$age_cod == "WK", demo2016$age/52,
                                                      ifelse(demo2016$age_cod == "YR", demo2016$age,NA))))))

demo2017$age_yr <- ifelse(demo2017$age_cod == "DEC", demo2017$age*10,
                          ifelse(demo2017$age_cod == "DY", demo2017$age/365,
                                 ifelse(demo2017$age_cod == "HR", demo2017$age/(24*365),
                                        ifelse(demo2017$age_cod == "MON", demo2017$age/12,
                                               ifelse(demo2017$age_cod == "WK", demo2017$age/52,
                                                      ifelse(demo2017$age_cod == "YR", demo2017$age,NA))))))

demo2018$age_yr <- ifelse(demo2018$age_cod == "DEC", demo2018$age*10,
                          ifelse(demo2018$age_cod == "DY", demo2018$age/365,
                                 ifelse(demo2018$age_cod == "HR", demo2018$age/(24*365),
                                        ifelse(demo2018$age_cod == "MON", demo2018$age/12,
                                               ifelse(demo2018$age_cod == "WK", demo2018$age/52,
                                                      ifelse(demo2018$age_cod == "YR", demo2018$age,NA))))))

demo2019$age_yr <- ifelse(demo2019$age_cod == "DEC", demo2019$age*10,
                          ifelse(demo2019$age_cod == "DY", demo2019$age/365,
                                 ifelse(demo2019$age_cod == "HR", demo2019$age/(24*365),
                                        ifelse(demo2019$age_cod == "MON", demo2019$age/12,
                                               ifelse(demo2019$age_cod == "WK", demo2019$age/52,
                                                      ifelse(demo2019$age_cod == "YR", demo2019$age,NA))))))

demo2020$age_yr <- ifelse(demo2020$age_cod == "DEC", demo2020$age*10,
                          ifelse(demo2020$age_cod == "DY", demo2020$age/365,
                                 ifelse(demo2020$age_cod == "HR", demo2020$age/(24*365),
                                        ifelse(demo2020$age_cod == "MON", demo2020$age/12,
                                               ifelse(demo2020$age_cod == "WK", demo2020$age/52,
                                                      ifelse(demo2020$age_cod == "YR", demo2020$age,NA))))))

demo2021$age_yr <- ifelse(demo2021$age_cod == "DEC", demo2021$age*10,
                          ifelse(demo2021$age_cod == "DY", demo2021$age/365,
                                 ifelse(demo2021$age_cod == "HR", demo2021$age/(24*365),
                                        ifelse(demo2021$age_cod == "MON", demo2021$age/12,
                                               ifelse(demo2021$age_cod == "WK", demo2021$age/52,
                                                      ifelse(demo2021$age_cod == "YR", demo2021$age,NA))))))

demo2022$age_yr <- ifelse(demo2022$age_cod == "DEC", demo2022$age*10,
                          ifelse(demo2022$age_cod == "DY", demo2022$age/365,
                                 ifelse(demo2022$age_cod == "HR", demo2022$age/(24*365),
                                        ifelse(demo2022$age_cod == "MON", demo2022$age/12,
                                               ifelse(demo2022$age_cod == "WK", demo2022$age/52,
                                                      ifelse(demo2022$age_cod == "YR", demo2022$age,NA))))))

demo2023$age_yr <- ifelse(demo2023$age_cod == "DEC", demo2023$age*10,
                          ifelse(demo2023$age_cod == "DY", demo2023$age/365,
                                 ifelse(demo2023$age_cod == "HR", demo2023$age/(24*365),
                                        ifelse(demo2023$age_cod == "MON", demo2023$age/12,
                                               ifelse(demo2023$age_cod == "WK", demo2023$age/52,
                                                      ifelse(demo2023$age_cod == "YR", demo2023$age,NA))))))

demo24q1$age_yr <- ifelse(demo24q1$age_cod == "DEC", demo24q1$age*10,
                          ifelse(demo24q1$age_cod == "DY", demo24q1$age/365,
                                 ifelse(demo24q1$age_cod == "HR", demo24q1$age/(24*365),
                                        ifelse(demo24q1$age_cod == "MON", demo24q1$age/12,
                                               ifelse(demo24q1$age_cod == "WK", demo24q1$age/52,
                                                      ifelse(demo24q1$age_cod == "YR", demo24q1$age,NA))))))

#Create wt_lbs variable for consistency#
demo2014$wt <-as.numeric(demo2014$wt)

demo2014$wt_lbs <- ifelse(demo2014$wt_cod == "KG", demo2014$wt*2.20462,
                          ifelse(demo2014$wt_code == "LBS", demo2014$wt, NA))

demo2015$wt_lbs <- ifelse(demo2015$wt_cod == "KG", demo2015$wt*2.20462,
                          ifelse(demo2015$wt_code == "LBS", demo2015$wt, NA))

demo2016$wt_lbs <- ifelse(demo2016$wt_cod == "KG", demo2016$wt*2.20462,
                          ifelse(demo2016$wt_code == "LBS", demo2016$wt, NA))

demo2017$wt_lbs <- ifelse(demo2017$wt_cod == "KG", demo2017$wt*2.20462,
                          ifelse(demo2017$wt_code == "LBS", demo2017$wt, NA))

demo2018$wt_lbs <- ifelse(demo2018$wt_cod == "KG", demo2018$wt*2.20462,
                          ifelse(demo2018$wt_code == "LBS", demo2018$wt, NA))

demo2019$wt_lbs <- ifelse(demo2019$wt_cod == "KG", demo2019$wt*2.20462,
                          ifelse(demo2019$wt_code == "LBS", demo2019$wt, NA))

demo2020$wt_lbs <- ifelse(demo2020$wt_cod == "KG", demo2020$wt*2.20462,
                          ifelse(demo2020$wt_code == "LBS", demo2020$wt, NA))

demo2021$wt_lbs <- ifelse(demo2021$wt_cod == "KG", demo2021$wt*2.20462,
                          ifelse(demo2021$wt_code == "LBS", demo2021$wt, NA))

demo2022$wt_lbs <- ifelse(demo2022$wt_cod == "KG", demo2022$wt*2.20462,
                          ifelse(demo2022$wt_code == "LBS", demo2022$wt, NA))

demo2023$wt_lbs <- ifelse(demo2023$wt_cod == "KG", demo2023$wt*2.20462,
                          ifelse(demo2023$wt_code == "LBS", demo2023$wt, NA))

demo24q1$wt_lbs <- ifelse(demo24q1$wt_cod == "KG", demo24q1$wt*2.20462,
                          ifelse(demo24q1$wt_code == "LBS", demo24q1$wt, NA))


demo_all <-rbind(demo2014, demo2015, demo2016, demo2017, demo2018, demo2019, demo2020,
                        demo2021, demo2022, demo2023, demo24q1)
View(demo_all)

#NAs for age <0 or >100 & weight <=0 & >600#
demo_all$age_yr[demo_all$age_yr > 100 | demo_all$age_yr < 0] <- NA
demo_all$wt_lbs[demo_all$wt_lbs > 600 | demo_all$wt_lbs <= 0] <- NA


#Left join to drug_all#

demo_drug <- drug_merge %>% left_join(demo_all, by=c("primaryid"))
View(demo_drug)

## Reaction ##

reac_all <- rbind(reac2014, reac2015, reac2016, reac2017, reac2018, reac2019, reac2020, reac2021, reac2022, reac2023, reac24q1)
View(reac_all)

## Outcome ##

outc_all <- rbind(outc2014, outc2015, outc2016, outc2017, outc2018, outc2019, outc2020, outc2021, outc2022, outc2023, outc24q1)
View(outc_all)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
# ##Creation of analytic file before ROI pare down##
# #Merge demo_drug with Reac & Outc#
# 
# all_merge <- demo_drug %>% left_join(reac_all, by=c("primaryid")) %>% left_join(outc_all,by=c("primaryid"))
# View(all_merge)
# 
# colnames(all_merge)
# n_distinct(all_merge$primaryid)
# 
# summary(duplicated(all_merge))
# 
# all_merge_dis <- distinct(all_merge)
# View(all_merge_dis)
# 
# 
# # Arrange Dates #
# all_merge_dis$exp_dt <- arrange_date(all_merge_dis$exp_dt)
# all_merge_dis$mfr_dt <- arrange_date(all_merge_dis$mfr_dt)
# all_merge_dis$start_dt <- arrange_date(all_merge_dis$start_dt)
# all_merge_dis$event_dt <- arrange_date(all_merge_dis$event_dt)
# all_merge_dis$init_fda_dt <- arrange_date(all_merge_dis$init_fda_dt)
# all_merge_dis$fda_dt <- arrange_date(all_merge_dis$fda_dt)
# all_merge_dis$rept_dt <- arrange_date(all_merge_dis$rept_dt)
# 
# 
# freq(drug_merge$indi_pt)
# 
# freq(all_merge_dis$drugname)
# 
# #Create variable for DOI groups#
# all_merge_dis$doi_group <- ifelse(all_merge_dis$drugname %in% c("BEROTRALSTAT", "ORLADEYO"), "Berotralstat",
#                               ifelse((str_detect(all_merge_dis$drugname, 'cinryze|Cinryze|CINRYZE')), "Cinryze",
#                                      ifelse(all_merge_dis$drugname == c("HAEGARDA"), "Haegarda",
#                                             ifelse(all_merge_dis$drugname %in% c("BEROTRALSTAT", "ORLADEYO"), "Berotralstat",
#                                                    ifelse((str_detect(all_merge_dis$drugname, 'lanadelumab|Lanadelumab|LANADELUMAB|TAKHZYRO')), "Lanadelumab", "C1 Esterase Inhibitor (NS)")))))
# 
# freq(all_merge_dis$doi_group)
# 
# #Blanks to NAs#
# all_merge_dis[all_merge_dis == ""] <- NA
# 
# #Write csv for all combined data (therapy & report sources not included - not needed for analyses)#
# write.csv(all_merge_dis, "Initial Analytic Dataset_6.30.24.csv")
# View(all_merge_dis)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


#Start to pare down for Table 1 counts#
drug_demo <- merge(drug_all_ps_doi3, demo_all, by=c("primaryid"), all.x = T, all.y = F)
View(drug_demo)


drug_demo$rpt_year <- as.character(drug_demo$rpt_year)

drug_demo$doi_group <- ifelse(drug_demo$drugname %in% c("BEROTRALSTAT", "ORLADEYO"), "Berotralstat",
                                  ifelse((str_detect(drug_demo$drugname, 'cinryze|Cinryze|CINRYZE')), "Cinryze",
                                         ifelse(drug_demo$drugname == c("HAEGARDA"), "Haegarda",
                                                ifelse(drug_demo$drugname %in% c("BEROTRALSTAT", "ORLADEYO"), "Berotralstat",
                                                       ifelse((str_detect(drug_demo$drugname, 'lanadelumab|Lanadelumab|LANADELUMAB|TAKHZYRO')), "Lanadelumab", "C1 Esterase Inhibitor (NS)")))))

freq(drug_demo$doi_group)


drug_demo[drug_demo == ""] <- NA


# Arrange Dates #
drug_demo$exp_dt <- arrange_date(drug_demo$exp_dt)
drug_demo$mfr_dt <- arrange_date(drug_demo$mfr_dt)
drug_demo$init_fda_dt <- arrange_date(drug_demo$init_fda_dt)
drug_demo$fda_dt <- arrange_date(drug_demo$fda_dt)
drug_demo$rept_dt <- arrange_date(drug_demo$rept_dt)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
##Test Tables Before Reaction Pare Down##
# table1a <- tableby(doi_group ~ rpt_year + age_grp + sex + i_f_code + occp_cod + reporter_country, data=drug_demo)
# summary(table1a)
# 
# table1b <- tableby(doi_group ~ age_yr + wt_lbs, data=drug_demo)
# summary(table1b)
# 
# drug_demo_indi <- merge(drug_demo, indi_all, by.x=c("primaryid", "drug_seq"), by.y=c("primaryid", "indi_drug_seq"), all.x=T, all.y = F)
# View(drug_demo_indi)
# 
# write.csv(freq(drug_demo_indi$indi_pt), "Indications Total.csv")
# 
# drug_demo_outc <- merge(drug_demo, outc_all, by.x=c("primaryid"), by.y=c("primaryid"), all.x=T, all.y = F)
# View(drug_demo_outc)
# 
# table1c <- tableby(doi_group ~ outc_cod, data=drug_demo_outc)
# summary(table1c)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


#Create output table to assess all reactions for DOI#

drug_demo_reac <- merge(drug_demo, reac_all, by.x=c("primaryid"), by.y=c("primaryid"), all.x=T, all.y = F)
View(drug_demo_reac)

#Output table of all reactions for DOI#
write.csv(freq(drug_demo_reac$pt), "Reactions Total.csv")

#Create table with only reactions of interest#
drug_demo_roi <- subset(drug_demo_reac, drug_demo_reac$pt %in% c("Hereditary angioedema", "Hereditary angioedema With C1 esterase inhibitor deficiency",
                                                    "Hereditary angioedema With normal C1 esterase inhibitor", "Angioedema", "Idiopathic angioedema",
                                                    
                                                    "Condition aggravated", "Decreased Activity", "Drug effect less than expected", "Drug ineffective",
                                                    "Drug ineffective for unapproved indication", "Drug intolerance", "Drug tolerance", "Drug tolerance increased", 
                                                    "Rebound effect", "Therapeutic product effect decreased", "Therapeutic product effect delayed", 
                                                    "Therapeutic product effect incomplete", "Therapeutic product ineffective", "Therapeutic response decreased", 
                                                    "Therapeutic response shortened", "Therapeutic response unexpected", "Therapy non-responder", 
                                                    "Therapy partial responder", "Treatment failure", "Treatment noncompliance", "Drug effect decreased", 
                                                    "Drug effect delayed", "Drug effect incomplete",
                                                    
                                                    "Brain oedema", "Breast swelling", "Choking", "Circumoral oedema", "Circumoral swelling", "Dysphagia", 
                                                    "Ear swelling", "Eye swelling", "Eyelid oedema", "Face oedema", "Gastrointestinal oedema", "Generalised oedema", 
                                                    "Genital swelling", "Gingival swelling", "Intestinal swelling", "Joint swelling", "Laryngeal obstruction", 
                                                    "Laryngeal oedema", "Lip oedema", "Lip swelling", "Localised oedema", "Lymhoedema", "Macular oedema", 
                                                    "Mouth swelling", "Mycoardial oedema", "Nasal oedema", "Non-pitting oedema", "Oedema", "Oedema genital",
                                                    "Oedema peripheral", "Oesophageal oedema", "Oropharyngeal discomfort", "Oropharyngeal pain", "Oropharyngeal swelling", 
                                                    "Palatal swelling", "Papilloedema", "Penile swelling", "Periorbital oedema", "Periorbital swelling", "Peripheral swelling", 
                                                    "Pharyngeal oedema", "Pharyngeal swelling", "Post procedural swelling", "Pulmonary oedema", "Respiratory tract oedema", 
                                                    "Scrotal swelling", "Skin oedema", "Skin swelling", "Swelling", "Swelling face", "Swelling of eyelid", "Swollen tongue", 
                                                    "Testicular swelling", "Throat tightness", "Tongue oedema", "Upper airway obstruction", "Vulvovaginal swelling", 
                                                    "Administration site swelling", "Catheter site swelling", "Tracheal obstruction", "Oedema mouth", "Local swelling", 
                                                    "Incision site swelling", "Infusion site oedema", "Infusion site swelling", "Injection site oedema", "Injection site swelling", 
                                                    "Abdominal distension", "Abdominal pain", "Abdominal pain upper", "Abdominal discomfort", "Intestinal angioedema"))
View(drug_demo_roi)

#Confirm total number of reports#
drug_demo_roi$primaryid <- as.character(drug_demo_roi$primaryid)
summary(unique(drug_demo_roi$primaryid))

drug_demo_roi_dis <- distinct(drug_demo_roi)
View(drug_demo_roi_dis)

#Create variable to group ROIs#
drug_demo_roi$roi_group <- ifelse(drug_demo_roi$pt %in% c("Hereditary angioedema", "Hereditary angioedema With C1 esterase inhibitor deficiency",
                                                          "Hereditary angioedema With normal C1 esterase inhibitor", "Angioedema", "Idiopathic angioedema"), "Angioedema",
                              ifelse(drug_demo_roi$pt %in%  c("Condition aggravated", "Decreased Activity", "Drug effect less than expected", "Drug ineffective",
                                                             "Drug ineffective for unapproved indication", "Drug intolerance", "Drug tolerance", "Drug tolerance increased", 
                                                             "Rebound effect", "Therapeutic product effect decreased", "Therapeutic product effect delayed", 
                                                             "Therapeutic product effect incomplete", "Therapeutic product ineffective", "Therapeutic response decreased", 
                                                             "Therapeutic response shortened", "Therapeutic response unexpected", "Therapy non-responder", 
                                                             "Therapy partial responder", "Treatment failure", "Treatment noncompliance", "Drug effect decreased", 
                                                             "Drug effect delayed", "Drug effect incomplete"), "Lack of Efficacy",
                                     ifelse(drug_demo_roi$pt %in% c("Brain oedema", "Breast swelling", "Choking", "Circumoral oedema", "Circumoral swelling", "Dysphagia", 
                                                    "Ear swelling", "Eye swelling", "Eyelid oedema", "Face oedema", "Gastrointestinal oedema", "Generalised oedema", 
                                                    "Genital swelling", "Gingival swelling", "Intestinal swelling", "Joint swelling", "Laryngeal obstruction", 
                                                    "Laryngeal oedema", "Lip oedema", "Lip swelling", "Localised oedema", "Lymhoedema", "Macular oedema", 
                                                    "Mouth swelling", "Mycoardial oedema", "Nasal oedema", "Non-pitting oedema", "Oedema", "Oedema genital",
                                                    "Oedema peripheral", "Oesophageal oedema", "Oropharyngeal discomfort", "Oropharyngeal pain", "Oropharyngeal swelling", 
                                                    "Palatal swelling", "Papilloedema", "Penile swelling", "Periorbital oedema", "Periorbital swelling", "Peripheral swelling", 
                                                    "Pharyngeal oedema", "Pharyngeal swelling", "Post procedural swelling", "Pulmonary oedema", "Respiratory tract oedema", 
                                                    "Scrotal swelling", "Skin oedema", "Skin swelling", "Swelling", "Swelling face", "Swelling of eyelid", "Swollen tongue", 
                                                    "Testicular swelling", "Throat tightness", "Tongue oedema", "Upper airway obstruction", "Vulvovaginal swelling", 
                                                    "Administration site swelling", "Catheter site swelling", "Tracheal obstruction", "Oedema mouth", "Local swelling", 
                                                    "Incision site swelling", "Infusion site oedema", "Infusion site swelling", "Injection site oedema", "Injection site swelling", 
                                                    "Abdominal distension", "Abdominal pain", "Abdominal pain upper", "Abdominal discomfort", "Intestinal angioedema"), "Swelling", NA)))

drug_demo_roi_dis$roi_group <- ifelse(drug_demo_roi_dis$pt %in% c("Hereditary angioedema", "Hereditary angioedema With C1 esterase inhibitor deficiency",
                                                          "Hereditary angioedema With normal C1 esterase inhibitor", "Angioedema", "Idiopathic angioedema"), "Angioedema",
                                  ifelse(drug_demo_roi$pt %in%  c("Condition aggravated", "Decreased Activity", "Drug effect less than expected", "Drug ineffective",
                                                                  "Drug ineffective for unapproved indication", "Drug intolerance", "Drug tolerance", "Drug tolerance increased", 
                                                                  "Rebound effect", "Therapeutic product effect decreased", "Therapeutic product effect delayed", 
                                                                  "Therapeutic product effect incomplete", "Therapeutic product ineffective", "Therapeutic response decreased", 
                                                                  "Therapeutic response shortened", "Therapeutic response unexpected", "Therapy non-responder", 
                                                                  "Therapy partial responder", "Treatment failure", "Treatment noncompliance", "Drug effect decreased", 
                                                                  "Drug effect delayed", "Drug effect incomplete"), "Lack of Efficacy",
                                         ifelse(drug_demo_roi$pt %in% c("Brain oedema", "Breast swelling", "Choking", "Circumoral oedema", "Circumoral swelling", "Dysphagia", 
                                                                        "Ear swelling", "Eye swelling", "Eyelid oedema", "Face oedema", "Gastrointestinal oedema", "Generalised oedema", 
                                                                        "Genital swelling", "Gingival swelling", "Intestinal swelling", "Joint swelling", "Laryngeal obstruction", 
                                                                        "Laryngeal oedema", "Lip oedema", "Lip swelling", "Localised oedema", "Lymhoedema", "Macular oedema", 
                                                                        "Mouth swelling", "Mycoardial oedema", "Nasal oedema", "Non-pitting oedema", "Oedema", "Oedema genital",
                                                                        "Oedema peripheral", "Oesophageal oedema", "Oropharyngeal discomfort", "Oropharyngeal pain", "Oropharyngeal swelling", 
                                                                        "Palatal swelling", "Papilloedema", "Penile swelling", "Periorbital oedema", "Periorbital swelling", "Peripheral swelling", 
                                                                        "Pharyngeal oedema", "Pharyngeal swelling", "Post procedural swelling", "Pulmonary oedema", "Respiratory tract oedema", 
                                                                        "Scrotal swelling", "Skin oedema", "Skin swelling", "Swelling", "Swelling face", "Swelling of eyelid", "Swollen tongue", 
                                                                        "Testicular swelling", "Throat tightness", "Tongue oedema", "Upper airway obstruction", "Vulvovaginal swelling", 
                                                                        "Administration site swelling", "Catheter site swelling", "Tracheal obstruction", "Oedema mouth", "Local swelling", 
                                                                        "Incision site swelling", "Infusion site oedema", "Infusion site swelling", "Injection site oedema", "Injection site swelling", 
                                                                        "Abdominal distension", "Abdominal pain", "Abdominal pain upper", "Abdominal discomfort", "Intestinal angioedema"), "Swelling", NA)))

freq(drug_demo_roi)
freq(drug_demo_roi_dis)


#Reactions of interest group by actual reaction for each DOI#
table(drug_demo_roi$pt, drug_demo_roi$roi_group, drug_demo_roi$doi_group)

#Confirming same count of reports#
summary(unique(drug_demo_roi$primaryid))


## Table 1 - Reaction Group counts by DOI ##
table1_reac <- tableby(doi_group ~ roi_group, data=drug_demo_roi_dis)
summary(table1_reac)

#Roll up reactions to one row per ID#
drug_demo_roi_id <- pivot_wider(data=drug_demo_roi, id_cols = "primaryid", names_from = "roi_group", values_from = "pt")
View(drug_demo_roi_id)

#Adding drug_demo for Table 1 counts#
drug_demo_roi_id_final <- merge(drug_demo, drug_demo_roi_id, by= c("primaryid"), all.x=F, all.y=T)
View(drug_demo_roi_id_final)

## Table 1 - Categorical variable counts (Demo) ##
table1_demo_cat <- tableby(doi_group ~ rpt_year + age_grp + sex + i_f_code + occp_cod + reporter_country, data=drug_demo_roi_id_final)
summary(table1_demo_cat)

## Table 1 - Numeric variable counts (Demo) ##
table1_demo_num <- tableby(doi_group ~ age_yr + wt_lbs, data=drug_demo_roi_id_final)
summary(table1_demo_num)

#Create table for Table 1 outcome counts#
drug_demo_roi_id_outc <- merge(drug_demo_roi_id_final, outc_all, by= c("primaryid"), all.x=T, all.y=F) 
View(drug_demo_roi_id_outc)

## Table 1 - Outcome counts ##
table1_outc <- tableby(doi_group ~ outc_cod, data=drug_demo_roi_id_outc)
summary(table1_outc)


## Start Regressions ##

#Create analytic table for analyses#
analytic_final <- merge(drug_demo_roi, outc_all, by=c("primaryid"), all.x = T, all.y= F)
View(analytic_final)

colnames(analytic_final)

# #Save initial final analysis table#
# write.csv(analytic_final, "Analytic Final 7.10.24.csv")

#Run missingness map to assess variables#
analytic_final_miss <- subset(analytic_final, select=c(5,6,7,8,21,34,35,36,37,39,40,41,43,44,45,46,47,48,50,51,52,54))

missmap(analytic_final_miss)


#Create variables for each ROI for logistic regressions#
analytic_final$loe_0_1 <- ifelse(analytic_final$roi_group == "Lack of Efficacy", 1, 0)
analytic_final$angio_0_1 <- ifelse(analytic_final$roi_group == "Angioedema", 1, 0)
analytic_final$swell_0_1 <- ifelse(analytic_final$roi_group == "Swelling", 1, 0)

#Unadjusted logistic regressions by ROI#
log_all_LOE <- glm(loe_0_1 ~ doi_group, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_LOE), confint(log_all_LOE)))       
summary(log_all_LOE)     
pR2(log_all_LOE)

log_all_angio <- glm(angio_0_1 ~ doi_group, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_angio), confint(log_all_angio)))       
summary(log_all_angio)     
pR2(log_all_angio)

log_all_swell <- glm(swell_0_1 ~ doi_group, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_swell), confint(log_all_swell)))       
summary(log_all_swell)     
pR2(log_all_swell)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# # Create time in mkt variable # - DECIDED NOT TO USE DO TOO BEING TOO CORRELATED/DEPENDENT ON DOI#
# 
# analytic_final$mkt_time <- ifelse(analytic_final$doi_group == "Berotralstat", 3.326027397, ---- #numbers calculated on FDA approval date through 3/31/24#
#                                   ifelse(analytic_final$doi_group == "Lanadelumab", 5.608219178,
#                                           ifelse(analytic_final$doi_group == "Haegarda", 6.764383562,
#                                                  ifelse(analytic_final$doi_group == "Cinryze", 15.48219178,
#                                                         ifelse(analytic_final$doi_group == "C1 Esterase Inhibitor (NS)", 11.61164384, NA)))))
# 
# analytic_final$mkt_time <- as.numeric(analytic_final$mkt_time)
# analytic_final$mkt_time_group <- as.character(analytic_final$mkt_time_group)
# analytic_final$doi_group <- as.character(analytic_final$doi_group)
# 
# write.csv(analytic_final, "Analytic Final.csv")
# 
# 
# analytic_final$event_dt <- sapply(analytic_final$event_dt, as.character)
# analytic_final$event_dt <- arrange_date(analytic_final$event_dt)
# analytic_final$event_dt[is.na(analytic_final$event_dt)] <- ""
# analytic_final[analytic_final == "NA"] <- NA
# 
# str(analytic_final)
# analytic_final <- 
# View(analytic_final)
# 
# log_all_LOE3 <- glm(loe_0_1 ~ doi_group + mkt_time, family=binomial(link='logit'), data=analytic_final)
# summary(log_all_LOE3)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_LOE3), confint(log_all_LOE3)))       
# pR2(log_all_LOE3)
# 
# log_all_angio3 <- glm(angio_0_1 ~ doi_group + mkt_time, data=analytic_final, family='binomial')
# summary(log_all_angio3)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_angio3), confint(log_all_angio3)))       
# pR2(log_all_angio3)
# 
# log_all_swell3 <- glm(swell_0_1 ~ doi_group + mkt_time, data=analytic_final, family='binomial')
# summary(log_all_swell3)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_swell3), confint(log_all_swell3)))       
# pR2(log_all_swell3)
# 
# # Check predictor relationships # 
# analytic_final_corr <- subset(analytic_final, select=c(48,52,59))
# View(analytic_final_corr)
# 
# chisq.test(analytic_final$doi_group, analytic_final$mkt_time_group)
# assocstats(xtabs(~analytic_final$doi_group + analytic_final$mkt_time_group))
# 
# model.matrix(~0+., data=analytic_final_corr) %>% 
#   cor(use="pairwise.complete.obs") %>% 
#   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
# 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


#Create FDA Approval Date variable#
analytic_final$fda_app_dt <- ifelse(analytic_final$doi_group == "Berotralstat", 20201203,
                                        ifelse(analytic_final$doi_group == "Lanadelumab", 20180823,
                                               ifelse(analytic_final$doi_group == "Haegarda", 20170627,
                                                      ifelse(analytic_final$doi_group == "Cinryze", 20081010,
                                                             ifelse(analytic_final$doi_group == "C1 Esterase Inhibitor (NS)", 20120822, NA)))))

analytic_final$fda_app_dt <- arrange_date(analytic_final$fda_app_dt)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
# # Regressions by month difference # --- DECIDED TO GO WITH YEAR DIFFERENCE#
# analytic_final$fda_app_dt <- arrange_date(analytic_final$fda_app_dt)
# 
# analytic_final$app_event_dt_diff <- (analytic_final$event_dt - analytic_final$fda_app_dt)
# 
# analytic_final$app_event_dt_diff[analytic_final$app_event_dt_diff == -2] <- 0
# analytic_final$app_event_dt_diff[analytic_final$app_event_dt_diff <0] <- NA
# summary(is.na(analytic_final$event_dt))
# 
# analytic_final$app_event_dt_diff <- analytic_final$app_event_dt_diff/12
# analytic_final$app_event_dt_diff <- as.numeric(analytic_final$app_event_dt_diff)
# 
# log_all_LOE4 <- glm(loe_0_1 ~ doi_group + app_event_dt_diff, family=binomial(link='logit'), data=analytic_final)
# summary(log_all_LOE4)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_LOE4), confint(log_all_LOE4)))       
# pR2(log_all_LOE4)
# 
# log_all_angio4 <- glm(angio_0_1 ~ doi_group + app_event_dt_diff, data=analytic_final, family='binomial')
# summary(log_all_angio4)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_angio4), confint(log_all_angio4)))       
# pR2(log_all_angio4)
# 
# log_all_swell4 <- glm(swell_0_1 ~ doi_group + app_event_dt_diff, data=analytic_final, family='binomial')
# summary(log_all_swell4)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_swell4), confint(log_all_swell4)))       
# pR2(log_all_swell4)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
# # Regressions by year difference event-FDA dates # -- #CHANGED TO REPORT DATE TO MATCH SOW#
# 
# analytic_final$fda_app_dt <- arrange_date(analytic_final$fda_app_dt)
# 
# analytic_final$app_event_dt_diff <- (analytic_final$event_dt - analytic_final$fda_app_dt)
# 
# analytic_final$app_event_dt_diff[analytic_final$app_event_dt_diff == -2] <- 0
# analytic_final$app_event_dt_diff[analytic_final$app_event_dt_diff <0] <- NA
# summary(is.na(analytic_final$app_event_dt_diff))
# 
# analytic_final$app_event_dt_diff <- analytic_final$app_event_dt_diff/364.25
# analytic_final$app_event_dt_diff <- as.numeric(analytic_final$app_event_dt_diff)
# 
# log_all_LOE5 <- glm(loe_0_1 ~ doi_group + app_event_dt_diff, family=binomial(link='logit'), data=analytic_final)
# summary(log_all_LOE5)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_LOE5), confint(log_all_LOE5)))       
# pR2(log_all_LOE5)
# 
# log_all_angio5 <- glm(angio_0_1 ~ doi_group + app_event_dt_diff, data=analytic_final, family='binomial')
# summary(log_all_angio5)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_angio5), confint(log_all_angio5)))       
# pR2(log_all_angio5)
# 
# log_all_swell5 <- glm(swell_0_1 ~ doi_group + app_event_dt_diff, data=analytic_final, family='binomial')
# summary(log_all_swell5)
# options(scipen=999)                                                               
# exp(cbind(Odds_Ratio = coef(log_all_swell5), confint(log_all_swell5)))       
# pR2(log_all_swell5)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


## Regressions with Report Date ##

analytic_final$app_rpt_dt_diff <- (analytic_final$rept_dt - analytic_final$fda_app_dt)


#Calculate to yearly difference#
analytic_final$app_rpt_dt_diff <- analytic_final$app_rpt_dt_diff/364.25
analytic_final$app_rpt_dt_diff <- as.numeric(analytic_final$app_rpt_dt_diff)

#Write csv with all variables of interest and analytic final (removed count Column A & changed to xlsx in combined folder)#
write.csv(analytic_final, "Analytic Final 7.10.24.csv")

#----------------------# IF YOU NEED TO START ANALYSES FROM HERE #---------------------------#
# analytic_final <- readxl::read_xlsx("Analytic Final 7.10.24.xlsx")
# View(analytic_final)
#--------------------------------------------------------------------------------------------#


log_all_LOE6 <- glm(loe_0_1 ~ doi_group + app_rpt_dt_diff, family=binomial(link='logit'), data=analytic_final)
summary(log_all_LOE6)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_LOE6), confint(log_all_LOE6)))       
pR2(log_all_LOE6)

log_all_angio6 <- glm(angio_0_1 ~ doi_group + app_rpt_dt_diff, data=analytic_final, family='binomial')
summary(log_all_angio6)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_angio6), confint(log_all_angio6)))       
pR2(log_all_angio6)

log_all_swell6 <- glm(swell_0_1 ~ doi_group + app_rpt_dt_diff, data=analytic_final, family='binomial')
summary(log_all_swell6)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_swell6), confint(log_all_swell6)))       
pR2(log_all_swell6)


# Plot models #
analytic_final_plots <- subset(analytic_final, select=c(48,55,56,57,59))

analytic_final_plots$LOE <- predict(log_all_LOE6, newdata = analytic_final_plots, type="response")
ggplot(analytic_final_plots, aes(x = app_rpt_dt_diff, y = LOE, colour = doi_group, group = doi_group)) + 
  geom_line() + xlab('Report/FDA Approval Date Difference (Years)') + ylab('Lack of Efficacy')

analytic_final_plots$Angioedema <- predict(log_all_angio6, newdata = analytic_final_plots, type="response")
ggplot(analytic_final_plots, aes(x = app_rpt_dt_diff, y = Angioedema, colour = doi_group, group = doi_group)) + 
  geom_line() + xlab('Report/FDA Approval Date Difference (Years)')

analytic_final_plots$Swelling <- predict(log_all_swell6, newdata = analytic_final_plots, type="response")
ggplot(analytic_final_plots, aes(x = app_rpt_dt_diff, y = Swelling, colour = doi_group, group = doi_group)) + 
  geom_line() + xlab('Report/FDA Approval Date Difference (Years)')


#-----------------------------------------------------------------------------------------------------------------------#

#### New analyses based on BioCryst meeting 7/15/24 ####

#Combined all C1 Esterase inhibitors#
analytic_final$doi_group_comb <- ifelse(analytic_final$doi_group == "Berotralstat", "Berotralstat",
                                  ifelse(analytic_final$doi_group == "Lanadelumab", "Lanadelumab",
                                         ifelse(analytic_final$doi_group == "Haegarda", "C1 Esterase Inhibitor All",
                                                ifelse(analytic_final$doi_group == "Cinryze", "C1 Esterase Inhibitor All",
                                                       ifelse(analytic_final$doi_group == "C1 Esterase Inhibitor (NS)", "C1 Esterase Inhibitor All", NA)))))

## Write csv for Analytic Final with Combined C1 esterase inhibitors (removed count Column A & changed to xlsx in shared folder) ##
write.csv(analytic_final, "Analytic Final with Combined C1s 7.15.24.csv")

#----------------------# IF YOU NEED TO START ANALYSES FROM HERE #---------------------------#
# analytic_final <- readxl::read_xlsx("Analytic Final with Combined C1s 7.15.24.xlsx")
# View(analytic_final)
#--------------------------------------------------------------------------------------------#

#Unadjusted regressions#

log_all_LOE7 <- glm(loe_0_1 ~ doi_group_comb, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_LOE7), confint(log_all_LOE7)))       
summary(log_all_LOE7)     
pR2(log_all_LOE7)

log_all_angio7 <- glm(angio_0_1 ~ doi_group_comb, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_angio7), confint(log_all_angio7)))       
summary(log_all_angio7)     
pR2(log_all_angio7)

log_all_swell7 <- glm(swell_0_1 ~ doi_group_comb, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_swell7), confint(log_all_swell7)))       
summary(log_all_swell7)     
pR2(log_all_swell7)


# DOI + Rpt/FDA time diff regressions #

log_all_LOE8 <- glm(loe_0_1 ~ doi_group_comb + app_rpt_dt_diff, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_LOE8), confint(log_all_LOE8)))       
summary(log_all_LOE8)     
pR2(log_all_LOE8)

log_all_angio8 <- glm(angio_0_1 ~ doi_group_comb + app_rpt_dt_diff, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_angio8), confint(log_all_angio8)))       
summary(log_all_angio8)     
pR2(log_all_angio8)

log_all_swell8 <- glm(swell_0_1 ~ doi_group_comb + app_rpt_dt_diff, family=binomial(link='logit'), data=analytic_final)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_swell8), confint(log_all_swell8)))       
summary(log_all_swell8)     
pR2(log_all_swell8)


# Plots of Regs #

analytic_final_plots2 <- subset(analytic_final, select=c(55,56,57,59,60))

analytic_final_plots2$LOE6 <- predict(log_all_LOE8, newdata = analytic_final_plots2, type="response")
ggplot(analytic_final_plots2, aes(x = app_rpt_dt_diff, y = LOE6, colour = doi_group_comb, group = doi_group_comb)) + 
  geom_line(size=1) + labs(title= 'Likelihood of Lack of Efficacy ADRs by Product Over Time', x='Time Since FDA Approval (Years)', y='Likelihood of Lack of Efficacy ADR', col='Product') + scale_y_continuous(limits = c(0,1))+ scale_color_manual(values=c('#5FC58B', '#4C899E', '#EFA511'))


analytic_final_plots2$Angioedema2 <- predict(log_all_angio8, newdata = analytic_final_plots2, type="response")
ggplot(analytic_final_plots2, aes(x = app_rpt_dt_diff, y = Angioedema2, colour = doi_group_comb, group = doi_group_comb)) + 
  geom_line(size=1) + labs(title= 'Likelihood of Angioedema ADRs by Product Over Time', x='Time Since FDA Approval (Years)', y='Likelihood of Angioedema ADR', col='Product') + scale_y_continuous(limits = c(0,1))+ scale_color_manual(values=c('#5FC58B', '#4C899E', '#EFA511'))


analytic_final_plots2$Swelling2 <- predict(log_all_swell8, newdata = analytic_final_plots2, type="response")
ggplot(analytic_final_plots2, aes(x = app_rpt_dt_diff, y = Swelling2, colour = doi_group_comb, group = doi_group_comb)) + 
  geom_line(size=1) + labs(title = 'Likelihood of Swelling ADRs by Product Over Time', x='Time Since FDA Approval (Years)', y='Likelihood of Swelling ADR', col='Product') + scale_y_continuous(limits = c(0,1)) + scale_color_manual(values=c('#5FC58B', '#4C899E', '#EFA511'))


## Composite Reaction Regressions ##

#Use table prior to paring for ROI#
drug_demo_reac <- merge(drug_demo, reac_all, by.x=c("primaryid"), by.y=c("primaryid"), all.x=T, all.y = F)
View(drug_demo_reac)

#Create variable to compare ROIs to other reactions#
drug_demo_reac$roi_vs_not <- ifelse(drug_demo_reac$pt %in% c("Hereditary angioedema", "Hereditary angioedema With C1 esterase inhibitor deficiency",
                                                                                             "Hereditary angioedema With normal C1 esterase inhibitor", "Angioedema", "Idiopathic angioedema","Condition aggravated", "Decreased Activity", "Drug effect less than expected", "Drug ineffective",
                                                                                                     "Drug ineffective for unapproved indication", "Drug intolerance", "Drug tolerance", "Drug tolerance increased", 
                                                                                                     "Rebound effect", "Therapeutic product effect decreased", "Therapeutic product effect delayed", 
                                                                                                     "Therapeutic product effect incomplete", "Therapeutic product ineffective", "Therapeutic response decreased", 
                                                                                                     "Therapeutic response shortened", "Therapeutic response unexpected", "Therapy non-responder", 
                                                                                                     "Therapy partial responder", "Treatment failure", "Treatment noncompliance", "Drug effect decreased", 
                                                                                                     "Drug effect delayed", "Drug effect incomplete","Brain oedema", "Breast swelling", "Choking", "Circumoral oedema", "Circumoral swelling", "Dysphagia", 
                                                                                                           "Ear swelling", "Eye swelling", "Eyelid oedema", "Face oedema", "Gastrointestinal oedema", "Generalised oedema", 
                                                                                                           "Genital swelling", "Gingival swelling", "Intestinal swelling", "Joint swelling", "Laryngeal obstruction", 
                                                                                                           "Laryngeal oedema", "Lip oedema", "Lip swelling", "Localised oedema", "Lymhoedema", "Macular oedema", 
                                                                                                           "Mouth swelling", "Mycoardial oedema", "Nasal oedema", "Non-pitting oedema", "Oedema", "Oedema genital",
                                                                                                           "Oedema peripheral", "Oesophageal oedema", "Oropharyngeal discomfort", "Oropharyngeal pain", "Oropharyngeal swelling", 
                                                                                                           "Palatal swelling", "Papilloedema", "Penile swelling", "Periorbital oedema", "Periorbital swelling", "Peripheral swelling", 
                                                                                                           "Pharyngeal oedema", "Pharyngeal swelling", "Post procedural swelling", "Pulmonary oedema", "Respiratory tract oedema", 
                                                                                                           "Scrotal swelling", "Skin oedema", "Skin swelling", "Swelling", "Swelling face", "Swelling of eyelid", "Swollen tongue", 
                                                                                                           "Testicular swelling", "Throat tightness", "Tongue oedema", "Upper airway obstruction", "Vulvovaginal swelling", 
                                                                                                           "Administration site swelling", "Catheter site swelling", "Tracheal obstruction", "Oedema mouth", "Local swelling", 
                                                                                                           "Incision site swelling", "Infusion site oedema", "Infusion site swelling", "Injection site oedema", "Injection site swelling", 
                                                                                                           "Abdominal distension", "Abdominal pain", "Abdominal pain upper", "Abdominal discomfort", "Intestinal angioedema"), 1, 0)

#Add back in FDA approval date#
drug_demo_reac$fda_app_dt <- ifelse(drug_demo_reac$doi_group == "Berotralstat", 20201203,
                                    ifelse(drug_demo_reac$doi_group == "Lanadelumab", 20180823,
                                           ifelse(drug_demo_reac$doi_group == "Haegarda", 20170627,
                                                  ifelse(drug_demo_reac$doi_group == "Cinryze", 20081010,
                                                         ifelse(drug_demo_reac$doi_group == "C1 Esterase Inhibitor (NS)", 20120822, NA)))))

drug_demo_reac$fda_app_dt <- arrange_date(drug_demo_reac$fda_app_dt)

#Calculate difference in years#
drug_demo_reac$app_rpt_dt_diff <- (drug_demo_reac$rept_dt - drug_demo_reac$fda_app_dt)

drug_demo_reac$app_rpt_dt_diff <- drug_demo_reac$app_rpt_dt_diff/364.25
drug_demo_reac$app_rpt_dt_diff <- as.numeric(drug_demo_reac$app_rpt_dt_diff)

#Recreate DOI groups with C1 esterase inhibitors combined#
drug_demo_reac$doi_group_comb <- ifelse(drug_demo_reac$doi_group == "Berotralstat", "Berotralstat",
                                        ifelse(drug_demo_reac$doi_group == "Lanadelumab", "Lanadelumab",
                                               ifelse(drug_demo_reac$doi_group == "Haegarda", "C1 Esterase Inhibitor All",
                                                      ifelse(drug_demo_reac$doi_group == "Cinryze", "C1 Esterase Inhibitor All",
                                                             ifelse(drug_demo_reac$doi_group == "C1 Esterase Inhibitor (NS)", "C1 Esterase Inhibitor All", NA)))))
View(drug_demo_reac)


#Write csv for analyses on ROI versus other (changed to xlsx in shared folder#
write.csv(drug_demo_reac, "Analytic Final for ROI versus all other reactions 7.16.24.csv")

#----------------------# IF YOU NEED TO START ANALYSES FROM HERE #---------------------------#
# drug_demo_reac <- readxl::read_xlsx("Analytic Final for ROI versus all other reactions 7.16.24.xlsx")
# View(drug_demo_reac)
#--------------------------------------------------------------------------------------------#

# Regressions for ROI vs other reactions #

log_all_reac <- glm(roi_vs_not ~ doi_group, family=binomial(link='logit'), data=drug_demo_reac)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_reac), confint(log_all_reac)))       
summary(log_all_reac)     
pR2(log_all_reac)

log_all_reac2 <- glm(roi_vs_not ~ doi_group + app_rpt_dt_diff, family=binomial(link='logit'), data=drug_demo_reac)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_reac2), confint(log_all_reac2)))       
summary(log_all_reac2)     
pR2(log_all_reac2)

log_all_reac3 <- glm(roi_vs_not ~ doi_group_comb, family=binomial(link='logit'), data=drug_demo_reac)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_reac3), confint(log_all_reac3)))       
summary(log_all_reac3)     
pR2(log_all_reac3)

log_all_reac4 <- glm(roi_vs_not ~ doi_group_comb + app_rpt_dt_diff, family=binomial(link='logit'), data=drug_demo_reac)
options(scipen=999)                                                               
exp(cbind(Odds_Ratio = coef(log_all_reac4), confint(log_all_reac4)))       
summary(log_all_reac4)     
pR2(log_all_reac4)

# Ratio of ROI versus not#
freq(drug_demo_reac$roi_vs_not)

drug_demo_reac_plots <- subset(drug_demo_reac, select=c(52,54,55))

drug_demo_reac_plots$ROI_all2 <- predict(log_all_reac4, newdata = drug_demo_reac_plots, type="response")
ggplot(drug_demo_reac_plots, aes(x = app_rpt_dt_diff, y = ROI_all2, colour = doi_group_comb, group = doi_group_comb)) + 
  geom_line(size=1) + labs(title = 'Likelihood of Reaction of Interest ADRs by Product Over Time', x='Time Since FDA Approval (Years)', y='Likelihood of Reaction of Interest ADR', col='Product') + scale_y_continuous(limits = c(0,1)) + scale_color_manual(values=c('#5FC58B', '#4C899E', '#EFA511'))


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#



