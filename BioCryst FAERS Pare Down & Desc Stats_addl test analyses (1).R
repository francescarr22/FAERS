#### BioCryst FAERS Additional Test Analyses ####

## Therapy Start & End date missingness ##
analytic_final_ther <- merge(analytic_final, ther_all, by.x=c("primaryid", "drug_seq"), by.y = c("primaryid", "dsg_drug_seq"), all.x = T, all.y = F)
View(analytic_final_ther)

summary(is.na(analytic_final_ther$start_dt))
table(summary(is.na(analytic_final_ther$end_dt)), analytic_final_ther$doi_group)

analytic_final_ther$end_dt <- arrange_date(analytic_final_ther$end_dt)
analytic_final_ther$end_dt <- as.numeric(analytic_final_ther$end_dt)
analytic_final_ther$end_dt_mis <- ifelse(is.na(analytic_final_ther$end_dt), 'No', 'Yes')
table(analytic_final_ther$end_dt_mis, analytic_final_ther$doi_group)

analytic_final_ther$start_dt_mis <- ifelse(is.na(analytic_final_ther$start_dt), 'No', 'Yes')
summary(tableby(analytic_final_ther$doi_group ~ analytic_final_ther$end_dt_mis))
summary(tableby(analytic_final_ther$doi_group ~ analytic_final_ther$start_dt_mis))


## Firazyr Test for Lana in Active Ingredient ##
drug_all_ps_fir <- drug_all_ps %>% filter(str_detect(drug_all_ps$drugname, 'Firazyr|FIRAZYR|firazyr'))
View(drug_all_ps_fir)

freq(drug_all_ps_fir$prod_ai)