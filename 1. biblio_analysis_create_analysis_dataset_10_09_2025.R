################################################################################
# Title			          :	Summarising WHO biblio database
# Task                : Summarise the database from Dawit
################################################################################
#install.packages("sjr_journals")
rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot
)
#install.packages("worldbank")
#install.packages("remotes")
#remotes::install_github("ikashnitsky/sjrdata")
library(worldbank)
library(sjrdata)

#=============================
# Working directory
#=============================
setwd("C:/IDDO_TDR/Mahanaz/bilblio/Jul data from Dawit")

#-----------------------------------------------------------------
# Tidy up the publication database (n = ~1800)
#-----------------------------------------------------------------
dat_dawit <-read_excel("Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                       sheet="data_dawit_pd") %>% 
  clean_names()

dat_dawit$authorship_position_first_mid_last <- trimws(dat_dawit$authorship_position_first_mid_last)

dat_dawit <- dat_dawit %>% 
  mutate(author_position =
          case_when(
            authorship_position_first_mid_last %in% c("first","First") ~ "first",
            authorship_position_first_mid_last %in% c("last","Last") ~ "last",
            authorship_position_first_mid_last %in% c("mid","Middle"," MIddle","middle","middle") ~ "mid",
            TRUE ~ NA_character_  # fallback for unexpected values  
  )
)

# n_before and n_after
dat_dawit$time_diff  <- dat_dawit$publication_year -dat_dawit$fellowship_year 
dat_dawit$time       <- ifelse(dat_dawit$publication_year <=dat_dawit$fellowship_year, "Before", "After")

#========================================================
# Add journal metrics to the publication database
#========================================================
journal_dat <- sjr_journals # from sjrdata library

# Tidy up the journal names and the variable names to match that with SJR database
dat_dawit <- dat_dawit %>% 
   dplyr::rename(
    title = journal_pd,
    year = publication_year
  )

# \\b ensures whole word matching (so "Of" in "Office" won't be touched).
# Case matters here: this only replaces exact "And" and "Of" (capitalized).

dat_dawit$title <- gsub("\\bAnd\\b",     "and", dat_dawit$title)
dat_dawit$title <- gsub("\\bOf\\b",      "of", dat_dawit$title)
dat_dawit$title <- gsub("\\bBmc\\b",     "BMC", dat_dawit$title)
dat_dawit$title <- gsub("\\bPlos\\b",    "PLoS", dat_dawit$title)
dat_dawit$title <- gsub("\\bIn\\b",       "in", dat_dawit$title)
dat_dawit$title <- gsub("\\bBmj\\b",      "BMJ", dat_dawit$title)
dat_dawit$title <- gsub("\\bHiv\\b",      "HIV", dat_dawit$title)
dat_dawit$title <- gsub("\\bJmir\\b",     "JMIR", dat_dawit$title)
dat_dawit$title <- gsub("\\bPeerj\\b",    "PeerJ", dat_dawit$title)
dat_dawit$title <- gsub("\\bElife\\b",     "eLife", dat_dawit$title)
dat_dawit$title <- gsub("\\bJama\\b",      "JAMA", dat_dawit$title)

dat_dawit <- dat_dawit %>%
  mutate(title = case_when(
    title == "The BMJ"                                                   ~ "BMJ",
    title == "The Lancet. Global Health" ~ "The Lancet Global Health",
    title == "The Lancet. Infectious Diseases" ~ "The Lancet Infectious Diseases",
    title == "The Lancet Hiv" ~ "The Lancet Hiv",
    title == "Ethiopian Journal of Health Sciences"   ~ "Ethiopian journal of health sciences",
    title =="PLoS ONE"  ~ "PLoS ONE",
    title =="The Journal of Infectious Diseases" ~ "Journal of Infectious Diseases",
    title =="Transactions of The Royal Society of Tropical Medicine and Hygiene" ~ "Transactions of the Royal Society of Tropical Medicine and Hygiene",
    title =="Memorias Do Instituto Oswaldo Cruz" ~ "Memorias do Instituto Oswaldo Cruz",
    title =="Jac-Antimicrobial Resistance" ~ "JAC-Antimicrobial Resistance",
    title =="Tropical Medicine & International Health" ~ "Tropical Medicine and International Health",
    title == "The Royal Society Open Science" ~"Royal Society Open Science",
    title =="Journal of The International Aids Society" ~ "Journal of the International AIDS Society",
    title =="Aids and Behavior" ~ "AIDS and Behavior",
    title =="Aids Research and Therapy" ~ "AIDS Research and Therapy",
    title =="Antimicrobial Resistance & Infection Control" ~ "Antimicrobial Resistance and Infection Control",
    title =="Biomed Research International" ~ "BioMed Research International",
    title =="Febs" ~ "FEBS Journal",
    title =="Febs Letters" ~"FEBS Letters",
    title =="Revista Da Associacao Medica Brasileira" ~"Revista da Associacao Medica Brasileira",
    title =="The Journal of Antimicrobial Chemotherapy" ~"Journal of Antimicrobial Chemotherapy",
    title =="European Journal of Obstetrics & Gynecology and Reproductive Biology" ~"European Journal of Obstetrics and Gynecology and Reproductive Biology",
    title =="Cellular & Molecular Immunology" ~"Cellular and Molecular Immunology",
    title == "Journal of Nepal Medical Association" ~"Journal of the Nepal Medical Association",
    title =="Aids" ~ "AIDS",
    title == "Aids Care" ~ "AIDS Care - Psychological and Socio-Medical Aspects of AIDS/HIV",
    title =="HIV/Aids - Research and Palliative Care" ~"HIV/AIDS - Research and Palliative Care",
    title =="PLoS Global Public Health" ~"PLOS Global Public Health",
    title =="International Journal For Parasitology" ~ "International Journal for Parasitology",
    title =="Jbi Evidence Synthesis" ~ "JBI Evidence Synthesis",
    title =="Journal of The Royal Society of Medicine" ~ "Journal of the Royal Society of Medicine",
    title =="Leukemia & Lymphoma" ~"Leukemia and Lymphoma",
    title =="Maternal & Child Nutrition" ~ "Maternal and Child Nutrition",
    title =="The Lancet. HIV" ~ "The Lancet HIV",
    title =="MBio" ~ "mBio",
    title == "Mbio" ~ "mBio",
    title =="The Lancet. Respiratory Medicine" ~ "The Lancet Respiratory Medicine",
    title =="African Journal of Primary Health Care & Family Medicine" ~ "African Journal of Primary Health Care and Family Medicine",
    title =="Cadernos De Saude Publica" ~ "Cadernos de Saude Publica",
    title =="Drugs & Therapy Perspectives" ~ "Drugs and Therapy Perspectives",
    title =="Erj Open Research" ~ "ERJ Open Research",
    title =="Ijid Regions" ~ "IJID Regions",
    title =="Acs Omega" ~ "ACS Omega",
    title =="Npj Vaccines" ~ "npj Vaccines",
    title =="Food & Nutrition Research" ~ "Food and Nutrition Research",
    title == "Aas Open Research" ~ "AAS Open Research",
    title =="International Journal of Std & Aids" ~ "International Journal of STD and AIDS",
    title =="Journal of Acquired Immune Deficiency Syndromes" ~ "Journal of Acquired Immune Deficiency Syndromes (1999)",
    title =="Journal of The Pediatric Infectious Diseases Society" ~ "Journal of the Pediatric Infectious Diseases Society",
    title == "Médecine Et Maladies Infectieuses Formation" ~ "Medecine et Maladies Infectieuses",
    title == "Neurology: Neuroimmunology & Neuroinflammation" ~ "Neurology: Neuroimmunology and NeuroInflammation",
    title =="Pediatric Blood & Cancer" ~ "Pediatric Blood and Cancer",
    title == "Pharmacology Research & Perspectives" ~ "Pharmacology Research and Perspectives",
    title =="Pnas Nexus" ~ "PNAS Nexus",
    title =="Revista Colombiana De Psiquiatria" ~ "Revista Colombiana de Psiquiatria",
    title =="Revista Facultad De Medicina" ~ "Revista Facultad de Medicina",
    title == "Acta Paediatrica" ~ "Acta Paediatrica, International Journal of Paediatrics",
    title == "Tobacco Prevention & Cessation" ~ "Tobacco Prevention and Cessation",
    title == "The Pediatric Infectious Disease Journal" ~ "Pediatric Infectious Disease Journal",
    title == "The Journal of Parasitology" ~ "Journal of Parasitology",
    title == "Annales De La Societe Entomologique De France" ~ "Annales de la Societe Entomologique de France",
    title == "Biotechniques" ~ "BioTechniques",
    title == "Clinical and Experimental Gastroenterololy" ~ "Clinical and Experimental Gastroenterology",
    title == "Cpt: Pharmacometrics & Systems Pharmacology" ~ "CPT: Pharmacometrics and Systems Pharmacology",
    title == "Human Vaccines & Immunotherapeutics" ~ "Human Vaccines and Immunotherapeutics",
    title == "Ingenieria Y Universidad" ~ "Ingenieria y Universidad",
    title == "INQUIRY: The Journal of Health Care Organization, Provision, and Financing" ~ "Inquiry (United States)",
    title == "Revista Panamericana De Salud Pública" ~ "Revista Panamericana de Salud Publica/Pan American Journal of Public Health",
    title == "Revista Peruana De Biologia" ~ "Revista Peruana de Biologia",
    title == "Southern African Journal of Infectious Diseases" ~ "Southern African Journal of Epidemiology and Infection",
    title == "The Ethiopian Journal of Health Development" ~ "Ethiopian Journal of Health Development",
    title == "The European Respiratory Journal" ~ "European Respiratory Journal",
    title == "The International Journal of Lower Extremity Wounds" ~ "International Journal of Lower Extremity Wounds",
    title == "The Journal of Clinical Pharmacology" ~ "Journal of Clinical Pharmacology",
    title == "The Journal of Infection in Developing Countries" ~ "Journal of Infection in Developing Countries",
    
    TRUE ~ title  # keep unchanged if not matched
  ))

# flag the preprints/book chapters and conference proceedings
dat_dawit <- dat_dawit %>% 
  mutate(
    preprints = case_when(
      title =="Book Chapter in Critical Care Medicine" ~ "Yes",
      title =="Methods in Molecular Biology (Clifton, N.j.)" ~ "Yes",
      title =="Liposomes: Methods and Protocols, Volume 2: Biological Membrane Models (Book)" ~ "Yes",
      title =="Book Chapter in Critical Care Medicine" ~ "Yes",
      title =="Researchsquare" ~ "Yes",
      title =="Medrxiv" ~ "Yes",
      #title ==  "Proceedings of the 30th Annual International Conference of the IEEE Engineering in Medicine and Biology Society, EMBS'08 - \"Personalized Healthcare through Technology\"" ~ "Yes",
      TRUE ~ "No"     # keep unchanged if not matched
    )
  )

#----------------------------------------------------------------------------------------------------------------
# Break into chunks: 
# a) successfully linked, 
# b) Prepints, 
# c) Journals that are not in the SJR database, 
# d) Journals in the SJR database but no data for a given year. Use closest match (later year preferred)
#----------------------------------------------------------------------------------------------------------------
data_for_merging <- dat_dawit %>% select(row_id, title,year,preprints) 

# Join the review database with the SJR database using year and title
merged <- data_for_merging %>%
    left_join(journal_dat, by = c("year", "title")) 

# a) Succesfully linked
merged_a <- merged %>%  filter(!is.na(rank)) %>% 
  mutate(
        year_imputed = year,
         match = "exact"
        )

# b) Preprints/bookchapters etc
merged_b <- merged %>%  filter(preprints=="Yes") %>% 
  mutate(
    year_imputed = year,
    match = "preprints"
  )

# c) Journals that are not in the SJR and hence cannot be linked
merged_c <- merged %>%  filter(is.na(rank)) %>% 
                  filter(preprints!="Yes") %>% 
                filter(!title %in% journal_dat$title) %>% 
  mutate(
    year_imputed = year,
    match = "Journal not found"
  )

# d) Journals in the SJR database but no data for a given year. 
# Use closest match (later year preferred)
# Add logical column to df2

# Rename year in each dataframe to avoid collision
journal_dat_a <- journal_dat %>% 
  dplyr:: rename(year_imputed = year)

merged_d <- merged %>% 
        filter(is.na(rank)) %>% 
         filter(title %in% journal_dat$title) %>% 
         select(row_id, title,preprints,year) %>% 
  mutate(year_imputed = if_else(year == 2025, 2024, year))  # keep all 2025 as 2024

# Join with the journal database
merged_d_I <- merged_d %>%
  left_join(journal_dat_a, by = c("year_imputed", "title")) %>% 
  filter(!is.na(rank)) %>% 
  mutate(
    match = "imputed"
  )

merged_d_II <- merged_d %>%
  left_join(journal_dat_a, by = c("year_imputed", "title")) %>% 
  filter(is.na(rank)) %>% 
  select(row_id, title,preprints,year) 

# Join and find closest year match, preferring latest year on ties
merged_d_II <- merged_d_II %>%
  left_join(journal_dat_a, by = "title", relationship = "many-to-many") %>%
  mutate(gap = abs(year - year_imputed)) %>%
  group_by(row_id) %>%
  slice_min(gap, with_ties = TRUE) %>%  
  ungroup() %>%
  mutate(
    match = if_else(!is.na(year_imputed), "imputed", "no_match")
  ) %>%
  relocate(match, gap, .after = year_imputed) %>% 
  select(-gap)

#--------------------------------------
# Bind together all the data now
#--------------------------------------
sjr_final <- rbind(merged_a, merged_b, merged_c, merged_d_I, merged_d_II)

sjr_final <- sjr_final %>% 
  dplyr:: rename(
    sjr_match = match,
    sjr_year_if_imputed = year_imputed
  ) %>% 
  select(row_id,
         issn,
         sjr_match,
         sjr_year_if_imputed, 
         sjr,
         sjr_best_quartile, 
         h_index,
         total_docs_year,
         total_docs_3years,
         total_refs,
         total_citations_3years,
         citable_docs_3years,
         citations_doc_2years,
         ref_doc
         )
         
rm(merged, 
    merged_a, 
    merged_b, 
    merged_c, 
    merged_d,
    merged_d_I, 
    merged_d_II,
    journal_dat_a
   )

#=======================================================
# Process the Journal Impact Factor from Clarivate
#=======================================================
clarivate_dat <- read_excel("C:/IDDO_TDR/Mahanaz/bilblio/Clarivate/journal_metrics_01_08_2025.xlsx", 
                       sheet="Jounal_metrics_PD_full_extactio") %>%
      clean_names() %>% 
  select (-c(n_pubs, total_pubs))

# Convert using dplyr
clarivate_dat <- clarivate_dat %>%
  mutate(cv_per_citable_items_oa = if_else(
    grepl("[0-9]", cv_per_citable_items_oa),
    as.numeric(gsub("[^0-9.]", "", cv_per_citable_items_oa)),
    NA_real_
  )) %>% 
    mutate(cv_journal_impact_factor = if_else(
    grepl("[0-9]", cv_journal_impact_factor),
    as.numeric(gsub("[^0-9.]", "", cv_journal_impact_factor)),
    NA_real_
  )) %>% 
    mutate(cv_journal_citation_indicator = if_else(
    grepl("[0-9]", cv_journal_citation_indicator),
    as.numeric(gsub("[^0-9.]", "", cv_journal_citation_indicator)),
    NA_real_
  )) %>% 
  mutate(year_master = year)

#==============================================
# Merge the Impact factor information first
#==============================================
clarivate_jif <- clarivate_dat %>% 
          select(title, year, cv_journal_impact_factor) 

data_for_merging <- data_for_merging %>%
          mutate(year = if_else(year == 2025, 2024, year))  # keep all 2025 as 2024
  
# Join the review database with the clarivate database using year and title
jif_merged <- data_for_merging %>%
  left_join(clarivate_jif, by = c("year", "title")) 

#---------------------
# Complete match
#---------------------
jif_merged_a <- data_for_merging %>%
              left_join(clarivate_jif, by = c("year", "title")) %>% 
            filter(!is.na(cv_journal_impact_factor)) %>% 
  mutate(
    year_imputed = year,
    match = "exact"
  )

#---------------
# Preprints
#---------------
jif_merged_b <- data_for_merging %>%
  left_join(clarivate_jif, by = c("year", "title")) %>% 
  filter(preprints=="Yes") %>% 
  mutate(
    year_imputed = year,
    match = "preprints"
  )

#------------------------------------------------------------
# For those with missing JIF, use nearest neighbor approach
#------------------------------------------------------------
# Join and find closest year match, preferring latest year on ties
clarivate_jif_a <- clarivate_jif %>% 
  dplyr:: rename(year_imputed = year) %>% # Rename year in master dataframe to avoid collision
  filter(!is.na(cv_journal_impact_factor))

jif_merged_c <- jif_merged %>% 
  filter(is.na(cv_journal_impact_factor)) %>% 
  select(row_id, title,preprints,year) %>% 
  filter(preprints=="No")

# Join and find closest year match, preferring latest year on ties
jif_merged_c <- jif_merged_c %>%
  left_join(clarivate_jif_a, by = "title", relationship = "many-to-many") %>%
  mutate(gap = abs(year - year_imputed)) %>%
  group_by(row_id) %>%
  slice_min(gap, with_ties = TRUE) %>%  # ✅ Keep all tied closest matches
  ungroup() %>%
  mutate(
    match = if_else(!is.na(year_imputed), "imputed", "no_match")
  ) %>%
  relocate(match, gap, .after = year_imputed) %>%
  select(-gap)

jif_final <- rbind(
  jif_merged_a,
  jif_merged_b,
  jif_merged_c
) 

jif_final <-jif_final %>% 
dplyr:: rename(
  jif_match = match,
  jif_year_if_imputed = year_imputed
 ) %>% 
select(row_id,jif_match, jif_year_if_imputed, cv_journal_impact_factor)

# remove transient data frames
rm(
  clarivate_jif,
  clarivate_jif_a,
  jif_merged,
  jif_merged_a,
  jif_merged_b,
  jif_merged_c
) 

#==========================================================
# Process the Journal Citation Index (JCI) from Clarivate
#==========================================================
clarivate_jci<- clarivate_dat %>% 
  select(title, year, cv_journal_citation_indicator) 

# Join the review database with the clarivate database using year and title
jci_merged <- data_for_merging %>%
              left_join(clarivate_jci, by = c("year", "title")) 

#---------------------
# Complete match
#---------------------
jci_merged_a <- data_for_merging %>%
  left_join(clarivate_jci, by = c("year", "title")) %>% 
  filter(!is.na(cv_journal_citation_indicator)) %>% 
  mutate(
    year_imputed = year,
    match = "exact"
  )

#---------------
# Preprints
#---------------
jci_merged_b <- data_for_merging %>%
  left_join(clarivate_jci, by = c("year", "title")) %>% 
  filter(preprints=="Yes") %>% 
  mutate(
    year_imputed = year,
    match = "preprints"
  )

#------------------------------------------------------------
# For those with missing JIF, use nearest neighbor approach
#------------------------------------------------------------
# Join and find closest year match, preferring latest year on ties
clarivate_jci_a <- clarivate_jci %>% 
  dplyr:: rename(year_imputed = year) %>% # Rename year in master dataframe to avoid collision
  filter(!is.na(cv_journal_citation_indicator))

jci_merged_c <- jci_merged %>% 
  filter(is.na(cv_journal_citation_indicator)) %>% 
  select(row_id, title,preprints,year) %>% 
  filter(preprints=="No")

# Join and find closest year match, preferring latest year on ties
jci_merged_c <- jci_merged_c %>%
  left_join(clarivate_jci_a, by = "title", relationship = "many-to-many") %>%
  mutate(gap = abs(year - year_imputed)) %>%
  group_by(row_id) %>%
  slice_min(gap, with_ties = TRUE) %>% 
  ungroup() %>%
  mutate(
    match = if_else(!is.na(year_imputed), "imputed", "no_match")
  ) %>%
  relocate(match, gap, .after = year_imputed) %>%
  select(-gap)


jci_final <- rbind(
  jci_merged_a,
  jci_merged_b,
  jci_merged_c
) 

jci_final <-jci_final %>% 
  dplyr:: rename(
    jci_match = match,
    jci_year_if_imputed = year_imputed
  ) %>% 
  select(row_id,jci_match, jci_year_if_imputed, cv_journal_citation_indicator)

rm(
  clarivate_jci,
  clarivate_jci_a,
  jci_merged,
  jci_merged_a,
  jci_merged_b,
  jci_merged_c
) 

#--------------------------
# Open Access status
#--------------------------
oa_info <- clarivate_dat %>% 
  select(title,cv_per_citable_items_oa) %>% 
  group_by(title) %>%
  slice(1) %>%
  ungroup()


oa_info <- oa_info %>%
  mutate(
    oa_status = case_when(
      is.na(cv_per_citable_items_oa)                              ~ "No Data",
      cv_per_citable_items_oa <= 25                               ~ "Not Open Access",
      cv_per_citable_items_oa > 25 & cv_per_citable_items_oa < 75 ~ "Hybrid Access",
      cv_per_citable_items_oa >= 75                               ~ "Open Access",
      TRUE                                                        ~ NA_character_
    )
  )

oa_info <- oa_info %>%
  mutate(
    oa_status = case_when(
      title %in% c("F1000Research",
                   "Wellcome Open Research",
                   "Gates Open Research",
                   "Medrxiv",
                   "PLoS Clinical Trials",
                   "Pan African Medical Journal-Clinical Medicine",
                   "eLife",
                   "BMC Dermatology",
                   "BMC Global and Public Health",
                   "Frontiers in Epidemiology",
                   "Frontiers in Malaria",
                   "Frontiers in Tropical Diseases"
                   )                                     ~ "Open Access",
         TRUE ~ oa_status
    )) 

table(oa_info$oa_status)
sum(table(oa_info$oa_status))

#=================================================================
# Merge the journal indicators with analysis dataset
#=================================================================

# analysis dataset
analysis_dat <- dat_dawit %>% 
  left_join(sjr_final, by="row_id")

analysis_dat <- analysis_dat %>% 
  left_join(jci_final, by="row_id")

analysis_dat <- analysis_dat %>% 
  left_join(jif_final, by="row_id")

analysis_dat <- analysis_dat %>% 
  left_join(oa_info, by="title")

# 1 Article has missing OA status
analysis_dat$oa_status <- factor(analysis_dat$oa_status, 
                                 levels = names(sort(table(analysis_dat$oa_status), decreasing = TRUE)))
analysis_dat$oa_status[is.na(analysis_dat$oa_status)] <- "No Data" 
table(analysis_dat$oa_status)
sum(table(analysis_dat$oa_status))

rm(
  sjr_final,
  jci_final, 
  jif_final, 
  oa_info,
  journal_dat,
  data_for_merging,
  clarivate_dat,
  dat_dawit
)

# Clean using case_when and summarize
analysis_dat <- analysis_dat %>%
  mutate(
    maternal_and_child_health = case_when(
            maternal_and_child_health %in% c("no", "No", "NO")   ~ "No",
            maternal_and_child_health %in% c("yes", "Yes")       ~ "Yes",
              TRUE ~ NA_character_
  )) 
sum(table(analysis_dat$maternal_and_child_health))


analysis_dat <- analysis_dat %>%
  mutate(
    drugs_vaccines_diagnostics = case_when(
      drugs_vaccines_diagnostics %in% c("Diagnostics")        ~ "Diagnostics",
      drugs_vaccines_diagnostics %in% c("Drugs")              ~ "Drugs",
      drugs_vaccines_diagnostics %in% c("n", "No","no")       ~ "None",
      drugs_vaccines_diagnostics %in% c("Vaccine","Vaccines") ~ "Vaccines",
      drugs_vaccines_diagnostics %in% c("Yes")                ~ "Drugs/Diagnostics/Vaccines",
      drugs_vaccines_diagnostics %in% c("NTD")                ~ "Unclear",
      TRUE ~ NA_character_
    )) 

sum(table(analysis_dat$drugs_vaccines_diagnostics))


# Clean using case_when and summarize
table(analysis_dat$international_co_authorship)

analysis_dat <- analysis_dat %>%
  mutate(
    international_co_authorship = case_when(
      international_co_authorship %in% c("no", "No", "NO")   ~ "No",
      international_co_authorship %in% c("yes", "Yes")       ~ "Yes",
      TRUE ~ NA_character_
    )) 

# Clean using case_when and summarize
table(analysis_dat$sjr_best_quartile)
analysis_dat <- analysis_dat <- analysis_dat %>%
  mutate(sjr_best_quartile = case_when(
    is.na(sjr_best_quartile) ~ "Not graded",
    sjr_best_quartile == "-" ~ "Not graded",
    TRUE ~ sjr_best_quartile
  ))
table(analysis_dat$sjr_best_quartile)
sum(table(analysis_dat$sjr_best_quartile))

# Research Areas
table(analysis_dat$research_areas_health_system_clinical_trial_basic)

analysis_dat <- analysis_dat %>%
  mutate(
    research_areas_health_system_clinical_trial_basic = case_when(
                
      research_areas_health_system_clinical_trial_basic %in% 
                                          c("Basic", "Basic research", "Basic Research")                                ~ "Basic Research",
      
      research_areas_health_system_clinical_trial_basic %in% 
                                        c("clinical", "Clinical", "Clinical research","Clinical Research","clinical")   ~ "Clinical Research",
      
      research_areas_health_system_clinical_trial_basic %in% 
                              c("clinical trail", "clinical trial", "clinical Trial","Clinical trial","Clinical Trial") ~ "Clinical Trials",
      
      research_areas_health_system_clinical_trial_basic %in% 
                 c("health system","health System","Health system","Health System","Health systems","Health Systems")   ~ "Health Systems",
      
      research_areas_health_system_clinical_trial_basic %in% 
                                  c("Diagnostics","Pharmacovigilance","Policy-focused","Vaccine development","Missing")   ~ "Other",
      
      is.na(research_areas_health_system_clinical_trial_basic)                                                            ~ "Other"
      
    )) 

table(analysis_dat$research_areas_health_system_clinical_trial_basic)
sum(table(analysis_dat$research_areas_health_system_clinical_trial_basic))

#----------------------
# Disease grouping
#----------------------
# read the revised grouping
disease_grouping <-read_excel("Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                       sheet="diaseas_area") %>% 
  clean_names()

analysis_dat <- analysis_dat %>% 
  left_join(disease_grouping, by="diaseas_area")
table(analysis_dat$disease_grouping_pd)
sum(table(analysis_dat$disease_grouping_pd))
table(analysis_dat$disease_grouping_pd)
table(analysis_dat$disease_grouping_pd)
sum(table(analysis_dat$disease_grouping_pd))

#----------------------
# study design
#----------------------
table(analysis_dat$study_design_pd)
as.data.frame(table(analysis_dat$study_design_pd))
sum(table(analysis_dat$disease_grouping_pd))

analysis_dat <- analysis_dat %>%
  mutate(
    study_design_pd = case_when(
      study_design_pd %in% c("Case control")                                          ~ "Case Control Studies",
      study_design_pd %in% c("Case report/case series")                               ~ "Other Observational Research",
      study_design_pd %in% c("Clinical Trials ")                                      ~ "Clinical Trials ",
      study_design_pd %in% c("Cross sectional")                                       ~ "Cross-Sectional",
      study_design_pd %in% c("Other observational research","Surveys/Surveillence")   ~ "Other Observational Research",
  
      TRUE ~ study_design_pd
    )) 
colnames(analysis_dat)

# Drop redundant columns
analysis_dat <- analysis_dat %>% 
  select(-c(diaseas_area,study_design,journal_dawit,added_by_dawit,pd_comments,authorship_position_first_mid_last))

# gender of the fellow
table(analysis_dat$gender_of_fellow)
analysis_dat <- analysis_dat %>%
  mutate(
    gender_of_fellow = case_when(
      gender_of_fellow %in% c("female", "Female")   ~ "Female",
      gender_of_fellow %in% c("male", "Male", "MALE")       ~ "Male",
    TRUE ~ gender_of_fellow
    ))

# Tidy up names
analysis_dat <- analysis_dat %>% 
  dplyr:: rename(
    study_design    = study_design_pd,
    disease_area    = disease_grouping_pd,
    research_area   = research_areas_health_system_clinical_trial_basic,
    quartile_old    = quartile,
    journal_name    = title
  )

###############################################################
## Add the fellows country, income and other distribution
###############################################################
# Read the fellows data (n=128)
dat_fellows <-read_excel("Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                         sheet="data_mahnaz_pd") %>%
  clean_names()
summary(dat_fellows$age_at_fellowship)

# Trim whitespace from the 'Name' column
dat_fellows <- dat_fellows %>%
  mutate(
    home_country = str_trim(home_country),
    country_name = str_trim(country_name)
  )

dat_fellows <- dat_fellows %>%
  mutate(
    WHO_Region = case_when(
      home_country  %in% c("Benin","Botswana","Burkina Faso","Cameroon","Congo","Eswatini",
                           "Ethiopia","Gabon","Ghana","Guinea","Kenya","Liberia","Madagascar","Malawi","Mali",
                           "Mozambique","Nigeria","RDC","Rwanda","Senegal","Sierra Leone","South Africa","Sudan",
                           "Tanzania","The Gambia","Uganda","Zambia","Zimbabwe")                               ~ "African Region",
      
      home_country =="Tunisia"                                                            ~ "Eastern Mediterranean Region",
      home_country %in% c("Argentina","Brazil","Colombia","Peru")                         ~  "Region of the Americas",
      home_country %in% c("Bangladesh","India","Nepal")                                   ~  "South-East Asian Region",
      home_country %in% c("China","Indonesia","Vietnam")                                  ~  "Western Pacific Region"
    )
  )

dat_fellows <- dat_fellows %>%
  mutate(
    WHO_Region_Code = case_when(
      home_country  %in% c("Benin","Botswana","Burkina Faso","Cameroon","Congo","Eswatini",
                           "Ethiopia","Gabon","Ghana","Guinea","Kenya","Liberia","Madagascar","Malawi","Mali",
                           "Mozambique","Nigeria","RDC","Rwanda","Senegal","Sierra Leone","South Africa","Sudan",
                           "Tanzania","The Gambia","Uganda","Zambia","Zimbabwe")                               ~ "AFR",
      
      home_country =="Tunisia"                                                            ~ "EMR",
      home_country %in% c("Argentina","Brazil","Colombia","Peru")                         ~  "AMR",
      home_country %in% c("Bangladesh","India","Nepal")                                   ~  "SEAR",
      home_country %in% c("China","Indonesia","Vietnam")                                  ~  "WPR"
    )
  )

# Income status from R package
wb_dat <- wb_country() %>% 
  select(country_name, income_level_id) %>% 
  filter(country_name %in% dat_fellows$country_name)

dat_fellows <- dat_fellows %>% 
  left_join(wb_dat, by="country_name") %>% 
  dplyr:: rename(who_fellow = name) 

dat_fellows <- dat_fellows %>% 
  select(
    c(who_fellow, age_at_fellowship,home_country, WHO_Region, WHO_Region_Code, income_level_id, tpo_country,tpo_region)
  )

# analysis dataset
analysis_dat <- analysis_dat %>% 
  left_join(dat_fellows, by="who_fellow")

## Keep up some of the original names from DAwit
dat_dawit_original <-read_excel("Clinical Research and Development Program Publications_May 2025_v2_DGA_11_07_2025.xlsx", 
                       sheet="data_dawit_pd") %>% 
  clean_names()
colnames(dat_dawit_original)

dat_dawit_original <- dat_dawit_original %>% 
  select(row_id, 
         drugs_vaccines_diagnostics,
         diaseas_area,
         study_design,
         research_areas_health_system_clinical_trial_basic
         )
colnames(dat_dawit_original)[2:5] <- paste0("dawit_", colnames(dat_dawit_original)[2:5])

analysis_dat <- analysis_dat %>% 
  left_join(dat_dawit_original, by="row_id")

# Remove preprints and book chapters
analysis_dat <- analysis_dat %>% 
  filter(preprints=="No")
table(analysis_dat$study_design)

#=====================================
# Export analysis data as CSV
#=====================================
write.csv(analysis_dat,
          "C:/IDDO_TDR/Mahanaz/bilblio/final_analysis_data/tdr_biblio_analysis_dataset_10_09_2025.csv",
          row.names=FALSE)

#=====================================
# Export analysis dataset as Excel
#=====================================
#install.packages("writexl")
library(writexl)
# Export to Excel
write_xlsx(analysis_dat, "C:/IDDO_TDR/Mahanaz/bilblio/final_analysis_data/tdr_biblio_analysis_dataset_10_09_2025.xlsx")

# END Script
