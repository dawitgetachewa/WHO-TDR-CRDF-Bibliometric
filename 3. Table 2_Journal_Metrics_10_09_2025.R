################################################################################
# Title :	Summarising WHO biblio database
################################################################################
#install.packages("sjr_journals")
rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot
)
library(worldbank)
library(sjrdata)

#=============================
# Working directory
#=============================
setwd("C:/IDDO_TDR/Mahanaz/bilblio/Jul data from Dawit")

# Read the publications database
analysis_dat <- read_excel("Desktop/Mahnaz project/final/tdr_biblio_analysis_dataset_10_09_2025.xlsx")%>%
    clean_names
#==========================================================================
# Summarise journal indicators before and after fellowship
#==========================================================================

#------------------------------------------
# Summary for journal open-access status
#------------------------------------------
oa_status <- analysis_dat %>%
  dplyr::group_by(time,oa_status) %>%
  dplyr::summarise(
    count = n()
  )
# Reshape so that time becomes columns (Before, After)
oa_status_wide <- oa_status %>%
  pivot_wider(
    names_from = time,
    values_from = count,
    values_fill = 0  # fill missing combinations with 0
  )

#-----------------------------------
# Summary for journal quartiles
#-----------------------------------
journal_quartile <- analysis_dat %>%
  dplyr::group_by(time, sjr_best_quartile) %>%
  dplyr::summarise(count = n(), .groups = 'drop')

# Reshape so that time becomes columns (Before, After)
journal_quartile_wide <- journal_quartile %>%
  pivot_wider(
    names_from = time,
    values_from = count,
    values_fill = 0  # fill missing combinations with 0
  )
#-----------------------------------
# Summary for journal SJR score
#-----------------------------------
sjr_table <- analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    n_non_missing = sum(!is.na(sjr)),
    median     = median(sjr, na.rm = TRUE),
    q25        = quantile(sjr, 0.25, na.rm = TRUE),
    q75        = quantile(sjr, 0.75, na.rm = TRUE),
    min_sjr    = min(sjr, na.rm = TRUE),
    max_sjr    = max(sjr, na.rm = TRUE)
  )

# Pivot longer (turn columns into rows)
sjr_table_long <- sjr_table %>%
  pivot_longer(cols = -time, names_to = "stat", values_to = "value")

# Pivot wider (turn time values into columns)
sjr_table_flipped <- sjr_table_long %>%
  pivot_wider(names_from = time, values_from = value)

#-----------------------------------
# Summary for journal Impact factor
#-----------------------------------
jif_table <- analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    n_non_missing  = sum(!is.na(cv_journal_impact_factor)),
    median         = median(cv_journal_impact_factor, na.rm = TRUE),
    q25            = quantile(cv_journal_impact_factor, 0.25, na.rm = TRUE),
    q75            = quantile(cv_journal_impact_factor, 0.75, na.rm = TRUE),
    min_jif        = min(cv_journal_impact_factor, na.rm = TRUE),
    max_jif        = max(cv_journal_impact_factor, na.rm = TRUE),
    high_impact    = sum(cv_journal_impact_factor>10,na.rm = TRUE)
  )

# Pivot longer (turn columns into rows)
jif_table_long <- jif_table %>%
  pivot_longer(cols = -time, names_to = "stat", values_to = "value")

# Pivot wider (turn time values into columns)
jif_table_flipped <- jif_table_long %>%
  pivot_wider(names_from = time, values_from = value)

#-----------------------------------
# Summary for journal H-index
#-----------------------------------
h_table <- analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    n_non_missing = sum(!is.na(h_index)),
    median     = median(h_index, na.rm = TRUE),
    q25        = quantile(h_index, 0.25, na.rm = TRUE),
    q75        = quantile(h_index, 0.75, na.rm = TRUE),
    min_h      = min(h_index, na.rm = TRUE),
    max_h      = max(h_index, na.rm = TRUE)
  )

# Pivot longer (turn columns into rows)
h_table_long <- h_table %>%
  pivot_longer(cols = -time, names_to = "stat", values_to = "value")

# Pivot wider (turn time values into columns)
h_table_flipped <- h_table_long %>%
  pivot_wider(names_from = time, values_from = value)

#-----------------------------------
# Summary for JCI Indicator
#-----------------------------------
jci_table <- analysis_dat %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    n_non_missing    = sum(!is.na(cv_journal_citation_indicator)),
    median           = median(cv_journal_citation_indicator, na.rm = TRUE),
    q25              = quantile(cv_journal_citation_indicator, 0.25, na.rm = TRUE),
    q75              = quantile(cv_journal_citation_indicator, 0.75, na.rm = TRUE),
    min_jci          = min(cv_journal_citation_indicator, na.rm = TRUE),
    max_jci          = max(cv_journal_citation_indicator, na.rm = TRUE)
  )

# Pivot longer (turn columns into rows)
jci_table_long <- jci_table %>%
  pivot_longer(cols = -time, names_to = "stat", values_to = "value")

# Pivot wider (turn time values into columns)
jci_table_flipped <- jci_table_long %>%
  pivot_wider(names_from = time, values_from = value)

#==========================================
# Export Journal metrics as Table
#==========================================

# Start PDF device
pdf("Desktop/Mahnaz project/finals/journal_metrics.pdf", 
    height = 8, 
    width = 8
)

# Your plotting code goes here
# Example:
# plot(x, y)
# barplot(counts)

# Close PDF device
dev.off()

grid.table(oa_status)

plot.new()
grid.table(oa_status_wide)

plot.new()
grid.table(journal_quartile)

plot.new()
grid.table(journal_quartile_wide)

plot.new()
grid.table(sjr_table)

plot.new()
grid.table(jif_table)

plot.new()
grid.table(h_table)

plot.new()
grid.table(jci_table)

dev.off()

# END
