######################################
# Title :	Summarising biblio database
######################################
rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot
)
library(patchwork)

#-----------------------------------
# Read the publications database
#-----------------------------------
analysis_dat <- read_excel("Desktop/Mahnaz project/final/tdr_biblio_analysis_dataset_10_09_2025.xlsx")
#===================================================
# Publication over time by Research Area
#===================================================
research_areas <-  analysis_dat %>%
  dplyr::group_by(research_area) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

research_areas
# Publication areas
pub_year_area <- analysis_dat %>%
  dplyr::group_by(year, time,research_area) %>%
  dplyr::summarise(
    count = n()
  ) 

pub_year_area <- pub_year_area %>%
  mutate(time = fct_recode(time,
                           "Before Fellowship" = "Before",
                           "After Fellowship"  = "After"))
pub_year_area$time <- factor(pub_year_area$time, levels = c("Before Fellowship", "After Fellowship"))


    a <- ggplot(pub_year_area, aes(x = year, y = count, fill = research_area )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "A: Research areas",
    x = "",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size=18, face="bold"),
    #legend.position = c(0.07, 0.9),              # inside the plot panel
    legend.position = c(0.51, 0.5),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    strip.text = element_text(size = 18)
  )+  
  facet_grid(~time)

a
#===================================================
# Publication over time by study design
#===================================================
summary_design <-  analysis_dat %>%
  dplyr::group_by( study_design) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

pub_year_design <- analysis_dat %>%
  dplyr::group_by(year, time,study_design) %>%
  dplyr::summarise(
    count = n()
  ) 

pub_year_design <- pub_year_design %>%
  mutate(time = fct_recode(time,
                           "Before Fellowship" = "Before",
                           "After Fellowship"  = "After"))
pub_year_design 
pub_year_design$time <- factor(pub_year_design$time, levels = c("Before Fellowship", "After Fellowship"))

b <- ggplot(pub_year_design, aes(x = year, y = count, fill = study_design )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "B: Study Design",
    x = "Publication Year",
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0, size=18, face="bold"),
    legend.position = c(0.51, 1.1),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    #legend.position = "bottom",              # Move legend below plot
    #legend.direction = "horizontal",         # Make it horizontal
    legend.title = element_text(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    strip.text = element_blank()  
      )+  facet_grid(~time)
b
#-------------------------------------------------------------------
# Figure 1
#-------------------------------------------------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/figure1_pub_areas.tiff", 
     width=32, height=26, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

(a / b) +
  plot_annotation(title = "")

dev.off()

#------------------------------------
# Export research areas tables
#------------------------------------
pdf(~"Desktop/Mahnaz project/final", 
    height=8, 
    width=8
    )
grid.table(research_areas)

plot.new()
grid.table(summary_design)

dev.off()

#=============================================
# Clinical trials and RCTs by countries
#=============================================
rcts <-  analysis_dat %>%
  filter(study_design=="Clinical Trials (RCTs)") %>% 
  dplyr::group_by( country_of_fellow, time) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# Reshape so each country has Before and After columns
rcts_wide <- rcts %>%
  pivot_wider(names_from = time, values_from = count)

#write.csv(rcts_wide,"C:/IDDO_TDR/Mahanaz/bilblio/Results/rcts_wide.csv", row.names=FALSE)
#write.csv(rcts,"C:/IDDO_TDR/Mahanaz/bilblio/Results/rcts.csv", row.names=FALSE)

# Reshape so each country has Before and After columns
# Ensure all countries have both Before and After (fill with 0 if missing)
df_complete <- rcts %>%
  pivot_wider(names_from = time, values_from = count) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  pivot_longer(cols = c("Before", "After"), names_to = "time", values_to = "count")

# Side-by-side bar charts
ggplot(df_complete, aes(x = reorder(country_of_fellow, count), y = count, fill = time)) +
  geom_col(position = "dodge") +
  #geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    x = "Country",
    y = "Count",
    title = "RCTs: Before vs After fellowship, by the country of the fellow",
    fill = ""
  ) +
  scale_fill_manual(values = c("Before" = "steelblue", "After" = "tomato")) +
  theme_minimal()

#=============================
# Disease areas & drgus
#=============================
disease_areas <-  analysis_dat %>%
  dplyr::group_by(disease_area) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# child health
mat_child <- analysis_dat %>%
  dplyr::group_by(maternal_and_child_health) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

disease_year_design <- analysis_dat %>%
  dplyr::group_by(year, disease_area) %>%
  dplyr::summarise(
    count = n()
  ) 
disease_year_design
#---------------------------------
# drugs, diagnostics and vaccines
#---------------------------------
drugs <-  analysis_dat %>%
  dplyr::group_by(drugs_vaccines_diagnostics) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# none of the drugs, dx and vaccines
none <- analysis_dat %>% 
  filter(drugs_vaccines_diagnostics=="None") 

none_studies <-  none %>%
  dplyr::group_by(disease_area) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

#-----------------------------
# disease areas and drugs
#-----------------------------
disease_drugs <-  analysis_dat %>%
  dplyr::group_by(disease_area,drugs_vaccines_diagnostics) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
 mutate( prop = count/sum(count)*100)

#================================================
# Disease & drug lollipop charts
#================================================
c <- ggdotchart(disease_areas, 
           x = "disease_area", 
           y = "count",
           color = "#E7B800",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07","#E7B800"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           #group = "drugs_vaccines_diagnostics",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           #label = bac_ranks$total_articles,                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  ylab("") +
  xlab("")+
  ggtitle("A: Disease areas")+
  theme(axis.text.x = element_text(size=10, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  geom_text(aes(label = count), color = "white", size = 2, fontface = "bold") 
c
# Drugs
d <- ggdotchart(drugs, 
           x = "drugs_vaccines_diagnostics", 
           y = "count",
           color = "#00AFBB",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07","#E7B800"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           #group = "drugs_vaccines_diagnostics",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           #label = bac_ranks$total_articles,                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  ylab("") +
  xlab("")+
  ggtitle("B: Interventions")+
  theme(axis.text.x = element_text(size=10, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  geom_text(aes(label = count), color = "white", size = 2, fontface = "bold") 
d
#---------------------------
# Export high res graph
#---------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/figure2_disease_drugs.tiff", 
     width=32, height=11, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

(c + d) +
  plot_annotation(title = "")

dev.off()

# Stacked barchart
e <- ggplot(disease_drugs, aes(x = disease_area,
                               y = count, 
                               fill = drugs_vaccines_diagnostics)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")  # smaller keys
  )+
  labs(title = "C: Disease and interventions combined")
e
#---------------------------
# figure 2
#---------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/figure2_disease_drugs_breakdown.tiff", 
     width=29, height=18, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

(c+ d)/e +
  plot_layout(widths = c(1.1, 1))  # c is twice as wide as (a/b)

dev.off()


#================================
# Export disease areas tables
#================================

pdf("C:/IDDO_TDR/Mahanaz/bilblio/Results/disease_areas.pdf", 
    height=8, 
    width=8
)
grid.table(disease_areas)

plot.new()
grid.table(mat_child)

plot.new()
grid.table(drugs)

plot.new()
grid.table(disease_drugs)

dev.off()

#===============================================
# Further breakdown by before and after
#===============================================
disease_year_design <- analysis_dat %>%
  dplyr::group_by(year, time, disease_area) %>%
  dplyr::summarise(
    count = n()
  ) 

drugs_year_time <-  analysis_dat %>%
  dplyr::group_by(year, time, drugs_vaccines_diagnostics) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

supp1 <- ggplot(disease_year_design, aes(x = disease_area,
                          y = count, 
                          fill = disease_area)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")  # smaller keys
  )+
  labs(title = "Disease Areas")+
  coord_flip()+
  facet_grid(~time)

supp2 <- ggplot(drugs_year_time, aes(x = drugs_vaccines_diagnostics,
                                         y = count, 
                                         fill = drugs_vaccines_diagnostics)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")  # smaller keys
  )+
  labs(title = "Drugs, Diagnostics and Vaccines")+
  coord_flip()+
  facet_grid(~time)
supp2
# Export supplemental figure
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/supp_figure2_disease_drugs_breakdown.tiff", 
     width=29, height=18, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

supp1 /supp2

dev.off()

#=============================================
# Evolution of disease research over time
#=============================================
disease_year <-  analysis_dat %>%
  dplyr::group_by(disease_area, year) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# Stacked barchart
sup_figure <-  ggplot(disease_year, aes(x = year , 
                                        y = count, 
                                        fill = disease_area)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "A: Disease research over time (counts)",
    x = "",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) 

# Stacked barchart
sup_figure2 <-  ggplot(disease_year, aes(x = year , 
                                         y = count, fill = disease_area)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "B: Disease research over time (proportion)",
    x = "",
    y = "Proportion of publications"
  ) +
  theme_minimal(base_size = 14) 
sup_figure2 
#---------------------------
# figure 2
#---------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/supp_figure_disease_year.tiff", 
     width=42, height=14, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

sup_figure+ sup_figure2

dev.off()

#================================
# Scientific Collaborations
#================================
collab <-  analysis_dat %>%
  dplyr::group_by(international_co_authorship) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

collab_network <-  analysis_dat %>%
  dplyr::group_by(type_of_collaboration) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

#================================
# Collaboration over time
#================================
collab_network_2 <-  analysis_dat %>%
  dplyr::group_by(type_of_collaboration,year) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# Stacked barchart
f <- ggplot(collab_network_2, aes(x = year , y = count, fill = type_of_collaboration)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "A: Collaboration",
    x = "",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = c(0.1, 0.9),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.4, "cm")
  )

#================================
# Collaboration by disease areas
#================================
collab_network_3 <-  analysis_dat %>%
  dplyr::group_by(type_of_collaboration,disease_area) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# Stacked barchart
g <- ggplot(collab_network_3, aes(x = disease_area , y = count, fill = type_of_collaboration)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "B: By disease areas",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = c(0.6, 0.3),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.4, "cm")
  )+ 
  coord_flip()

#================================
# Collaboration by research areas
#================================
collab_network_4 <-  analysis_dat %>%
  dplyr::group_by(type_of_collaboration,research_area) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
mutate( prop = count/sum(count)*100)

# Stacked barchart
h <- ggplot(collab_network_4, aes(x = research_area , y = count, fill = type_of_collaboration)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
  labs(
    title = "C: By research areas",
    x = "",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = c(0.1, 1.1),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.4, "cm")
  )+ 
  coord_flip()

#================================
# Collaboration by research areas
#================================
collab_network_5 <-  analysis_dat %>%
  dplyr::group_by(type_of_collaboration,study_design) %>%
  dplyr::summarise(
    count = n()
  ) %>% 
  mutate( prop = count/sum(count)*100)

# Stacked barchart
i <- ggplot(collab_network_5, aes(x = study_design , y = count, fill = type_of_collaboration)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "") +
    labs(
    title = "D: By study design",
    x = "",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = c(0.5, 0.9),              # inside the plot panel
    legend.justification = c("left", "top"),     # anchor the position
    legend.background = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.4, "cm")
  )+
  coord_flip()

#---------------------------
# figure 3: Collaborator
#---------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/figure3_collaboration.tiff", 
     width=42, height=22, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

(f+ g+h+i) +
  plot_layout(widths = c(1.2, 1))  # c is twice as wide as (a/b)

dev.off()

#================================
# Export collaboration tables
#================================
pdf("C:/IDDO_TDR/Mahanaz/bilblio/Results/collaboration_tables.pdf", 
    height=8, 
    width=8
)
grid.table(collab)

plot.new()
grid.table(collab_network)

dev.off()

# END