#===========================================================
# Title	:	Plots show publications before-after fellowship
#===========================================================
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
library(mgcv) # for GAMS
# Install devtools if you don't have it
install.packages("devtools")
library(devtools)

# Install from GitHub (replace with correct repo if needed)
remotes::install_github("dawitgetachewa/Dawit-Assefa", subdir = "sjrdata")
#=============================
# Working directory
#=============================
setwd("C:/IDDO_TDR/Mahanaz/bilblio/Jul data from Dawit")

# Read the publications database
analysis_dat <- read_excel("Desktop/Mahnaz project/final/tdr_biblio_analysis_dataset_10_09_2025.xlsx")

#=========================
# Journal Impact Factor
#=========================
jif_dat <- analysis_dat %>% filter(!is.na(cv_journal_impact_factor))
gam_before <- gam(cv_journal_impact_factor ~ s(time_diff), data = filter(jif_dat, time == "Before"))
gam_after  <- gam(cv_journal_impact_factor ~ s(time_diff), data = filter(jif_dat, time == "After"))

# Predict fitted values
jif_dat <- jif_dat %>%
  mutate(fit = ifelse(time == "Before",
                      predict(gam_before, newdata = jif_dat),
                      predict(gam_after, newdata = jif_dat)))


# Plot raw data and fitted lines with custom colors and legend label
a <- ggplot(jif_dat, aes(x = time_diff, y = cv_journal_impact_factor, color = time)) +
  geom_jitter(shape = 1, size=5,alpha = 1.5, width = 0.5, height = 0.3) +  # Open circles
  geom_line(aes(y = fit), size = 1.2, color = "salmon") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Before" = "#0072B2", "After" = "#E69F00"),
    name = "Period"
  ) +
  labs(
    title = "A: Journal Impact Factor",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Bold and larger title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
a
#====================
# SJR Score
#====================
sjr_dat <- analysis_dat %>% filter(!is.na(sjr))
gam_before <- gam(sjr ~ s(time_diff), data = filter(sjr_dat, time == "Before"))
gam_after  <- gam(sjr ~ s(time_diff), data = filter(sjr_dat, time == "After"))

# Predict fitted values
sjr_dat <- sjr_dat %>%
  mutate(fit = ifelse(time == "Before",
                      predict(gam_before, newdata = sjr_dat),
                      predict(gam_after, newdata = sjr_dat)))


# Plot raw data and fitted lines with custom colors and legend label
b <- ggplot(sjr_dat, aes(x = time_diff, y = sjr, color = time)) +
  geom_jitter(shape = 1, size=5,alpha = 1.5, width = 0.5, height = 0.3) +  # Open circles
  geom_line(aes(y = fit), size = 1.2, color = "salmon") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Before" = "#0072B2", "After" = "#E69F00"),
    name = "Period"
  ) +
  labs(
    title = "B: SJR Score",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Bold and larger title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )+
  theme(legend.position = "none")
b
#====================
# H-index
#====================
h_dat <- analysis_dat %>% filter(!is.na(h_index))
gam_before <- gam(h_index ~ s(time_diff), data = filter(h_dat, time == "Before"))
gam_after  <- gam(h_index ~ s(time_diff), data = filter(h_dat, time == "After"))

# Predict fitted values
h_dat <- h_dat %>%
  mutate(fit = ifelse(time == "Before",
                      predict(gam_before, newdata = h_dat),
                      predict(gam_after, newdata = h_dat)))

# Plot raw data and fitted lines with custom colors and legend label
c<- ggplot(h_dat, aes(x = time_diff, y = h_index, color = time)) +
  geom_jitter(shape = 1, size=5,alpha = 1.5, width = 0.5, height = 0.3) +  # Open circles
  geom_line(aes(y = fit), size = 1.2, color = "salmon") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Before" = "#0072B2", "After" = "#E69F00"),
    name = "Period"
  ) +
  labs(
    title = "C: Journal H-index",
    x = "Publication year relative to the fellowship year",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Bold and larger title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )+
  theme(legend.position = "none")
c
#====================
# JCI-score
#====================
jci_dat <- analysis_dat %>% filter(!is.na(cv_journal_citation_indicator))
gam_before <- gam(cv_journal_citation_indicator ~ s(time_diff), data = filter(jci_dat, time == "Before"))
gam_after  <- gam(cv_journal_citation_indicator ~ s(time_diff), data = filter(jci_dat, time == "After"))

# Predict fitted values
jci_dat <- jci_dat %>%
  mutate(fit = ifelse(time == "Before",
                      predict(gam_before, newdata = jci_dat),
                      predict(gam_after, newdata = jci_dat)))

# Plot raw data and fitted lines with custom colors and legend label
d<-ggplot(jci_dat, aes(x = time_diff, y = cv_journal_citation_indicator, color = time)) +
  geom_jitter(shape = 1, size=5, alpha = 1.5, width = 0.5, height = 0.3) +  
  geom_line(aes(y = fit), size = 1.2, color = "salmon") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Before" = "#0072B2", "After" = "#E69F00"),
    name = "Period"
  ) +
  labs(
    title = "D: JCI-Indicator",
    x = "Publication year relative to the fellowship year",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Bold and larger title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  theme(legend.position = "none")
d
#---------------------------------------
# Export the figures as 2 by 2 panel
#---------------------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/journal_metrics_before_after_10_Sep_2025.tiff", 
     width=32, height=20, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=600, antialias = "none" )

a + b+ c + d

dev.off()

#==============================================================================
# Before after plot: Authorship positions: BY FELLOWSHIP ROUNDS
#==============================================================================
refined_palette_10 <- c(
  "#4E79A7",  # blue
  "#F28E2B",  # orange
  "#E15759",  # red
  "#76B7B2",  # teal
  "#59A14F",  # green
  "#EDC948",  # yellow
  "#B07AA1",  # purple
  "#FF9DA7",  # light red-pink
  "#9C755F",  # brown
  "#BAB0AC"   # gray
)

# n_pub before and after
before_after <- analysis_dat %>%
  dplyr::group_by(fellowship_round,time_diff,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

# Plot1
plot1 <- ggplot(before_after, aes(x = time_diff, y = count, fill = fellowship_round)) +
  
  # Add shaded "before" area
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "lavender", alpha = 0.4) +
  
  # Add shaded "after" area
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "honeydew", alpha = 0.4) +
  
  geom_bar(stat = "identity") +
  #geom_bar(stat = "identity", position="fill") +
  labs(
    title = "",
    x = "",
    y = "Number of publications"
  ) +
  scale_fill_manual(
    values = refined_palette_10,
    name = "Fellowship Round"
  )+
  #  scale_fill_manual(values = c("first" = "#0072B2","last" = "#E69F00","mid"="lightblue")) +
  
  #  scale_fill_manual(
  #    values = c("first" = "#0072B2", "last" = "#E69F00", "mid" = "lightblue"),
  #  labels = c("first" = "First author", "last" = "Last author", "mid" = "Middle author"),
  #  name = "Authorship position"
  # )+
  theme_minimal() +
  #guides(fill = guide_legend(title = "Fellowship Round")) +
  theme(
    axis.text = element_text(size = 12),         # Axis tick labels
    axis.title = element_text(size = 16),        # Axis titles
    legend.title = element_text(size = 12),      # Legend title
    legend.text = element_text(size = 12)        # Legend text
  )+
  geom_vline(xintercept = 0, color = "#E66456", linetype = 6, size = 1.2) +  # <-- vertical line
  annotate("segment", x = -0.5, xend = -20, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("segment", x = 0.5, xend = 15, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("text", x = -10, y =160, label = "Before fellowship",
           size = 5, color = "#333333") +
  
  annotate("text", x = 10, y = 160, label = "After fellowship",
           size = 5, color = "#333333")
plot1
#---------------------------
# authorship positions
#---------------------------
# n_pub before and after
analysis_dat %>%
  dplyr::group_by(time,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

analysis_dat %>%
  dplyr::group_by(time, author_position) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

# n_pub before and after
before_after <- analysis_dat %>%
  dplyr::group_by(fellowship_round,time_diff,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

# plot2
plot2 <- ggplot(before_after, aes(x = time_diff, y = count, fill = author_position)) +
  
  # Add shaded "before" area
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "lavender", alpha = 0.4) +
  
  # Add shaded "after" area
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "honeydew", alpha = 0.4) +
  
  geom_bar(stat = "identity") +
  #geom_bar(stat = "identity", position="fill") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  #  scale_fill_manual(values = c("first" = "#0072B2","last" = "#E69F00","mid"="lightblue")) +
  
  scale_fill_manual(
    values = c("first" = "#0072B2", "last" = "#E69F00", "mid" = "lightblue"),
    labels = c("first" = "First author", "last" = "Last author", "mid" = "Middle author"),
    name = "Authorship position"
  )+
  theme_minimal() +
  guides(fill = guide_legend(title = "Authorship position")) +
  theme(
    axis.text = element_text(size = 14),         # Axis tick labels
    axis.title = element_text(size = 16, face = "bold"),        # Axis titles
    legend.title = element_text(size = 14),      # Legend title
    legend.text = element_text(size = 12)        # Legend text
  )+
  geom_vline(xintercept = 0, color = "#E66456", linetype = 6, size = 1.2) +  # <-- vertical line
  annotate("segment", x = -0.5, xend = -20, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("segment", x = 0.5, xend = 15, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("text", x = -10, y =160, label = "Before fellowship",
           size = 5, color = "#333333") +
  
  annotate("text", x = 10, y = 160, label = "After fellowship",
           size = 5, color = "#333333")
plot2
#==============================================================================
# Before after plot: COLLABORATIONS
#==============================================================================
# n_pub before and after
before_after2 <- analysis_dat %>%
  dplyr::group_by(type_of_collaboration,time_diff) %>%
  dplyr::summarise(
    count = n()
  ) 

# collaborations plot
plot3 <- ggplot(before_after2, aes(x = time_diff, y = count, fill = type_of_collaboration)) +
  
  # Add shaded "before" area
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "lavender", alpha = 0.4) +
  
  # Add shaded "after" area
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "honeydew", alpha = 0.4) +
  
  geom_bar(stat = "identity") +
  #geom_bar(stat = "identity", position="fill") +
  labs(
    title = "",
    x = "Year of publication relative to the fellowship year",
    y = "Number of publications"
  ) +
  scale_fill_manual(
    values = refined_palette_10,
    name = "Collaborations"
  )+
  #  scale_fill_manual(values = c("first" = "#0072B2","last" = "#E69F00","mid"="lightblue")) +
  
  #  scale_fill_manual(
  #    values = c("first" = "#0072B2", "last" = "#E69F00", "mid" = "lightblue"),
  #  labels = c("first" = "First author", "last" = "Last author", "mid" = "Middle author"),
  #  name = "Authorship position"
  # )+
  theme_minimal() +
  guides(fill = guide_legend(title = "Collaborations")) +
  theme(
    axis.text = element_text(size = 14),         # Axis tick labels
    axis.title = element_text(size = 14),        # Axis titles
    legend.title = element_text(size = 14),      # Legend title
    legend.text = element_text(size = 14)        # Legend text
  )+
  geom_vline(xintercept = 0, color = "#E66456", linetype = 6, size = 1.2) +  # <-- vertical line
  annotate("segment", x = -0.5, xend = -20, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("segment", x = 0.5, xend = 15, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("text", x = -10, y =160, label = "Before fellowship",
           size = 5, color = "#333333") +
  
  annotate("text", x = 10, y = 160, label = "After fellowship",
           size = 5, color = "#333333")
plot3
#===================================================
# Authorship positions among N-S collaboration
#===================================================

# N-N collaboration
analysis_dat %>%
  filter(type_of_collaboration=="North–North") %>% 
  dplyr::group_by(time,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

# N-S collaboration
analysis_dat %>%
  filter(type_of_collaboration=="North–South") %>% 
  dplyr::group_by(time,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

# N-S collaboration by time
analysis_dat %>%
  filter(type_of_collaboration=="North–South") %>% 
  dplyr::group_by(time, author_position) %>%
  dplyr::summarise(count = n(), .groups = "drop_last") %>%
  dplyr::mutate(
    total = sum(count),
    prop = (count / total) * 100
  )

# Summarise N-S collab and plot
before_after2b <- analysis_dat %>%
  filter(type_of_collaboration=="North–South") %>% 
  dplyr::group_by(fellowship_round,time_diff,author_position) %>%
  dplyr::summarise(
    count = n()
  ) 

# plot5
plot5 <- ggplot(before_after2b, aes(x = time_diff, y = count, fill = author_position)) +
  
  # Add shaded "before" area
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "lavender", alpha = 0.4) +
  
  # Add shaded "after" area
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "honeydew", alpha = 0.4) +
  
  geom_bar(stat = "identity") +
  #geom_bar(stat = "identity", position="fill") +
  labs(
    title = "",
    x = "Year of publication relative to the fellowship year",
    y = ""
  ) +
  #  scale_fill_manual(values = c("first" = "#0072B2","last" = "#E69F00","mid"="lightblue")) +
  
  scale_fill_manual(
    values = c("first" = "#0072B2", "last" = "#E69F00", "mid" = "lightblue"),
    labels = c("first" = "First author", "last" = "Last author", "mid" = "Middle author"),
    name = "Authorship position"
  )+
  theme_minimal() +
  guides(fill = guide_legend(title = "Authorship position \n in N-S collaboration")) +
  theme(
    axis.text = element_text(size = 14),         # Axis tick labels
    axis.title = element_text(size = 14),        # Axis titles
    legend.title = element_text(size = 14),      # Legend title
    legend.text = element_text(size = 14)        # Legend text
  )+
  geom_vline(xintercept = 0, color = "#E66456", linetype = 6, size = 1.2) +  # <-- vertical line
  annotate("segment", x = -0.5, xend = -20, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("segment", x = 0.5, xend = 15, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("text", x = -10, y =160, label = "Before fellowship",
           size = 5, color = "#333333") +
  
  annotate("text", x = 10, y = 160, label = "After fellowship",
           size = 5, color = "#333333")
plot5
#============================================
# Evolution of SJR quartile
#============================================
# SJR quartile and after
before_after3 <- analysis_dat %>%
  dplyr::group_by(sjr_best_quartile,time_diff) %>%
  dplyr::summarise(
    count = n()
  ) 

plot4<- ggplot(before_after3, aes(x = time_diff, y = count, fill = sjr_best_quartile)) +
  
  # Add shaded "before" area
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "lavender", alpha = 0.4) +
  
  # Add shaded "after" area
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "honeydew", alpha = 0.4) +
  
  geom_bar(stat = "identity") +
  #geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "",
    x = "Year of publication relative to the fellowship year",
    y = ""
  ) +
  #  scale_fill_manual(values = c("first" = "#0072B2","last" = "#E69F00","mid"="lightblue")) +
  
 # scale_fill_manual(
  #   values = c("first" = "#0072B2", "last" = "#E69F00", "mid" = "lightblue"),
  #  labels = c("first" = "First author", "last" = "Last author", "mid" = "Middle author"),
  #  name = "Authorship position"
  #)+
  scale_fill_manual(
    values = refined_palette_10,
    name = "Journal quartile"
  )+
  theme_minimal() +
  guides(fill = guide_legend(title = "Journal quartile")) +
  theme(
    axis.text = element_text(size = 14),         # Axis tick labels
    axis.title = element_text(size = 14),        # Axis titles
    legend.title = element_text(size = 14),      # Legend title
    legend.text = element_text(size = 14)        # Legend text
  )+
  geom_vline(xintercept = 0, color = "#E66456", linetype = 6, size = 1.2) +  # <-- vertical line
  
  annotate("segment", x = -0.5, xend = -20, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("segment", x = 0.5, xend = 15, y = 152, yend = 152,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "darkgrey", size = 0.8) +
  
  annotate("text", x = -10, y =160, label = "Before fellowship",
           size = 5, color = "#333333") +
  
  annotate("text", x = 10, y = 160, label = "After fellowship",
           size = 5, color = "#333333")

#==========================================
# Export Before-After plots
#==========================================
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/publication_before_after_panel_plots_10_Sep_2025.tiff", 
     width=36, 
     height=20, 
     units="cm", 
     pointsize="8", 
     compression = "lzw+p", 
     bg="white", 
     res=600, 
     antialias = "none" 
     )
plot1 + plot2 + plot3 + plot5

dev.off

#===================================================
# Top 20 Journal as lollipop
#===================================================
# Most frequent journal
journal_name <- analysis_dat %>%
  dplyr::group_by(journal_name) %>%
  dplyr::summarise(
    count = n()
  ) 

# Publication areas
journal_top10 <- analysis_dat %>% 
  dplyr::group_by(journal_name) %>%
  dplyr::summarise(
    count = n()
  )  %>%  
  mutate( prop = count/sum(count)*100) %>% 
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Top journals before
journal_top10_before <- analysis_dat %>% filter(time=="Before") %>% 
  dplyr::group_by(journal_name) %>%
  dplyr::summarise(
    count = n()
  ) %>%
  filter(count >=10)

# Top journals after
journal_top10_after <- analysis_dat %>% filter(time=="After") %>% 
  dplyr::group_by(journal_name) %>%
  dplyr::summarise(
    count = n()
  ) %>%
  filter(count >=10)

# top10 journals before
plot1 <- ggdotchart(journal_top10_before, 
                    x = "journal_name",
                    y = "count",
                    color = "#00AFBB",                            # Color by groups
                    sorting = "descending",                       # Sort value in descending order
                    add = "segments",                             # Add segments from y = 0 to dots
                    rotate = TRUE,                                # Rotate vertically
                    dot.size = 8,                                 # Large dot size
                    label = journal_top10_before$count,          # Add count values as dot labels
                    font.label = list(color = "white", size = 9, vjust = 0.5), # Label params
                    ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of publications") +
  xlab("") +
  ggtitle("A: Before Fellowship") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )

# top10 journals after
plot2 <-  ggdotchart(journal_top10_after, 
            x = "journal_name",
            y = "count",
            color = "#00AFBB",                            
            sorting = "descending",                       
            add = "segments",                             
            rotate = TRUE,                                
            dot.size = 8,                                 # Large dot size
            label = journal_top10_after$count,            
            font.label = list(color = "white", size = 9, 
                              vjust = 0.5),               # Adjust label parameters
            ggtheme = theme_pubr()                        # ggplot2 theme
 )+
   ylab("Number of publications") +
   #ylim (0,100)+
   xlab("")+
   ggtitle("B: After Fellowship")+
   theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )

#--------------------------------
# Top journals before and after
#--------------------------------
tiff(file="C:/IDDO_TDR/Mahanaz/bilblio/Results/freq_journals_before_after_panel_plots_10_Sep_2025.tiff", 
     width=36, 
     height=16, 
     units="cm", 
     pointsize="8", 
     compression = "lzw+p", 
     bg="white", 
     res=800, 
     antialias = "none" 
)
plot1 + plot2 

dev.off()

#==========================================
# Export Journal metrics as Table
#==========================================
pdf("C:/IDDO_TDR/Mahanaz/bilblio/Results/journal_metrics_freq.pdf", 
    height=12, 
    width=26
)

grid.table(journal_top10)

plot.new()
grid.table(journal_top10_before)

plot.new()
grid.table(journal_top10_after)

dev.off()
# END