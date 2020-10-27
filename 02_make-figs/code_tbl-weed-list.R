##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: June 5 2020
#
# Purpose: make manuscript figs
#
# Notes: 
# Last modified: 7/13/2020 (change UB/UG to UD/UM)
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

library(patchwork)
library(PFIweeds2020)



# constant themes ---------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFC000"
p_gray <- "#E7E6E6"

scales::show_col(p_green)


# weed list ---------------------------------------------------------------

dat_table <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  group_by(weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2),
         pct2 = ifelse(pct < 0.1, "<0.10", pct),
         pct2 = paste0(pct2, "%")) %>% 
  arrange(-pct) %>% 
  rename(code = weed) %>% 
  left_join(pfi_weedsplist) %>% 
  unite(photo_path, functional_grp, col = "desc", sep = " ") %>% 
  select(code, scientific_name, common_name, desc, pct2) %>% 
  mutate(scientific_name = str_to_sentence(scientific_name),
         common_name = str_to_sentence(common_name),
         #scientific_name = ifelse(scientific_name == "Setaria", "Setaria genus", scientific_name),
         common_name = ifelse(common_name == "Water hemp", "Waterhemp", common_name),
         common_name = ifelse(common_name == "Marestail", "Horseweed", common_name),
         common_name = ifelse(common_name == "Nightshade", "Eastern black nightshade", common_name),
         common_name = ifelse(common_name == "Rye (cereal)", "Cereal rye", common_name),
         desc = ifelse(desc == "NA NA", NA, desc))

dat_table

write_csv(dat_table, "02_make-figs/mf_weed-list-arranged.csv")


# weed list, by trial ---------------------------------------------------------------

dat_table_trial <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  unite(site_name, field, sys_trt, col = "site_sys") %>% 
  group_by(site_sys, weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2),
         pct2 = ifelse(pct < 0.1, "<0.10", pct),
         pct2 = paste0(pct2, "%")) %>% 
  arrange(-pct) %>% 
  select(site_sys, weed, pct2) %>% 
  pivot_wider(names_from = site_sys, values_from = pct2)

dat_table_trial

write_csv(dat_table_trial, "02_make-figs/mf_weed-list-arranged-by-trial.csv")


