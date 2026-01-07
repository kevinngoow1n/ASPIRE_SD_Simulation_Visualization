library(gtools)
# library(ensurer) # not compatible with current R versions
library(truncnorm)
library(assertthat)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
library(reticulate)
library(fastRG)
library(mice)
library(svglite)

# setting up data

# creating the diag, trans, and simData files
diag_time_demo_sum_all.df = NULL
trans_tree_demo_sum_all.df = NULL
simData_sum_all.df = NULL

TEMPORARY_FOLDER_FILE_NAMES = list(
  "gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9"
  #, "gen_O9Y9_i", "_O1Y7_i", "_O2Y5_i", "_O3Y8_i", "_O4Y6_i", "_O5Y5_i", "_O6Y1_i", "_O7Y6_i", "_O9Y8_i", "_O8Y9_i", "_O10Y2_i"
  )

for (folder_name in TEMPORARY_FOLDER_FILE_NAMES) {
  diag_time_demo_sum_all.df = NULL
  trans_tree_demo_sum_all.df = NULL
  simData_sum_all.df = NULL
  #simObj files
  for (simObj_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention/output_simObj_epi", folder_name))) { 
    simObj = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention/output_simObj_epi", folder_name, "/", simObj_rds_file))
    diag_time_demo.df = left_join(
      simObj$diag_time %>% filter(event == "diagnosis"),
      bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                simObj$popdf_dead),
      by = join_by(ID == id))
    
    diag_time_demo.df = diag_time_demo.df %>%
      mutate(year = ceiling(month/12)+2018)
    
    diag_time_demo_sum.df = diag_time_demo.df %>%
      group_by(month, race, year) %>%
      summarize(diag = n()) %>%
      ungroup() %>%
      group_by(race, year) %>%
      summarize(tot_diag = sum(diag))
    
    diag_time_demo_sum.df$file = simObj_rds_file
    
    diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all.df,
                                          diag_time_demo_sum.df)
    
    diag_time_demo_sum_all.df = diag_time_demo_sum_all.df %>%
      arrange(year)
    
    trans_tree_demo.df = left_join(simObj$trans_tree, bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                                                                simObj$popdf_dead) %>% select(id, race),
                                   by = join_by(ID2 == id))
    
    trans_tree_demo.df = trans_tree_demo.df %>%
      mutate(year = ceiling(month/12)+2018)
    
    trans_tree_demo_sum.df = trans_tree_demo.df %>%
      group_by(month, race, year) %>%
      summarize(infects = n()) %>%
      ungroup() %>%
      group_by(race, year) %>%
      summarize(tot_infects = sum(infects))
    
    trans_tree_demo_sum.df$file = simObj_rds_file
    
    trans_tree_demo_sum_all.df = bind_rows(trans_tree_demo_sum_all.df,
                                           trans_tree_demo_sum.df)
  }
  
  #simData files
  for (simData_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention/output_simData_epi", folder_name))) { 
    simData_list = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention/output_simData_epi", folder_name, "/", simData_rds_file))
    simData = bind_rows(simData_list$notdisc, simData_list$disc)
    simData_sum.df = simData %>%
      group_by(month) %>%
      filter(stage %in% c("care", "diag", "left", "suppress")) %>%
      summarise(total_diag = sum(pospopsize),
                supp_diag = sum(pospopsize[stage == "suppress"])) %>%
      mutate(supp_prop = supp_diag / total_diag) %>%
      mutate(year = ceiling(month/12)+2018) %>%
      ungroup() %>%
      group_by(year) %>%
      summarise(mean_supp_prop_annual = mean(supp_prop))
    
    simData_sum_all.df = bind_rows(simData_sum_all.df,
                                   simData_sum.df)
  }
  # currently
  saveRDS(diag_time_demo_sum_all.df,
          paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_name, "_ni_race_iter20_diag.rds", sep = "")
  )
  saveRDS(trans_tree_demo_sum_all.df,
          paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_name, "_ni_race_iter20_infects.rds", sep = "")
  )
  saveRDS(simData_sum_all.df,
          paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_name, "_ni_race_iter20_suppress_prop.rds", sep = "")
  )
}

## no change
TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES = list(
  "gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
  "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9")

## diag no intervention
diag_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O5Y5"){
    diag_time_demo_sum_all_eg_ni.df = readRDS('/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epigen_O5Y5_ni_race_iter20_diag.rds') %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    diag_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_file_no_intervention, "_ni_race_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi')
    
    diag_time_demo_sum_all_e_ni.df = bind_rows(diag_time_demo_sum_all_e_ni.df,
                                               diag_time_demo_sum_all_e_temp_ni.df)
  }
}

## time tree (infection) no intervention
inf_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O5Y5"){
    inf_time_demo_sum_all_eg_ni.df = readRDS('/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epigen_O5Y5_ni_race_iter20_infects.rds') %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    inf_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_file_no_intervention, "_ni_race_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi')
    
    inf_time_demo_sum_all_e_ni.df = bind_rows(inf_time_demo_sum_all_e_ni.df,
                                              inf_time_demo_sum_all_e_temp_ni.df)
  }
}

# RACE intervention
# creating the diag, trans, and simData files
diag_time_demo_sum_all.df = NULL
trans_tree_demo_sum_all.df = NULL
simData_sum_all.df = NULL

TEMPORARY_FOLDER_FILE_NAMES = list(
  "gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
  "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9"
)

race_list = list("black", "hispanic", "other")

for (race in race_list){
  for (folder_name in TEMPORARY_FOLDER_FILE_NAMES) {
    diag_time_demo_sum_all.df = NULL
    trans_tree_demo_sum_all.df = NULL
    simData_sum_all.df = NULL
    #simObj files
    for (simObj_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_race/output_epi", folder_name, "/output_simObj_", race))) { 
      simObj = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_race/output_epi", folder_name, "/output_simObj_", race, "/", simObj_rds_file))
      diag_time_demo.df = left_join(
        simObj$diag_time %>% filter(event == "diagnosis"),
        bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                  simObj$popdf_dead),
        by = join_by(ID == id))
      
      diag_time_demo.df = diag_time_demo.df %>%
        mutate(year = ceiling(month/12)+2018)
      
      diag_time_demo_sum.df = diag_time_demo.df %>%
        group_by(month, race, year) %>%
        summarize(diag = n()) %>%
        ungroup() %>%
        group_by(race, year) %>%
        summarize(tot_diag = sum(diag))
      
      diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all.df,
                                            diag_time_demo_sum.df)
      
      diag_time_demo_sum_all.df = diag_time_demo_sum_all.df %>%
        arrange(year)
      
      trans_tree_demo.df = left_join(simObj$trans_tree, bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                                                                  simObj$popdf_dead) %>% select(id, race),
                                     by = join_by(ID2 == id))
      
      trans_tree_demo.df = trans_tree_demo.df %>%
        mutate(year = ceiling(month/12)+2018)
      
      trans_tree_demo_sum.df = trans_tree_demo.df %>%
        group_by(month, race, year) %>%
        summarize(infects = n()) %>%
        ungroup() %>%
        group_by(race, year) %>%
        summarize(tot_infects = sum(infects))
      
      trans_tree_demo_sum.df$file = simObj_rds_file
      
      trans_tree_demo_sum_all.df = bind_rows(trans_tree_demo_sum_all.df,
                                             trans_tree_demo_sum.df)
    }
    
    #simData files
    for (simData_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_race/output_epi", folder_name, "/output_simData_", race))) { 
      simData_list = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_race/output_epi", folder_name, "/output_simData_", race, "/", simData_rds_file))
      simData = bind_rows(simData_list$notdisc, simData_list$disc)
      simData_sum.df = simData %>%
        group_by(month) %>%
        filter(stage %in% c("care", "diag", "left", "suppress")) %>%
        summarise(total_diag = sum(pospopsize),
                  supp_diag = sum(pospopsize[stage == "suppress"])) %>%
        mutate(supp_prop = supp_diag / total_diag) %>%
        mutate(year = ceiling(month/12)+2018) %>%
        ungroup() %>%
        group_by(year) %>%
        summarise(mean_supp_prop_annual = mean(supp_prop))
      
      simData_sum_all.df = bind_rows(simData_sum_all.df,
                                     simData_sum.df)
    }
    # currently
    saveRDS(diag_time_demo_sum_all.df,
            # '/Volumes/SSK Media/output_epi_epigen_Nov2025/intervention_RACE_rds_files'
            paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_RACE_rds_files/epi", folder_name, "_", race, "_iter20_diag.rds", sep = "")
    )
    saveRDS(trans_tree_demo_sum_all.df,
            paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_RACE_rds_files/epi", folder_name, "_", race, "_iter20_infects.rds", sep = "")
    )
    saveRDS(simData_sum_all.df,
            paste("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_RACE_rds_files/epi", folder_name, "_", race, "_iter20_suppress_prop.rds", sep = "")
    )
  }
}

## black
race = "black"

FOLDER_NAMES = list("gen_O9Y9", "_O1Y7", "_O2Y5", "_O3Y8", 
                    "_O4Y6", "_O5Y5", "_O6Y1", "_O7Y6", "_O8Y9", 
                    "_O9Y8", "_O10Y2")
### diag
diag_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    diag_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    diag_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    diag_time_demo_sum_all_e_r.df = bind_rows(diag_time_demo_sum_all_e_r.df,
                                              diag_time_demo_sum_all_e_temp_r.df)
  }
}

### time tree (infection)
inf_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    inf_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    inf_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    inf_time_demo_sum_all_e_r.df = bind_rows(inf_time_demo_sum_all_e_r.df,
                                             inf_time_demo_sum_all_e_temp_r.df)
  }
}

# combining all diag
diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e_ni.df, diag_time_demo_sum_all_e_r.df)
diag_time_demo_sum_all_eg.df = bind_rows(diag_time_demo_sum_all_eg_ni.df, diag_time_demo_sum_all_eg_r.df)
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

# combining all time tree (infection)
inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_r.df)
inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_r.df)
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

SD_diag.df = read_csv("/Users/kevinnguyen/Downloads/sd_newDx_race_realloc.csv") %>% 
  rename(year = `Diagnosis year`,
         race = `Exposure Category`,
         mean_diag = n_adj) %>%
  mutate(sd_diag = 0) %>%
  filter(year %in% c(2019:2021)) %>%
  mutate(race = case_when(race == "Black" ~ "black",
                          race == "Hispanic" ~ "hispanic",
                          race == "Other" ~ "other")) %>%
  select(year, race, mean_diag, sd_diag) %>%
  mutate(type = "Observed",
         sim_type = "Observed")

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_inf= mean(tot_infects),
            sd_inf = sd(tot_infects)) %>%
  mutate(type = "Simulation")

inf_time_demo_sum_plot.df = inf_time_demo_sum_mean.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

## hispanic
race = "hispanic"

FOLDER_NAMES = list("gen_O9Y9", "_O1Y7", "_O2Y5", "_O3Y8", 
                    "_O4Y6", "_O5Y5", "_O6Y1", "_O7Y6", "_O8Y9", 
                    "_O9Y8", "_O10Y2")
### diag
diag_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    diag_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    diag_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    diag_time_demo_sum_all_e_r.df = bind_rows(diag_time_demo_sum_all_e_r.df,
                                              diag_time_demo_sum_all_e_temp_r.df)
  }
}

### time tree (infection)
inf_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    inf_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    inf_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    inf_time_demo_sum_all_e_r.df = bind_rows(inf_time_demo_sum_all_e_r.df,
                                             inf_time_demo_sum_all_e_temp_r.df)
  }
}

# combining all diag
diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e_ni.df, diag_time_demo_sum_all_e_r.df)
diag_time_demo_sum_all_eg.df = bind_rows(diag_time_demo_sum_all_eg_ni.df, diag_time_demo_sum_all_eg_r.df)
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

# combining all time tree (infection)
inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_r.df)
inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_r.df)
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

SD_diag.df = read_csv("/Users/kevinnguyen/Downloads/sd_newDx_race_realloc.csv") %>% 
  rename(year = `Diagnosis year`,
         race = `Exposure Category`,
         mean_diag = n_adj) %>%
  mutate(sd_diag = 0) %>%
  filter(year %in% c(2019:2021)) %>%
  mutate(race = case_when(race == "Black" ~ "black",
                          race == "Hispanic" ~ "hispanic",
                          race == "Other" ~ "other")) %>%
  select(year, race, mean_diag, sd_diag) %>%
  mutate(type = "Observed",
         sim_type = "Observed")

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_inf= mean(tot_infects),
            sd_inf = sd(tot_infects)) %>%
  mutate(type = "Simulation")

inf_time_demo_sum_plot.df = inf_time_demo_sum_mean.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

## other
race = "other"

FOLDER_NAMES = list("gen_O9Y9", "_O1Y7", "_O2Y5", "_O3Y8", 
                    "_O4Y6", "_O5Y5", "_O6Y1", "_O7Y6", "_O8Y9", 
                    "_O9Y8", "_O10Y2")
### diag
diag_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    diag_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    diag_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    diag_time_demo_sum_all_e_r.df = bind_rows(diag_time_demo_sum_all_e_r.df,
                                              diag_time_demo_sum_all_e_temp_r.df)
  }
}

### time tree (infection)
inf_time_demo_sum_all_e_r.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O9Y9"){
    inf_time_demo_sum_all_eg_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epigen_O9Y9_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_genetic_r')
  } else {
    inf_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/epi", folder, "_", race, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_r')
    
    inf_time_demo_sum_all_e_r.df = bind_rows(inf_time_demo_sum_all_e_r.df,
                                             inf_time_demo_sum_all_e_temp_r.df)
  }
}

# combining all diag
diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e_ni.df, diag_time_demo_sum_all_e_r.df)
diag_time_demo_sum_all_eg.df = bind_rows(diag_time_demo_sum_all_eg_ni.df, diag_time_demo_sum_all_eg_r.df)
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

# combining all time tree (infection)
inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_r.df)
inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_r.df)
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

SD_diag.df = read_csv("/Users/kevinnguyen/Downloads/sd_newDx_race_realloc.csv") %>% 
  rename(year = `Diagnosis year`,
         race = `Exposure Category`,
         mean_diag = n_adj) %>%
  mutate(sd_diag = 0) %>%
  filter(year %in% c(2019:2021)) %>%
  mutate(race = case_when(race == "Black" ~ "black",
                          race == "Hispanic" ~ "hispanic",
                          race == "Other" ~ "other")) %>%
  select(year, race, mean_diag, sd_diag) %>%
  mutate(type = "Observed",
         sim_type = "Observed")

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(race, year, sim_type) %>%
  summarize(mean_inf= mean(tot_infects),
            sd_inf = sd(tot_infects)) %>%
  mutate(type = "Simulation")

inf_time_demo_sum_plot.df = inf_time_demo_sum_mean.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))
