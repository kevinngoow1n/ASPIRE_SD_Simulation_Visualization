library(gtools)
library(ensurer)
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
  # '/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention/output_simData_epi_O1Y6'
  for (simObj_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention/output_simObj_epi", folder_name))) { 
    simObj = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention/output_simObj_epi", folder_name, "/", simObj_rds_file))
    diag_time_demo.df = left_join(
      simObj$diag_time %>% filter(event == "diagnosis"),
      bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                simObj$popdf_dead),
      by = join_by(ID == id))
    
    diag_time_demo.df = diag_time_demo.df %>%
      mutate(year = ceiling(month/12)+2018)
    
    diag_time_demo_sum.df = diag_time_demo.df %>%
      group_by(month, risk, year) %>%
      summarize(diag = n()) %>%
      ungroup() %>%
      group_by(risk, year) %>%
      summarize(tot_diag = sum(diag))
    
    diag_time_demo_sum.df$file = simObj_rds_file
    
    diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all.df,
                                          diag_time_demo_sum.df)
    
    diag_time_demo_sum_all.df = diag_time_demo_sum_all.df %>%
      arrange(year)
    
    trans_tree_demo.df = left_join(simObj$trans_tree, bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                                                                simObj$popdf_dead) %>% select(id, risk),
                                   by = join_by(ID2 == id))
    
    trans_tree_demo.df = trans_tree_demo.df %>%
      mutate(year = ceiling(month/12)+2018)
    
    trans_tree_demo_sum.df = trans_tree_demo.df %>%
      group_by(month, risk, year) %>%
      summarize(infects = n()) %>%
      ungroup() %>%
      group_by(risk, year) %>%
      summarize(tot_infects = sum(infects))
    
    trans_tree_demo_sum.df$file = simObj_rds_file
    
    trans_tree_demo_sum_all.df = bind_rows(trans_tree_demo_sum_all.df,
                                           trans_tree_demo_sum.df)
  }
  
  #simData files
  for (simData_rds_file in list.files(paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention/output_simData_epi", folder_name))) { 
    simData_list = read_rds(paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention/output_simData_epi", folder_name, "/", simData_rds_file))
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
          paste("/Volumes/SSK Media/output_epi_epigen_Nov2025/rds_files_no_intervention/epi", folder_name, "_ni_risk_iter20_diag.rds", sep = "")
  )
  saveRDS(trans_tree_demo_sum_all.df,
          paste("/Volumes/SSK Media/output_epi_epigen_Nov2025/rds_files_no_intervention/epi", folder_name, "_ni_risk_iter20_infects.rds", sep = "")
  )
  saveRDS(simData_sum_all.df,
          paste("/Volumes/SSK Media/output_epi_epigen_Nov2025/rds_files_no_intervention/epi", folder_name, "_ni_risk_iter20_suppress_prop.rds", sep = "")
  )
}

# combining all files by epi/epigen & intervention
# no intervention
TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES = list(
  "gen_O9Y9_ni", "_O1Y7_ni", "_O2Y5_ni", "_O3Y8_ni", "_O4Y6_ni", "_O5Y5_ni", 
  "_O6Y1_ni", "_O7Y6_ni", "_O9Y8_ni", "_O8Y9_ni", "_O10Y2_ni")

## diag no intervention
diag_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O9Y9_ni"){
    diag_time_demo_sum_all_eg_ni.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/risk/epigen_O9Y9_ni_iter20_diag.rds") %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    diag_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/risk/epi", folder_file_no_intervention, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi')
    
    diag_time_demo_sum_all_e_ni.df = bind_rows(diag_time_demo_sum_all_e_ni.df,
                                            diag_time_demo_sum_all_e_temp_ni.df)
  }
}

## time tree (infection) no intervention
inf_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O9Y9_ni"){
    inf_time_demo_sum_all_eg_ni.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/risk/epigen_O9Y9_ni_iter20_infects.rds") %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    inf_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/risk/epi", folder_file_no_intervention, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi')
    
    inf_time_demo_sum_all_e_ni.df = bind_rows(inf_time_demo_sum_all_e_ni.df,
                                              inf_time_demo_sum_all_e_temp_ni.df)
  }
}

## diag  intervention
TEMPORARY_INTERVENTION_FOLDER_FILE_NAMES = list(
  "gen_O9Y9_i", "_O1Y7_i", "_O2Y5_i", "_O3Y8_i", "_O4Y6_i", "_O5Y5_i", 
  "_O6Y1_i", "_O7Y6_i", "_O9Y8_i", "_O8Y9_i", "_O10Y2_i")

diag_time_demo_sum_all_e_i.df = NULL

for (folder_file_intervention in TEMPORARY_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_intervention == "gen_O9Y9_i"){
    diag_time_demo_sum_all_eg_i.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epigen_O9Y9_i_iter20_diag.rds") %>%
      mutate(sim_type = 'epi_genetic_i')
  } else {
    diag_time_demo_sum_all_e_temp_i.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epi", folder_file_intervention, "_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi_i')
    
    diag_time_demo_sum_all_e_i.df = bind_rows(diag_time_demo_sum_all_e_i.df,
                                               diag_time_demo_sum_all_e_temp_i.df)
  }
}

## time tree (infection) no intervention
inf_time_demo_sum_all_e_i.df = NULL

for (folder_file_intervention in TEMPORARY_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_intervention == "gen_O9Y9_i"){
    inf_time_demo_sum_all_eg_i.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epigen_O9Y9_i_iter20_infects.rds") %>%
      mutate(sim_type = 'epi_genetic_i')
  } else {
    inf_time_demo_sum_all_e_temp_i.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epi", folder_file_intervention, "_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi_i')
    
    inf_time_demo_sum_all_e_i.df = bind_rows(inf_time_demo_sum_all_e_i.df,
                                              inf_time_demo_sum_all_e_temp_i.df)
  }
}

# combining all diag
diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e_ni.df, diag_time_demo_sum_all_e_i.df)
diag_time_demo_sum_all_eg.df = bind_rows(diag_time_demo_sum_all_eg_ni.df, diag_time_demo_sum_all_eg_i.df)
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

# combining all time tree (infection)
inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_i.df)
inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_i.df)
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

SD_diag.df = read_csv("/Users/kevinnguyen/Downloads/SDPH_Data_Processing_Scripts-calc_abm_inputs/output/sd_newDx_risk_realloc.csv") %>%
  rename(year = `Diagnosis year`,
         risk = `Exposure Category`,
         mean_diag = n_adj) %>%
  mutate(sd_diag = 0) %>%
  filter(year %in% c(2019:2021)) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSM & IDU" ~ "MSMandIDU",
                          risk == "Other" ~ "other")) %>%
  select(year, risk, mean_diag, sd_diag) %>%
  mutate(type = "Observed",
         sim_type = "Observed")

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type) %>%
  summarize(mean_inf= mean(tot_infects),
            sd_inf = sd(tot_infects)) %>%
  mutate(type = "Simulation")

inf_time_demo_sum_plot.df = inf_time_demo_sum_mean.df %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

#####Slide 34

fig_diag_time = ggplot(diag_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_diag, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_diag-1.96*sd_diag, ymax=mean_diag+1.96*sd_diag), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Diagnoses Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_diag_time

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_diag_time_epi_epigenetic.svg",
        width = 10, height = 5)
fig_diag_time
dev.off()

#####Slide 36

fig_inf_time = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_inf, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_inf-1.96*sd_inf, ymax=mean_inf+1.96*sd_inf), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Infections Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_time

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_inf_time_epi_epigenetic.svg",
        width = 10, height = 5)
fig_inf_time
dev.off()

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_i", "epi_genetic_i"))

inf_time_demo_sum_plot_TEMP.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP.df$sim_type, levels=c("Observed", "epi_i", "epi_genetic_i"))

#####Slide 37

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_inf, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_inf-1.96*sd_inf, ymax=mean_inf+1.96*sd_inf), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_i", "epi_genetic_i"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Infections Over Time\nStratified by Transmission Risk: Intervention") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_time_int

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_inf_time_epi_epigenetic_int.svg",
        width = 10, height = 5)
fig_inf_time_int
dev.off()

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  filter(year %in% c(2024,2025,2026,2027,2028,2029, 2030)) %>%
  summarize(sum_inf= sum(sum_inf)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_i)/epi_genetic * 100,
         epi_reduce = (epi-epi_i)/epi * 100) %>%
  select(risk, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_i)/epi_genetic * 100,
         epi_reduce = (epi-epi_i)/epi * 100) %>%
  select(risk, year, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean.df %>%
  pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "reduce")

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean_l.df %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other")) %>%
  mutate(Simulation = case_when(type == "epi_reduce" ~ "Sim (E)",
                                type == "epi_genetic_reduce" ~ "Sim (E+G)"))

#####Slide 38

inf_time_demo_sum_mean_l.df$Simulation <- factor(inf_time_demo_sum_mean_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))


fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_l.df
                            %>% filter(risk %in% c("MSM", "MSM & IDU"))
                            , aes(x=Simulation, y=reduce, fill=Simulation)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  #geom_line(position=pd) +
  #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("Reduction in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Reduction in New Infections \nStratified by Transmission Risk: Intervention \n(Increase in PrEP among MSM)") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 1, scales = "free")

fig_inf_reduce_int

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_inf_time_epi_epigenetic_reduce_int.svg",
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_l.df, aes(x=year, y=reduce, colour=Simulation, group=Simulation)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Reduction in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Reduction in New Infections \nStratified by Transmission Risk: Intervention \n(Increase in PrEP among MSM)") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_reduce_int

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_inf_time_epi_epigenetic_reduce_int_time.svg",
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

###################

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

#####Slide 34a

fig_diag_time = ggplot(diag_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_diag, colour=sim_type)) +
  geom_errorbar(aes(ymin=mean_diag-1.96*sd_diag, ymax=mean_diag+1.96*sd_diag), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Diagnoses Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_diag_time

svglite(filename = "/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/fig_diag_time_epi_epigenetic_all.svg",
        width = 10, height = 5)
fig_diag_time
dev.off()