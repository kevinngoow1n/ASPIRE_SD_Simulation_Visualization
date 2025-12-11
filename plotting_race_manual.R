race = "black"
# combining all files by epi/epigen & intervention
# no intervention
TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES = list(
  "gen_O9Y9_ni", "_O1Y7_ni", "_O2Y5_ni", "_O3Y8_ni", "_O4Y6_ni", "_O5Y5_ni", 
  "_O6Y1_ni", "_O7Y6_ni", "_O9Y8_ni", "_O8Y9_ni", "_O10Y2_ni")

## diag no intervention
diag_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O9Y9_ni"){
    diag_time_demo_sum_all_eg_ni.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epigen_O9Y9_ni_race_iter20_diag.rds") %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    diag_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epi", folder_file_no_intervention, "_race_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi')
    
    diag_time_demo_sum_all_e_ni.df = bind_rows(diag_time_demo_sum_all_e_ni.df,
                                               diag_time_demo_sum_all_e_temp_ni.df)
  }
}

## time tree (infection) no intervention
inf_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O9Y9_ni"){
    inf_time_demo_sum_all_eg_ni.df = readRDS("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epigen_O9Y9_ni_race_iter20_infects.rds") %>%
      mutate(sim_type = 'epi_genetic')
  } else {
    inf_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_Aug2025/epi", folder_file_no_intervention, "_race_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi')
    
    inf_time_demo_sum_all_e_ni.df = bind_rows(inf_time_demo_sum_all_e_ni.df,
                                              inf_time_demo_sum_all_e_temp_ni.df)
  }
}

# combining race files by epi/epigen
FOLDER_NAMES = list("gen_O9Y9", "_O1Y7", "_O2Y5", "_O3Y8", 
                    "_O4Y6", "_O5Y5", "_O6Y1", "_O7Y6", "_O8Y9", 
                    "_O9Y8", "_O10Y2")
## diag
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

## time tree (infection)
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

#####Slide 34 No Intervention

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
  ggtitle(paste0("Number of New Diagnoses Over Time\nStratified by Race\nNo Intervention")) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 3, scales = "free")

fig_diag_time

svglite(filename = paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/figures/fig_diag_time_epi_epigenetic.svg"),
        width = 10, height = 5)
fig_diag_time
dev.off()

#####Slide 36 No Intervention

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
  ggtitle(paste0("Number of New Infections Over Time\nStratified by Race\nNo Intervention")) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 2, scales = "free")

fig_inf_time

svglite(filename = paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/figures/fig_inf_time_epi_epigenetic.svg"),
        width = 10, height = 5)
fig_inf_time
dev.off()

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_r", "epi_genetic_r"))

inf_time_demo_sum_plot_TEMP.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP.df$sim_type, levels=c("Observed", "epi_r", "epi_genetic_r"))

#####Slide 37 Intervention

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_inf, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_inf-1.96*sd_inf, ymax=mean_inf+1.96*sd_inf), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time\nStratified by Race\nIntervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 2, scales = "free")

fig_inf_time_int

svglite(filename = paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/figures/fig_inf_time_epi_epigenetic_int_", race, ".svg"),
        width = 10, height = 5)
fig_inf_time_int
dev.off()

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(race, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  filter(year %in% c(2024,2025,2026,2027,2028,2029, 2030)) %>% #### same as 237 just no this line
  summarize(sum_inf= sum(sum_inf)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_r)/epi_genetic * 100,
         epi_reduce = (epi-epi_r)/epi * 100) %>%
  select(race, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(race, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_r)/epi_genetic * 100,
         epi_reduce = (epi-epi_r)/epi * 100,
         epi_genetic_increase = (epi_genetic_r-epi_genetic)/epi_genetic * 100,
         epi_increase = (epi_r-epi)/epi * 100,
         epi_genetic_proport = (epi-epi_genetic) / epi,
         epi_genetic_proport_r = (epi_r-epi_genetic_r) / epi_r)

mean(inf_time_demo_sum_mean.df$epi_genetic_proport)
mean(inf_time_demo_sum_mean.df$epi_genetic_proport_r)

### decrease
inf_time_demo_sum_mean_reduce.df = inf_time_demo_sum_mean.df %>%
  select(race, year, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean_reduce.df %>%
  pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "reduce")

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean_l.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other")) %>%
  mutate(Simulation = case_when(type == "epi_reduce" ~ "Sim (E)",
                                type == "epi_genetic_reduce" ~ "Sim (E+G)"))

### increase
inf_time_demo_sum_mean_increase.df = inf_time_demo_sum_mean.df %>%
  select(race, year, epi_increase, epi_genetic_increase)

inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase.df %>%
  pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "increase")

inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase_l.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other")) %>%
  mutate(Simulation = case_when(type == "epi_increase" ~ "Sim (E)",
                                type == "epi_genetic_increase" ~ "Sim (E+G)"))

#####Slide 38 Intervention

inf_time_demo_sum_mean_l.df$Simulation <- factor(inf_time_demo_sum_mean_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))
inf_time_demo_sum_mean_increase_l.df$Simulation <- factor(inf_time_demo_sum_mean_increase_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))


fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_increase_l.df #inf_time_demo_sum_mean_l.df
                            %>% filter(race %in% c("Black", "Hispanic", "Other"))
                            , aes(x=Simulation, y=increase, fill=Simulation)) + # change y = REDUCE OR INCREASE
  geom_bar(stat = "identity", position=position_dodge()) +
  #geom_line(position=pd) +
  #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("Change in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Change in New Infections \nStratified by Race\n(", race, " does not have access to ART)")) +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits = c(0, 100)) +         # Setting max and min y values
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 1, scales = "free")

fig_inf_reduce_int

svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_change_int_", race, ".svg"),
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_increase_l.df, aes(x=year, y=increase, colour=Simulation, group=Simulation)) + #inf_time_demo_sum_mean_l.df & change y = REDUCE OR INCREASE
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Change in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Change in New Infections \nStratified by Race: ", race)) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 2, scales = "free")

fig_inf_reduce_int

svglite(filename = paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/figures/fig_inf_time_epi_epigenetic_change_int_time_", race, ".svg"),
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

###################

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

#####Slide 34a No Intervention

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
  ggtitle(paste0("Number of New Diagnoses Over Time\nStratified by Race\nNo Intervention")) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~race, nrow = 2, scales = "free")

fig_diag_time

svglite(filename = paste0("/Users/kevinnguyen/Downloads/output_epi_epigen_race_Aug2025/figures/fig_diag_time_epi_epigenetic_all.svg"),
        width = 10, height = 5)
fig_diag_time
dev.off()
