# no infections
FOLDER_NAMES = list("gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
  "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9")

### time tree (infection)
inf_time_demo_sum_all_e_ni.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O5Y5"){
    inf_time_demo_sum_all_eg_ni.df = readRDS(paste0('/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention_RACE_rds_files/epigen_O5Y5_ni_race_iter20_infects.rds')) %>%
      mutate(sim_type = 'epi_genetic') %>%
      mutate(sim_folder = folder)
  } else {
    inf_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/no_intervention_RACE_rds_files/epi", folder, "_ni_race_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi') %>%
      mutate(sim_folder = folder)
      
    inf_time_demo_sum_all_e_ni.df = bind_rows(inf_time_demo_sum_all_e_ni.df,
                                               inf_time_demo_sum_all_e_temp_ni.df)
  }
}

# race
#race = "other" # change "black", "hispanic", "other"

race_list = list("black", "hispanic", "other")

FOLDER_NAMES = list("gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
                    "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9")

for (race in race_list){
  ### time tree (infection)
  inf_time_demo_sum_all_e_r.df = NULL
  
  for (folder in FOLDER_NAMES){
    if (folder == "gen_O5Y5"){
      inf_time_demo_sum_all_eg_r.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_RACE_rds_files/epigen_O5Y5_", race, "_iter20_infects.rds")) %>%
        mutate(sim_type = 'epi_genetic_r') %>%
        mutate(sim_folder = folder)
    } else {
      inf_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/intervention_RACE_rds_files/epi", folder, "_", race, "_iter20_infects.rds")) %>%
        mutate(sim_type = 'epi_r') %>%
        mutate(sim_folder = folder)
      
      inf_time_demo_sum_all_e_r.df = bind_rows(inf_time_demo_sum_all_e_r.df,
                                               inf_time_demo_sum_all_e_temp_r.df)
    }
  }
  
  # combining all time tree (infection)
  inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_r.df)
  inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_r.df)
  inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)
  
  inf_time_demo_sum_all.df$intervention = race
  
  inf_time_demo_sum_plot.df = inf_time_demo_sum_all.df %>%
    mutate(race = case_when(race == "black" ~ "Black",
                            race == "hispanic" ~ "Hispanic",
                            race == "other" ~ "Other")) %>%
    mutate(intervention = case_when(intervention == "black" ~ "Black",
                                    intervention == "hispanic" ~ "Hispanic",
                                    intervention == "other" ~ "Other"))
  
  # inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot_TEMP.df %>%
  #   mutate(intervention = case_when(intervention == "black" ~ "Black",
  #                                   intervention == "hispanic" ~ "Hispanic",
  #                                   intervention == "other" ~ "Other"))
  inf_time_demo_sum_plot_TOTAL.df = inf_time_demo_sum_plot.df
  
  inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
    filter(sim_type %in% c("Observed", "epi_r", "epi_genetic_r"))
  
  inf_time_demo_sum_plot_TEMP.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP.df$sim_type, levels=c("Observed", "epi_r", "epi_genetic_r"))
  
  pd <- position_dodge(0.1)
  
  print(race)
  
  if (race == "black"){
    inf_time_demo_sum_plot_TEMP.df_black = inf_time_demo_sum_plot_TEMP.df # requires for race to be changed to be "black"
  } else if (race == "hispanic"){
    inf_time_demo_sum_plot_TEMP.df_hispanic = inf_time_demo_sum_plot_TEMP.df # requires for race to be changed to be "hispanic"
  } else {
    inf_time_demo_sum_plot_TEMP.df_other = inf_time_demo_sum_plot_TEMP.df # requires for race to be changed to be "other"
  }
  
  if (race == "black"){
    inf_time_demo_sum_plot_TOTAL.df_black = inf_time_demo_sum_plot_TOTAL.df # requires for race to be changed to be "black"
  } else if (race == "hispanic"){
    inf_time_demo_sum_plot_TOTAL.df_hispanic = inf_time_demo_sum_plot_TOTAL.df # requires for race to be changed to be "hispanic"
  } else {
    inf_time_demo_sum_plot_TOTAL.df_other = inf_time_demo_sum_plot_TOTAL.df # requires for race to be changed to be "other"
  }
}

inf_time_demo_sum_plot_TEMP.df = bind_rows(inf_time_demo_sum_plot_TEMP.df_black, inf_time_demo_sum_plot_TEMP.df_hispanic, inf_time_demo_sum_plot_TEMP.df_other)
inf_time_demo_sum_plot_TOTAL.df = bind_rows(inf_time_demo_sum_plot_TOTAL.df_black, inf_time_demo_sum_plot_TOTAL.df_hispanic, inf_time_demo_sum_plot_TOTAL.df_other)

# epi + epigen
fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=tot_infects, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha = 0.2) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E) & Sim (E+G)\nStratified by Race\nIntervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 1500), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
        #width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_inf_time_epi_epigenetic_int_epi_epigen.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

# epi only
inf_time_demo_sum_plot_TEMP_epi.df = inf_time_demo_sum_plot_TEMP.df %>%
  filter(sim_type %in% c("epi_r"))

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP_epi.df, aes(x=year, y=tot_infects, colour=sim_type, group=file)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                  l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E)\nStratified by Race\nIntervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 3500), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
#width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Sep2025/fig_inf_time_epi_epigenetic_int_epi.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

# epigen only
inf_time_demo_sum_plot_TEMP_epigen.df = inf_time_demo_sum_plot_TEMP.df %>%
  filter(sim_type %in% c("epi_genetic_r"))

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP_epigen.df, aes(x=year, y=tot_infects, colour=sim_type, group=file)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E+G)\nStratified by Race\nIntervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 2100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
#width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Sep2025/fig_inf_time_epi_epigenetic_int_epigen.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

###***
# NO INFECTIONS
inf_time_demo_sum_all_e.df = inf_time_demo_sum_all_e_ni.df
inf_time_demo_sum_all_eg.df = inf_time_demo_sum_all_eg_ni.df
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

inf_time_demo_sum_plot.df = inf_time_demo_sum_all.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

# no infections (overall = summing all races together)
inf_time_demo_sum_plot_TEMP_ni.df = inf_time_demo_sum_plot.df %>%
  ungroup() %>%
  group_by(year, file, sim_type, sim_folder) %>%
  summarise(year_infects = sum(tot_infects)) %>%
  filter(sim_type %in% c("Observed", "epi", "epi_genetic"))

inf_time_demo_sum_plot_TEMP_ni.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP_ni.df$sim_type, levels=c("Observed", "epi", "epi_genetic"))

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP_ni.df, aes(x=year, y=year_infects, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E) & Sim (E+G)\nNo Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 
  #facet_wrap(~race, nrow = 2, scales = "free")
  #scale_y_continuous(limits = c(0, 2100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  #scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
#width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_inf_time_epi_epigenetic_noint_overall.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

# checking mean
inf_time_demo_sum_ni_mean.df = inf_time_demo_sum_plot_TEMP_ni.df %>%
  group_by(year, sim_type, sim_folder) %>%
  summarise(mean_infect = mean(year_infects)) %>%
  filter(sim_type %in% c("Observed", "epi", "epi_genetic"))

fig_inf_time_noint = ggplot(inf_time_demo_sum_ni_mean.df, aes(x=year, y=mean_infect, colour=sim_type, group=sim_folder)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Average of New Infections Over Time: Sim (E) & Sim (E+G)")) + 
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits = c(0, 1000)) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 

fig_inf_time_noint

ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/inf_fig_ni_simMean_epi_epigen.svg", plot = fig_inf_time_noint, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_noint
dev.off()
