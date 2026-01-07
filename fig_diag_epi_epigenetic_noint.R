## no change
TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES = list(
  "gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
  "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9")

## diag no intervention
diag_time_demo_sum_all_e_ni.df = NULL

for (folder_file_no_intervention in TEMPORARY_NO_INTERVENTION_FOLDER_FILE_NAMES){
  if (folder_file_no_intervention == "gen_O5Y5"){
    diag_time_demo_sum_all_eg_ni.df = readRDS('/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epigen_O5Y5_ni_race_iter20_diag.rds') %>%
      mutate(sim_type = 'epi_genetic') %>%
      mutate(sim_folder = folder_file_no_intervention)
  } else {
    diag_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder_file_no_intervention, "_ni_race_iter20_diag.rds")) %>%
      mutate(sim_type = 'epi') %>%
      mutate(sim_folder = folder_file_no_intervention)
    
    diag_time_demo_sum_all_e_ni.df = bind_rows(diag_time_demo_sum_all_e_ni.df,
                                               diag_time_demo_sum_all_e_temp_ni.df)
  }
}
# NO INTERVENTION
diag_time_demo_sum_all_e.df = diag_time_demo_sum_all_e_ni.df
diag_time_demo_sum_all_eg.df = diag_time_demo_sum_all_eg_ni.df
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

diag_time_demo_sum_plot.df = diag_time_demo_sum_all.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other"))

# no diagnoses (overall = summing all races together)
diag_time_demo_sum_plot_TEMP_ni.df = diag_time_demo_sum_plot.df %>%
  ungroup() %>%
  group_by(year, file, sim_type, sim_folder) %>%
  summarise(year_diag = sum(tot_diag)) %>%
  filter(sim_type %in% c("Observed", "epi", "epi_genetic"))

diag_time_demo_sum_plot_TEMP_ni.df$sim_type <- factor(diag_time_demo_sum_plot_TEMP_ni.df$sim_type, levels=c("Observed", "epi", "epi_genetic"))

fig_diag_time_int = ggplot(diag_time_demo_sum_plot_TEMP_ni.df, aes(x=year, y=year_diag, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Diagnoses Over Time: Sim (E) & Sim (E+G)\nNo Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 
#facet_wrap(~race, nrow = 2, scales = "free")
#scale_y_continuous(limits = c(0, 2100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
#scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_diag_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
#width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_diag_time_epi_epigenetic_noint_overall.svg", plot = fig_diag_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_diag_time_int
dev.off()

# epigenetic only
diag_time_demo_sum_plot_TEMP_ni.df_epigenetic = diag_time_demo_sum_plot_TEMP_ni.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic"))

fig_diag_time_int = ggplot(diag_time_demo_sum_plot_TEMP_ni.df_epigenetic, aes(x=year, y=year_diag, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_genetic"),
                   labels=c("Obs", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Diagnoses Over Time: Sim (E+G)\nNo Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 
#facet_wrap(~race, nrow = 2, scales = "free")
#scale_y_continuous(limits = c(0, 2100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
#scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_diag_time_int

#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_int_epigen.svg"),
#width = 10, height = 5)
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_diag_time_epigenetic_noint_overall.svg", plot = fig_diag_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_diag_time_int
dev.off()

# mean
diag_time_demo_sum_ni_mean.df = diag_time_demo_sum_plot_TEMP_ni.df %>%
  group_by(year, sim_type, sim_folder) %>%
  summarise(mean_diag = mean(year_diag)) %>%
  filter(sim_type %in% c("Observed", "epi", "epi_genetic"))

fig_diag_time_noint = ggplot(diag_time_demo_sum_ni_mean.df, aes(x=year, y=mean_diag, colour=sim_type, group=sim_folder)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Average of New Diagnoses Over Time: Sim (E) & Sim (E+G)")) + 
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits = c(0, 1000)) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 

fig_diag_time_noint

ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Dec2025/diag_fig_ni_simMean_epi_epigen.svg", plot = fig_diag_time_noint, device = "svg", width = 10, height = 5, units = "in")
fig_diag_time_noint
dev.off()
