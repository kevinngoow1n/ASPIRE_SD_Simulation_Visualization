# December 15, 2025 Data Visualization Request 
# (Analysis of why epigenetic values are lower in change than epi)

# combining no intervention and intervention infections
# no infections
FOLDER_NAMES = list("gen_O5Y5", "_O1Y6", "_O2Y9", "_O3Y2", "_O4Y10", 
                    "_O5Y4", "_O6Y1", "_O7Y6", "_O9Y8", "_O8Y2", "_O10Y9")

### time tree (infection)
inf_time_demo_sum_all_e_ni.df = NULL

for (folder in FOLDER_NAMES){
  if (folder == "gen_O5Y5"){
    inf_time_demo_sum_all_eg_ni.df = readRDS(paste0('/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epigen_O5Y5_ni_race_iter20_infects.rds')) %>%
      mutate(sim_type = 'epi_genetic') %>%
      mutate(sim_folder = folder)
  } else {
    inf_time_demo_sum_all_e_temp_ni.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/no_intervention_RACE_rds_files/epi", folder, "_ni_race_iter20_infects.rds")) %>%
      mutate(sim_type = 'epi') %>%
      mutate(sim_folder = folder)
    
    inf_time_demo_sum_all_e_ni.df = bind_rows(inf_time_demo_sum_all_e_ni.df,
                                              inf_time_demo_sum_all_e_temp_ni.df)
  }
}

# race
#race = "other" # change "black", "hispanic", "other"

race_list = list("black", "hispanic", "other")

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

## black intervention black race epi only + add MSM Assortativity by Risk
epi_folders = unique(na.omit(inf_time_demo_sum_plot_TOTAL.df$sim_folder)[na.omit(inf_time_demo_sum_plot_TOTAL.df$sim_folder) != "gen_O5Y5"])

for (i in epi_folders) {
  simulation_split = inf_time_demo_sum_plot_TOTAL.df %>%
    filter(sim_folder %in% c(i, NA))
  
  inf_time_demo_sum_mean.df_BlackrBlacki = simulation_split %>%
    filter(race %in% "Black") %>%
    filter(intervention %in% "Black") %>%
    group_by(race, sim_type, year, intervention) %>%
    summarize(sum_inf= mean(tot_infects)) %>%
    pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
    mutate(epi_reduce = (epi-epi_r)/epi * 100,
           epi_increase = (epi_r-epi)/epi * 100)
  
  ### increase
  inf_time_demo_sum_mean_increase.df = inf_time_demo_sum_mean.df_BlackrBlacki %>%
    select(race, year, epi_increase, intervention)
  
  inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase.df %>%
    pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "increase")
  
  inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase_l.df %>%
    # mutate(race = case_when(race == "black" ~ "Black",
    #                         race == "hispanic" ~ "Hispanic",
    #                         race == "other" ~ "Other")) %>%
    mutate(Simulation = case_when(type == "epi_increase" ~ "Sim (E)"))
  
  #####Slide 38 Intervention
  inf_time_demo_sum_mean_increase_l.df$Simulation <- factor(inf_time_demo_sum_mean_increase_l.df$Simulation, levels=c("Sim (E)"))
  
  
  fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_increase_l.df #inf_time_demo_sum_mean_l.df
                              %>% filter(race %in% c("Black"))
                              , aes(x=Simulation, y=increase, fill=Simulation)) + # change y = REDUCE OR INCREASE
    geom_bar(stat = "identity", position=position_dodge()) +
    #geom_line(position=pd) +
    #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
    xlab("") +
    ylab("Change in New Infections") +
    labs(subtitle = "Infections") +
    scale_colour_hue(name="Data",    # Legend label, use darker colors
                     #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                     #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                     breaks=c("Sim (E)"),
                     labels=c("Sim (E)"),
                     l=40) +                    # Use darker colors, lightness=40
    ggtitle(paste0("Change in New Infections\n Simulation:", i)) +
    expand_limits(y=0) +                        # Expand y range
    #scale_y_continuous(limits = c(0, 100)) +         # Setting max and min y values
    theme_bw() +
    theme(legend.justification=c(1,0),
          #legend.position=c(1,0)
          legend.position = "bottom",
          plot.subtitle = element_text(hjust = 0.5)) +
    facet_grid(intervention~race, scales = "free")  + 
    scale_y_continuous(limits = c(-50, 100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL))
  
  fig_inf_reduce_int
  
  # svglite(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/fig_inf_time_epi_epigenetic_change_int", i, "_pos_neg.svg"),
  #         width = 10, height = 5)
  ggsave(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_inf_time_epi_epigenetic_change_int", i, "_pos_neg.svg"),
         width = 10, height = 5, plot = fig_inf_reduce_int)
  fig_inf_reduce_int
  dev.set(dev.next())
  dev.off()
}

## black intervention black race but years change 0-1, 0-2, 0-3, 0-4...0-10
inf_blk_rce_yr_chnge = inf_time_demo_sum_plot_TOTAL.df_black %>%
  filter(race %in% "Black")

starting_year = min(inf_blk_rce_yr_chnge$year)
ending_year = max(inf_blk_rce_yr_chnge$year)
difference_year = ending_year - starting_year

for (i in 1:difference_year){
  max_year = starting_year + i
  inf_yr_rnge_df = inf_blk_rce_yr_chnge %>%
    filter(year <= max_year) %>%
    group_by(race, sim_type, year, intervention) %>%
    summarize(sum_inf= mean(tot_infects)) %>%
    pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
    mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_r)/epi_genetic * 100,
           epi_reduce = (epi-epi_r)/epi * 100,
           epi_genetic_increase = (epi_genetic_r-epi_genetic)/epi_genetic * 100,
           epi_increase = (epi_r-epi)/epi * 100,
           epi_genetic_proport = (epi-epi_genetic) / epi,
           epi_genetic_proport_r = (epi_r-epi_genetic_r) / epi_r)
  
  mean(inf_yr_rnge_df$epi_genetic_proport)
  mean(inf_yr_rnge_df$epi_genetic_proport_r)
  
  ### increase
  inf_time_demo_sum_mean_increase.df = inf_yr_rnge_df %>%
    select(race, year, epi_increase, epi_genetic_increase, intervention)
  
  inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase.df %>%
    pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "increase")
  
  inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase_l.df %>%
    # mutate(race = case_when(race == "black" ~ "Black",
    #                         race == "hispanic" ~ "Hispanic",
    #                         race == "other" ~ "Other")) %>%
    mutate(Simulation = case_when(type == "epi_increase" ~ "Sim (E)",
                                  type == "epi_genetic_increase" ~ "Sim (E+G)"))
  
  #####Slide 38 Intervention
  inf_time_demo_sum_mean_increase_l.df$Simulation <- factor(inf_time_demo_sum_mean_increase_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))
  
  
  fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_increase_l.df #inf_time_demo_sum_mean_l.df
                              %>% filter(race %in% c("Black"))
                              , aes(x=Simulation, y=increase, fill=Simulation)) + # change y = REDUCE OR INCREASE
    geom_bar(stat = "identity", position=position_dodge()) +
    #geom_line(position=pd) +
    #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
    xlab("") +
    ylab("Change in New Infections") +
    labs(subtitle = "Infections") +
    scale_colour_hue(name="Data",    # Legend label, use darker colors
                     #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                     #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                     breaks=c("Sim (E)", "Sim (E+G)"),
                     labels=c("Sim (E)", "Sim (E+G)"),
                     l=40) +                    # Use darker colors, lightness=40
    ggtitle(paste0("Change in New Infections \n Years:", starting_year, " to ", max_year)) +
    expand_limits(y=0) +                        # Expand y range
    #scale_y_continuous(limits = c(0, 100)) +         # Setting max and min y values
    theme_bw() +
    theme(legend.justification=c(1,0),
          #legend.position=c(1,0)
          legend.position = "bottom",
          plot.subtitle = element_text(hjust = 0.5)) +
    facet_grid(race~intervention, scales = "free")  + 
    scale_y_continuous(limits = c(-50, 100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL))
  
  fig_inf_reduce_int
  
  # svglite(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/fig_inf_time_epi_epigenetic_change_int_", race, "_pos_neg.svg"),
  #         width = 10, height = 5)
  ggsave(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_inf_time_epi_epigenetic_change_int_", starting_year,"_", max_year, "_pos_neg.svg"),
         width = 10, height = 5, plot = fig_inf_reduce_int)
  fig_inf_reduce_int
  dev.set(dev.next())
  dev.off()
}
