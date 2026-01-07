# December 15, 2025 Data Visualization Request 
# (Analysis of why epigenetic values are lower in change than epi)

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
  ggsave(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/fig_inf_time_epi_epigenetic_change_int", i, "_pos_neg.svg"),
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
  ggsave(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Nov2025/fig_inf_time_epi_epigenetic_change_int_", starting_year,"_", max_year, "_pos_neg.svg"),
         width = 10, height = 5, plot = fig_inf_reduce_int)
  fig_inf_reduce_int
  dev.set(dev.next())
  dev.off()
}
