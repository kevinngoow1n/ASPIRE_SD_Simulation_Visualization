race_list = list("black", "hispanic", "other")

FOLDER_NAMES = list("gen_O9Y9", "_O1Y7", "_O2Y5", "_O3Y8", 
                    "_O4Y6", "_O5Y5", "_O6Y1", "_O7Y6", "_O8Y9", 
                    "_O9Y8", "_O10Y2")

for (race in race_list){
  ### time tree (infection)
  inf_time_demo_sum_all_e_r.df = NULL
  
  for (folder in FOLDER_NAMES){
    if (folder == "gen_O9Y9"){
      inf_time_demo_sum_all_eg_r.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Sep2025/epigen_O9Y9_", race, "_iter20_infects.rds")) %>%
        mutate(sim_type = 'epi_genetic_r')
    } else {
      inf_time_demo_sum_all_e_temp_r.df = readRDS(paste0("/Volumes/SSK Media/output_epi_epigen_Sep2025/epi", folder, "_", race, "_iter20_infects.rds")) %>%
        mutate(sim_type = 'epi_r')
      
      inf_time_demo_sum_all_e_r.df = bind_rows(inf_time_demo_sum_all_e_r.df,
                                               inf_time_demo_sum_all_e_temp_r.df)
    }
  }
  
  # combining all time tree (infection)
  inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e_ni.df, inf_time_demo_sum_all_e_r.df)
  inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg_ni.df, inf_time_demo_sum_all_eg_r.df)
  inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)
  
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
  inf_time_demo_sum_mean.df$intervention = race
  
  mean(inf_time_demo_sum_mean.df$epi_genetic_proport)
  mean(inf_time_demo_sum_mean.df$epi_genetic_proport_r)
  
  if (race == "black"){
    inf_time_demo_sum_mean_black.df = inf_time_demo_sum_mean.df # requires for race to be changed to be "black"
  } else if (race == "hispanic"){
    inf_time_demo_sum_mean_hispanic.df = inf_time_demo_sum_mean.df # requires for race to be changed to be "hispanic"
  } else {
    inf_time_demo_sum_mean_other.df = inf_time_demo_sum_mean.df # requires for race to be changed to be "other"
  }
}

inf_time_demo_sum_mean.df = bind_rows(inf_time_demo_sum_mean_other.df, inf_time_demo_sum_mean_black.df, inf_time_demo_sum_mean_hispanic.df)

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

inf_time_demo_sum_mean_l.df$Simulation <- factor(inf_time_demo_sum_mean_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))

### increase
inf_time_demo_sum_mean_increase.df = inf_time_demo_sum_mean.df %>%
  select(race, year, epi_increase, epi_genetic_increase, intervention)

inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase.df %>%
  pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "increase")

inf_time_demo_sum_mean_increase_l.df = inf_time_demo_sum_mean_increase_l.df %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "hispanic" ~ "Hispanic",
                          race == "other" ~ "Other")) %>%
  mutate(Simulation = case_when(type == "epi_increase" ~ "Sim (E)",
                                type == "epi_genetic_increase" ~ "Sim (E+G)")) %>%
  mutate(intervention = case_when(intervention == "black" ~ "Black",
                                  intervention == "hispanic" ~ "Hispanic",
                                  intervention == "other" ~ "Other"))

inf_time_demo_sum_mean_increase_l.df$Simulation <- factor(inf_time_demo_sum_mean_increase_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))

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
  ggtitle(paste0("Change in New Infections \nStratified by Race")) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(limits = c(0, 100)) +         # Setting max and min y values
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(race ~ intervention, scales = "free") + 
  scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_reduce_int
#svglite(filename = paste0("/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_change_int_time_race.svg"),
        #width = 10, height = 5)
ggsave(filename = "/Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_change_int_time_race.svg", plot = fig_inf_reduce_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_reduce_int
dev.off()

# Bar Chart of Same Data
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
  labs(subtitle = "Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Change in New Infections \nStratified by Race\n(", race, " does not have access to ART)")) +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(limits = c(0, 100)) +         # Setting max and min y values
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom",
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL))

fig_inf_reduce_int

svglite(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Sep2025/fig_inf_time_epi_epigenetic_change_int_", race, ".svg"),
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()
