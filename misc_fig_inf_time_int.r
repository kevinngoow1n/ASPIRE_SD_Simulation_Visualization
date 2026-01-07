#6 diagnosis plot to csv with 2.5%, 25%, 75%, & 97.5%
race_diag = diag_time_demo_sum_plot_TEMP.df %>% dplyr::filter(year < 2022)
race_diag$y2.5 = race_diag$mean_diag - (1.96 * race_diag$sd_diag)
race_diag$y97.5 = race_diag$mean_diag + (1.96 * race_diag$sd_diag)
race_diag$y25 = race_diag$mean_diag - (1.15 * race_diag$sd_diag)
race_diag$y75 = race_diag$mean_diag + (1.15 * race_diag$sd_diag)

risk_diag = diag_time_demo_sum_plot_TEMP.df %>% dplyr::filter(year < 2022)
risk_diag$y2.5 = risk_diag$mean_diag - (1.96 * risk_diag$sd_diag)
risk_diag$y97.5 = risk_diag$mean_diag + (1.96 * risk_diag$sd_diag)
risk_diag$y25 = risk_diag$mean_diag - (1.15 * risk_diag$sd_diag)
risk_diag$y75 = risk_diag$mean_diag + (1.15 * risk_diag$sd_diag)

write_csv(race_diag, file = "/Users/kevinnguyen/Downloads/race_diag.csv")
write_csv(risk_diag, file = "/Users/kevinnguyen/Downloads/risk_diag.csv")

#5 bar chart change y axis limits to 0% to 100%
inf_time_demo_sum_mean.df = inf_time_demo_sum_plot_TOTAL.df %>%
  group_by(race, sim_type, year, intervention) %>%
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
  ggtitle(paste0("Change in New Infections \nStratified by Race")) +
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

svglite(filename = paste0("/Volumes/SSK Media/output_epi_epigen_Dec2025/fig_inf_time_epi_epigenetic_change_int_", race, "_pos_neg.svg"),
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

#1 mean epi + epigen [2 lines per graph, column: black infections, hispanic infections, other infections, row: black intervention, hispanic intervention, and other intervention]
## refer to /Users/kevinnguyen/Downloads/fig_inf_time_epi_epigenetic_change_int_time_race_mean_epi_epigen.r

#4 total epi + epigen [1100 lines per graph, column: black infections, hispanic infections, other infections, row: black intervention, hispanic intervention, and other intervention]


# new infections over time by race no intervention
# epigen
inf_time_demo_sum_plot_TOTAL.df_black = inf_time_demo_sum_plot_TOTAL.df %>%
  filter(intervention == "Black") %>%
  filter(sim_type %in% c("Observed", "epi_genetic"))
  
fig_inf_time_noint = ggplot(inf_time_demo_sum_plot_TOTAL.df_black, aes(x=year, y=tot_infects, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_genetic"),
                   labels=c("Obs", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E+G)\nStratified by Race\nNo Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 800), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_noint

#downloading plot
ggsave(filename = "/Users/kevinnguyen/Downloads/fig_inf_time_epigenetic_ni_epigen_NoIntervention.svg", plot = fig_inf_time_noint, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_noint
dev.off()

# new infections over time by race black intervention
# epigen
inf_time_demo_sum_plot_TEMP.df_black = inf_time_demo_sum_plot_TEMP.df %>%
  filter(intervention == "Black") %>%
  filter(sim_type %in% c("Observed", "epi_genetic_r"))
fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP.df_black, aes(x=year, y=tot_infects, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E+G)\nStratified by Race\nIntervention")) + 
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")  + 
  scale_y_continuous(limits = c(0, 800), sec.axis = sec_axis(~ . , name = "Intervention", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Infections", breaks = NULL, labels = NULL))

fig_inf_time_int

#downloading plot
ggsave(filename = "/Users/kevinnguyen/Downloads/fig_inf_time_epigenetic_int_epigen_BlackIntervention.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()
