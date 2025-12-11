inf_time_demo_sum_plot_TEMP.df

# black intervention only
inf_time_demo_sum_plot_TEMP.df_black = inf_time_demo_sum_plot_TEMP.df %>%
  filter(intervention == "Black")

inf_time_demo_sum_plot_TEMP_iBLACK.df = inf_time_demo_sum_plot_TEMP.df_black %>%
  ungroup() %>%
  group_by(year, file, sim_type) %>%
  summarise(year_infects = sum(tot_infects)) %>%
  filter(sim_type %in% c("Observed", "epi_r", "epi_genetic_r"))

inf_time_demo_sum_plot_TEMP_iBLACK.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP_iBLACK.df$sim_type, levels=c("Observed", "epi_r", "epi_genetic_r"))

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP_iBLACK.df, aes(x=year, y=year_infects, colour=sim_type, group=file)) +
  geom_line(position=pd, alpha=0.2) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Number of New Infections Over Time: Sim (E) & Sim (E+G)\nBlack Intervention")) + 
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
ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Sep2025/fig_inf_time_epi_epigenetic_intBLACK_overall.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

# checking mean
inf_time_demo_sum_BLACK_mean.df = inf_time_demo_sum_plot_TEMP.df_black %>%
  group_by(year, sim_type) %>%
  summarise(mean_infects = mean(tot_infects)) %>%
  filter(sim_type %in% c("Observed", "epi_r", "epi_genetic_r"))

fig_inf_time_int = ggplot(inf_time_demo_sum_BLACK_mean.df, aes(x=year, y=mean_infects, colour=sim_type, group=sim_type)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Average of New Infections Over Time: Sim (E) & Sim (E+G)\nBlack Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits = c(0, 3000)) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") 

fig_inf_time_int

ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Sep2025/fig_inf_time_epi_epigenetic_intBLACK_overall_mean.svg", plot = fig_inf_time_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int
dev.off()

# applying mean to all races and intervention
inf_time_demo_sum_mean.df = inf_time_demo_sum_plot_TEMP.df %>%
  group_by(race, year, sim_type, intervention) %>%
  summarise(mean_infects = mean(tot_infects)) %>%
  filter(sim_type %in% c("Observed", "epi_r", "epi_genetic_r"))

fig_inf_time_int_sum_mean_int = ggplot(inf_time_demo_sum_mean.df, aes(x=year, y=mean_infects, colour=sim_type, group=sim_type)) +
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_r", "epi_genetic_r"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle(paste0("Average of New Infections Over Time: Sim (E) & Sim (E+G)\nBlack Intervention")) + 
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits = c(0, 3000)) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_grid(intervention~race, scales = "free")

fig_inf_time_int_sum_mean_int

ggsave(filename = "/Volumes/SSK Media/output_epi_epigen_Nov2025/fig_inf_time_epi_epigenetic_int_overall_mean.svg", plot = fig_inf_time_int_sum_mean_int, device = "svg", width = 10, height = 5, units = "in")
fig_inf_time_int_sum_mean_int
dev.off()