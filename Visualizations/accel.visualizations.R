accel.data$weekday <- factor(accel.data$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                             labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

accel.data$intensity <- ifelse(accel.data$sedentary==1, "Sedentary", 
                               ifelse(accel.data$light==1, "Light", 
                                      ifelse(accel.data$mvpa==1, "MVPA", "Non-wear")))

accel.data$intensity <- factor(accel.data$intensity, levels = c("Non-wear", "Sedentary", "Light", "MVPA"),
                               labels = c("Non-wear", "Sedentary", "Light", "MVPA"))

accel.data %>%
  filter(wear==1) %>%
  mutate(time = as.numeric(format(time.stamp, "%H"))*60+as.numeric(format(time.stamp, "%M"))) %>%
  group_by(time) %>%
  ggplot(aes(x = time/60, y = counts, color=intensity)) +
  geom_point(size=1) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(breaks = c(1013, 1952)) +
  coord_cartesian(ylim = c(0,3000)) +
  labs(y="", x="") +
  geom_hline(yintercept = 1013, color = "black") +
  geom_hline(yintercept = 1952, color = "black") +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a")) +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text = element_text(family = "Trebuchet MS", size = 14),
        panel.background = element_rect(fill = "white"), 
        axis.line = element_line(color = "black"))

ggsave("", width = 11.5, height = 7.5, units = "in", dpi = 300)


  
  
  
  
  
  
  
  
  
  
  



  
  
  

