# Import libraries
library(dplyr)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(gtsummary)
# Remove all items from environment
rm(list=ls())

# Set working directory
setwd("")

# Import the data
tor1.agd60 <- cbind(read_csv("AGD Files/Results/Summary Files/m2g.troiano.adult.1axis.person.date.csv"), cp.method = "Troiano VA")

tor1.agd60lfe <- cbind(read_csv("Low Frequency/Results/Summary Files/m2g.lfe.troiano.adult.1axis.person.date.csv"), cp.method = "Troiano VA LFE")

fb1.agd60 <- cbind(read_csv("AGD Files/Results/Summary Files/m2g.fariasbarnett.adult.1axis.person.date.csv"), cp.method = "Barnett VA")

fb1.agd60lfe <- cbind(read_csv("Low Frequency/Results/Summary Files/m2g.lfe.fariasbarnett.adult.1axis.person.date.csv"), cp.method = "Barnett VA LFE")

fb3.agd60 <- cbind(read_csv("AGD Files/Results/Summary Files/m2g.fariasbarnett.adult.3axis.person.date.csv"), cp.method = "Barnett VM")

fb3.agd60lfe <- cbind(read_csv("Low Frequency/Results/Summary Files/m2g.lfe.fariasbarnett.adult.3axis.person.date.csv"), cp.method = "Barnett VM LFE")

fre1.agd60 <- cbind(read_csv("AGD Files/Results/Summary Files/m2g.freedson.adult.1axis.person.date.csv"), cp.method = "Freedson VA")

fre1.agd60lfe <- cbind(read_csv("Low Frequency/Results/Summary Files/m2g.lfe.freedson.adult.1axis.person.date.csv"), cp.method = "Freedson VA LFE")

fre3.agd60 <- cbind(read_csv("AGD Files/Results/Summary Files/m2g.freedson.adult.3axis.person.date.csv"), cp.method = "Freedson VM")

fre3.agd60lfe <- cbind(read_csv("Low Frequency/Results/Summary Files/m2g.lfe.freedson.adult.3axis.person.date.csv"), cp.method = "Freedson VM LFE")

# Keep these variables

cols <- c("month", "record.id", "date", "days", "age",  "cp.method", "valid_days", "wear", "counts", "vector.magnitude", "sedentary", "light", "mvpa")

tor1.agd60 <- tor1.agd60[cols]
tor1.agd60lfe <- tor1.agd60lfe[cols]
fb1.agd60 <- fb1.agd60[cols]
fb1.agd60lfe <- fb1.agd60lfe[cols]
fb3.agd60 <- fb3.agd60[cols]
fb3.agd60lfe <- fb3.agd60lfe[cols]
fre1.agd60 <- fre1.agd60[cols]
fre1.agd60lfe <- fre1.agd60lfe[cols]
fre3.agd60 <- fre3.agd60[cols]
fre3.agd60lfe <- fre3.agd60lfe[cols]

va.data <- rbind(tor1.agd60, tor1.agd60lfe, fb1.agd60, fb1.agd60lfe, fre1.agd60, fre1.agd60lfe)

vm.data <- rbind(fb3.agd60, fb3.agd60lfe, fre3.agd60, fre3.agd60lfe)

data <- rbind(va.data, vm.data)

# Clean R environment of data and lists that are not needed.

rm(list=setdiff(ls(), c("data", "va.data", "vm.data")))

va.cp.method.list <- c("Troiano VA", "Freedson VA", "Barnett VA", "Troiano VA LFE", "Freedson VA LFE", "Barnett VA LFE")

vm.cp.method.list <- c("Freedson VM", "Barnett VM", "Freedson VM LFE", "Barnett VM LFE")

cp.method.list <- c("Troiano VA", "Freedson VA", "Barnett VA", "Freedson VM", "Barnett VM",
                    "Troiano VA LFE", "Freedson VA LFE", "Barnett VA LFE", "Freedson VM LFE", "Barnett VM LFE")

va.data$cp.method <- factor(va.data$cp.method, levels = va.cp.method.list, labels = va.cp.method.list)

vm.data$cp.method <- factor(vm.data$cp.method, levels = vm.cp.method.list, labels = vm.cp.method.list)

data$cp.method <- factor(data$cp.method, levels = cp.method.list, labels = cp.method.list)

va.data <- va.data %>%
  filter(valid_days==1) %>%
  group_by(record.id, cp.method) %>%
  summarise(counts = mean(counts, na.rm=TRUE),
            vector.magnitude = mean(vector.magnitude, na.rm=TRUE),
            wear = mean(wear, na.rm=TRUE),
            sedentary = mean(sedentary, na.rm=TRUE),
            light = mean(light, na.rm=TRUE),
            mvpa = mean(mvpa, na.rm=TRUE)) %>%
  ungroup()

vm.data <- vm.data %>%
  filter(valid_days==1) %>%
  group_by(record.id, cp.method) %>%
  summarise(counts = mean(counts, na.rm=TRUE),
            vector.magnitude = mean(vector.magnitude, na.rm=TRUE),
            wear = mean(wear, na.rm=TRUE),
            sedentary = mean(sedentary, na.rm=TRUE),
            light = mean(light, na.rm=TRUE),
            mvpa = mean(mvpa, na.rm=TRUE)) %>%
  ungroup()

data <- data %>%
  filter(valid_days==1) %>%
  group_by(record.id, cp.method) %>%
  summarise(counts = mean(counts, na.rm=TRUE),
            vector.magnitude = mean(vector.magnitude, na.rm=TRUE),
            wear = mean(wear, na.rm=TRUE),
            sedentary = mean(sedentary, na.rm=TRUE),
            light = mean(light, na.rm=TRUE),
            mvpa = mean(mvpa, na.rm=TRUE)) %>%
  ungroup()

levels(data$cp.method)
cutpoint.labels <- c("Troiano Vertical Axis", "Freedson Vertical Axis", "Barnett Vertical Axis", "Freedson Vector Magnitude", "Barnett Vector Magnitude")
cutpoint.colors <- c('#07468C', '#8B0000', '#990899', '#008080', '#FF8C00')
nf.cpoints <- as.character(levels(data$cp.method))[1:5]
lfe.cpoints <- as.character(levels(data$cp.method))[6:10]

data %>%
  filter(cp.method %in% nf.cpoints) %>%
  ggplot(aes(x = cp.method, y = mvpa, color = cp.method)) +
  geom_jitter(size=0.5) +
  geom_boxplot(outlier.shape = NA, alpha=0.5) + 
  scale_color_manual(labels=cutpoint.labels, values=cutpoint.colors) +
  coord_cartesian(ylim = c(0,60)) +
  scale_y_continuous(breaks = seq(0,60,5)) +
  stat_summary(fun=mean, geom="point", shape=4, size=4, color=cutpoint.colors) +
  labs(title = "Vertical axis and vector magnitude cut-points applied to Actigraph's normal filter", 
       x="", y="MVPA (minutes / day)") +
  theme(axis.ticks.x = element_blank(), 
        legend.position = c(0,1),
        legend.justification = c(-0.1, 1),
        title = element_text(size = 12, family = "Trebuchet MS", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "Trebuchet MS", face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Trebuchet MS", color = "#006400"),
        axis.title.y = element_text(size = 16, family = "Trebuchet MS", face = "bold", margin = margin(r=15)),
        axis.text.y = element_text(size=14, family = "Trebuchet MS", face = "bold", margin = margin(r=5)),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

ggsave(filename="", width = 12, height = 8, units = "in", dpi = 300)

data %>%
  filter(cp.method %in% lfe.cpoints) %>%
  ggplot(aes(x = cp.method, y = mvpa, color = cp.method)) +
  geom_jitter(size=0.5) +
  geom_boxplot(outlier.shape = NA, alpha=0.5) + 
  scale_color_manual(labels=cutpoint.labels, values=cutpoint.colors) +
  coord_cartesian(ylim = c(0,60)) +
  scale_y_continuous(breaks = seq(0,60,5)) +
  stat_summary(fun=mean, geom="point", shape=4, size=4, color=cutpoint.colors) +
  labs(title = "Vertical axis and vector magnitude cut-points applied to Actigraph's low frequency extension filter", 
       x="", y="MVPA (minutes / day)") +
  theme(axis.ticks.x = element_blank(), 
        legend.position = c(0,1),
        legend.justification = c(-0.1, 1),
        title = element_text(size = 12, family = "Trebuchet MS", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "Trebuchet MS", face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Trebuchet MS", color = "#006400"),
        axis.title.y = element_text(size = 16, family = "Trebuchet MS", face = "bold", margin = margin(r=15)),
        axis.text.y = element_text(size=14, family = "Trebuchet MS", face = "bold", margin = margin(r=5)),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))

ggsave(filename="", width = 12, height = 8, units = "in", dpi = 300)



# Mean Absolute Percent Error (MAPE) --------------------------------------

mape <- function(data, id, column.name, cp1, cp2) {
  n <- length(as_vector(unique(data[, id])))
  cp.list1 <- as_vector(data[data[, column.name]==cp1, "mvpa"])
  cp.list2 <- as_vector(data[data[, column.name]==cp2, "mvpa"])
  mape <- round((1/n) * (sum(abs(cp.list1 - cp.list2))), 2)
  mape.description <- paste0("MAPE: ", mape, "%")
  return(mape.description)
}

mape(data, "record.id", "cp.method", "Troiano VA", "Troiano VA LFE")
mape(data, "record.id", "cp.method", "Freedson VA", "Freedson VA LFE")
mape(data, "record.id", "cp.method", "Barnett VA", "Barnett VA LFE")
mape(data, "record.id", "cp.method", "Freedson VM", "Freedson VM LFE")
mape(data, "record.id", "cp.method", "Barnett VM", "Barnett VM LFE")
mape(data, "record.id", "cp.method", "Troiano VA", "Freedson VA")
mape(data, "record.id", "cp.method", "Troiano VA", "Barnett VA")
mape(data, "record.id", "cp.method", "Freedson VA", "Barnett VA")
mape(data, "record.id", "cp.method", "Freedson VM", "Barnett VM")
mape(data, "record.id", "cp.method", "Troiano VA LFE", "Freedson VA LFE")
mape(data, "record.id", "cp.method", "Troiano VA LFE", "Barnett VA LFE")
mape(data, "record.id", "cp.method", "Freedson VA LFE", "Barnett VA LFE")
mape(data, "record.id", "cp.method", "Freedson VM LFE", "Barnett VM LFE")


# Equivalence Testing -----------------------------------------------------

equivalent_description <-  function(data, id, column.name, cp1, cp2) {
  var1 <- as_vector(data[data[, column.name]==cp1, "mvpa"])
  m1 <- mean(var1)
  n1 <- length(var1)
  var2 <- as_vector(data[data[, column.name]==cp2, "mvpa"])
  m2 <- mean(var2)
  n2 <- length(var2)
  c.mean <- ((n1*m1 + n2*m2) / (n1+m1))
  p10 <- c.mean*0.10

  TOSTER::TOSTpaired.raw(n = n1, m1 = m1, m2 = m2,
                         sd1 = sd(var1), sd2 = sd(var2), r12 = cor(var1, var2),
                         alpha = 0.05, low_eqbound = -p10, high_eqbound = p10,
                         plot = TRUE, verbose = TRUE)
}

options(scipen = 999) # Remove scientific notation

equivalent_description(data, "record.id", "cp.method", "Troiano VA", "Troiano VA LFE") 
equivalent_description(data, "record.id", "cp.method", "Freedson VA", "Freedson VA LFE")
equivalent_description(data, "record.id", "cp.method", "Barnett VA", "Barnett VA LFE") 
equivalent_description(data, "record.id", "cp.method", "Freedson VM", "Freedson VM LFE") 
equivalent_description(data, "record.id", "cp.method", "Barnett VM", "Barnett VM LFE")

p.adjust(c(0.169, 0.158, 0.754, 0.764, 0.900), method = "holm")

equivalent_description(data, "record.id", "cp.method", "Troiano VA", "Freedson VA") 
equivalent_description(data, "record.id", "cp.method", "Troiano VA", "Barnett VA") 
equivalent_description(data, "record.id", "cp.method", "Freedson VA", "Barnett VA") 
equivalent_description(data, "record.id", "cp.method", "Freedson VM", "Barnett VM")

p.adjust(c(0.00000061, 1.000, 1.000, 0.999), method = "holm")

equivalent_description(data, "record.id", "cp.method", "Troiano VA LFE", "Freedson VA LFE") 
equivalent_description(data, "record.id", "cp.method", "Troiano VA LFE", "Barnett VA LFE") 
equivalent_description(data, "record.id", "cp.method", "Freedson VA LFE", "Barnett VA LFE")
equivalent_description(data, "record.id", "cp.method", "Freedson VM LFE", "Barnett VM LFE")

p.adjust(c(0.000000324, 1.000, 1.000, 1.000), method="holm")


# Wilcoxon Signed Rank Test -------------------------------------------------------

w.sr.test <- function(data, id, column.name, cp1, cp2) {
  var1 <- as_vector(data[data[, column.name]==cp1, "mvpa"])
  var2 <- as_vector(data[data[, column.name]==cp2, "mvpa"])
  w <- wilcox.test(var1, var2, paired = TRUE, exact = FALSE)
  return(w$p.value)
}

w.sr.test(data, "record.id", "cp.method", "Troiano VA", "Troiano VA LFE") 
w.sr.test(data, "record.id", "cp.method", "Freedson VA", "Freedson VA LFE")
w.sr.test(data, "record.id", "cp.method", "Barnett VA", "Barnett VA LFE")
w.sr.test(data, "record.id", "cp.method", "Freedson VM", "Freedson VM LFE")
w.sr.test(data, "record.id", "cp.method", "Barnett VM", "Barnett VM LFE")

p.adjust(c(0.001859189, 0.001769184, 0.000275834, 0.0001821012, 0.00001613803), method="holm")

w.sr.test(data, "record.id", "cp.method", "Troiano VA", "Freedson VA")
w.sr.test(data, "record.id", "cp.method", "Troiano VA", "Barnett VA")
w.sr.test(data, "record.id", "cp.method", "Freedson VA", "Barnett VA")
w.sr.test(data, "record.id", "cp.method", "Freedson VM", "Barnett VM")

p.adjust(c(0.000002675306, 0.0000001751675, 0.0000001753202, 0.0000001751675), method="holm")

w.sr.test(data, "record.id", "cp.method", "Troiano VA LFE", "Freedson VA LFE")
w.sr.test(data, "record.id", "cp.method", "Troiano VA LFE", "Barnett VA LFE")
w.sr.test(data, "record.id", "cp.method", "Freedson VA LFE", "Barnett VA LFE")
w.sr.test(data, "record.id", "cp.method", "Freedson VM LFE", "Barnett VM LFE")

p.adjust(c(0.000008767045, 0.0000001753202, 0.0000001753202, 0.0000001753202), method="holm")




