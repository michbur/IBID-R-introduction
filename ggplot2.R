# https://tinyurl.com/IBID-R-intro2
# https://github.com/michbur/IBID-R-introdution/blob/master/ggplot2.R

library(dplyr)
library(reshape2)

dat <- read.csv("https://raw.githubusercontent.com/michbur/IBID-R-introdution/master/data/data1.csv")

final_dat <- mutate(dat, strain = as.character(strain)) %>% 
  melt(variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value)) %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = median(value)) 

library(ggplot2)

# Dot plots

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point()

set.seed(1410)
ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter")

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_wrap(~ active)

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_grid(medium ~ active)

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_grid(medium ~ active, labeller = label_both)

set.seed(1410)
ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_wrap(~ active + medium)

# Task: Create a plot of relationship between medium and value and 
# split it using active and pathotype

ggplot(final_dat, aes(x = medium, y = value)) +
  geom_point(position = "jitter") +
  facet_grid(pathotype ~ active, labeller = label_both)

# Boxplots

set.seed(1410)
ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_boxplot() +
  facet_grid(medium ~ active)

# Task: Create a boxplot only for active == "W1" and pathotype == "UPEC"

filter(final_dat, active == "W1", pathotype == "UPEC") %>% 
  ggplot(aes(x = pathotype, y = value)) +
  geom_boxplot() +
  facet_grid(medium ~ active)

# Task: Create a boxplot only for active W1 and W3, on facets 
# active and pathotype, map medium to the X-axis

filter(final_dat, active %in% c("W1", "W3")) %>% 
  ggplot(aes(x = medium, y = value)) +
  geom_boxplot() +
  facet_grid(pathotype ~ active)

# Task: Create a dotplot only for active W1 and W3, on facets 
# active and pathotype, map medium to the X-axis

filter(final_dat, active %in% c("W1", "W3")) %>% 
  ggplot(aes(x = medium, y = value)) +
  geom_point(position = "jitter") +
  facet_grid(pathotype ~ active)

# Beeswarm charts - no more jitter
# install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(final_dat, aes(x = medium, y = value)) +
  geom_quasirandom() +
  facet_grid(pathotype ~ active, labeller = label_both)

ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_point(position = "jitter") +
  facet_wrap(~ medium)

ggplot(filter(final_dat, active != "W3"), aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)

# Task: Create a dotplot and beeswarm plot for data where we consider
# only measurements with value higher than 0.1. X axis - medium,
# facets: active

filter(final_dat, value > 0.1) %>% 
  ggplot(aes(x = medium, y = value)) +
  geom_point() +
  facet_wrap(~ active) +
  ggtitle("Dot plot")

filter(final_dat, value > 0.1) %>% 
  ggplot(aes(x = medium, y = value)) +
  geom_quasirandom(groupOnX = TRUE) +
  facet_wrap(~ active) +
  ggtitle("Beeswarm plot")

# Customizing facets

ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_point(position = "jitter") +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_point(position = "jitter") +
  facet_wrap(~ medium, scales = "free_y")

filter(final_dat, pathotype != "EAEC" | medium != "LB") %>% 
  ggplot(aes(x = pathotype, y = value, color = active)) +
  geom_point(position = "jitter") +
  facet_wrap(~ medium, scales = "free_x")

filter(final_dat, pathotype != "EAEC" | medium != "LB") %>% 
  ggplot(aes(x = pathotype, y = value, color = active)) +
  geom_point(position = "jitter") +
  facet_wrap(~ medium, scales = "free")

# Density plots

ggplot(final_dat, aes(x = value)) +
  geom_density()

ggplot(final_dat, aes(x = value)) +
  geom_density() +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value)) +
  geom_density() +
  facet_wrap(~ active)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density() +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_grid(pathotype ~ medium)

mutate(final_dat, active = factor(active, 
                                  levels = c("W2", "W3", "W1"),
                                  labels = c("A1", "Helena Fisher", "W1"))) %>% 
  ggplot(aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ medium)


# 1. Create a density plot for each pathotype and medium.

ggplot(final_dat, aes(x = value, fill = medium)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ pathotype)

ggplot(final_dat, aes(x = value)) +
  geom_density(alpha = 0.2) +
  facet_grid(medium ~ pathotype)

# Bar plots

thr_dat <- mutate(final_dat, thr = value > 0.07)

ggplot(thr_dat, aes(x = thr)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar(position = "stack") 

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar(position = "fill")

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar(position = "stack") +
  coord_polar()

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") 

# Task: Using facets and bar charts show threshold data separately
# for each active substance and medium.

ggplot(thr_dat, aes(x = active, fill = thr)) +
  geom_bar(position = "fill") +
  facet_wrap(~ medium)

# 2. Show on a barchart number of strains from each pathotype.

thr_dat2 <- group_by(thr_dat, medium) %>% 
  summarise(thr = mean(thr))

rbind(mutate(thr_dat2, thr_et = TRUE),
      mutate(thr_dat2, thr_et = FALSE,
             thr = 1 - thr)) %>% 
  ggplot(aes(x = medium, y = thr, fill = thr_et, label = formatC(thr, 2))) +
  geom_bar(stat = "identity") +
  geom_text(vjust = 2)


thr_dat <- mutate(final_dat, thr = value > 0.05)
thr_dat2 <- group_by(thr_dat, medium) %>% 
  summarise(thr = mean(thr))
rbind(mutate(thr_dat2, thr_et = TRUE),
      mutate(thr_dat2, thr_et = FALSE,
             thr = 1 - thr)) %>% 
  ggplot(aes(x = medium, y = thr, fill = thr_et, label = formatC(thr, 2))) +
  geom_bar(stat = "identity") +
  geom_text(vjust = 2)

mean_dat <- group_by(final_dat, active, medium, pathotype) %>% 
  summarise(mean_value = mean(value),
            sd_value = sd(value))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_bar(position = "dodge", stat = "identity") + 
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1)

# Task: Using a bar chart compare median values for each medium, 
# pathotype and active. 
# Use median absolute deviation (mad()) as a dispersion measure.
# mean() - median(); sd() - mad()

median_dat <- group_by(final_dat, active, medium, pathotype) %>% 
  summarise(median_value = median(value),
            mad_value = mad(value)) %>% 
  mutate(upper = median_value + mad_value)

ggplot(median_dat, aes(x = pathotype, y = median_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = upper, 
                    ymin = median_value, color = medium), 
                position = "dodge") +
  facet_wrap(~ active, ncol = 1)

# 2. Using a heat map compare median values for each medium and pathotype. 

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1) + 
  coord_flip()

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, nrow = 1) + 
  coord_flip()

p <- ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1)

# Styling

p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

my_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  legend.position = "bottom")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  my_theme

# 1. Create your own theme. See ?theme
# 2. Try existing themes (for example theme_bw)

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

unique(thr_dat[["medium"]])

thr_dat2 <- mutate(thr_dat,
                   medium = factor(medium, levels = c("LB", "BHI", "M63", "TSB")))

ggplot(thr_dat2, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

# Reverse the sequence of active (W3, W2, W1) and create a bar chart, 
# with the fraction of strains above threshold for each possible value
# of active.

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_discrete("Threshold")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Threshold", values = c("orange", "lightblue3"))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black")

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black") +
  scale_fill_continuous(low = "blue", high = "red")

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) 

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) +
  scale_size_continuous(range = c(5, 10))

# Task: Create a heatmap with gradient scale, 
# midpoint should be a median of mean_value

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = (max(mean_dat[["mean_value"]]) - min(mean_dat[["mean_value"]]))/2)


ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium) +
  coord_cartesian(xlim = c(0, 0.1))

# interactive plots

library(plotly)
ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

ggplotly(ggplot(thr_dat, aes(x = medium, fill = thr)) +
           geom_bar(position = "fill"))

ggplotly(ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
           geom_point(position = "jitter") +
           facet_wrap(~ medium))


ggplotly(ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
           geom_quasirandom() +
           facet_wrap(~ medium))
