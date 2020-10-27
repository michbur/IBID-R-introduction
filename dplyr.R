# https://github.com/michbur/IBID-R-introduction/blob/master/dplyr.R

library(dplyr)
library(reshape2)

# Data and data types

# ctrl + enter to run the line in R
dat <- read.csv("https://raw.githubusercontent.com/michbur/IBID-R-introdution/master/data/data1.csv")

# readr package: read xls files

class(dat)

head(dat)

colnames(dat)

dat[["pathotype"]]
class(dat[["pathotype"]])

dat[["LB_1"]]
class(dat[["LB_1"]])

dat[["strain"]]
class(dat[["strain"]])

# pipes

dat[["strain"]] <- as.character(dat[["strain"]])
class(dat[["strain"]])

mutate(dat, strain = as.character(strain))

dat <- read.csv("https://raw.githubusercontent.com/michbur/IBID-R-introdution/master/data/data1.csv") %>% 
  mutate(strain = as.character(strain))

dat[["strain"]]

# pipe shortcut: ctrl+shift+m 

dat %>% 
  group_by(strain) %>% 
  mutate(mean_LB = mean(c(LB_1, LB_2, LB_3))) %>% 
  select(mean_LB)

# Subsetting I: indices

select(dat, M63_1, M63_2, M63_3)
select(dat, M63_1) %>% head
select(dat, "M63_1") %>% head

paste0("M63_", 1L:3)

# select_(dat, .dots = paste0("M63_", 1L:3)) %>% head
select(dat, paste0("M63_", 1L:3)) %>% head


# select selects columns

slice(dat, 25L:27)

# slice: extract rows

# Task: Extract rows 50-57 but only for columns active, pathotype 
# and LB_1

slice(dat, 50L:57) %>% 
  select(active, pathotype, LB_1)
  
# Task: Extract first five rows but only for columns pathotype, 
# LB_1, LB_2 and LB_3

slice(dat, 1:5) %>%
  select(pathotype, paste0("LB_", 1:3))

# Melting data: long and wide formats

melt(dat, variable.name = "medium") 

melt(dat, variable.name = "medium")[["medium"]]

# separate between medium name and replicate ID

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character %>% 
  strsplit(split = "_")

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character %>% 
  strsplit("_") %>% 
  sapply(first)

melt(dat, variable.name = "medium") %>% 
  mutate(medium2 = sapply(strsplit(as.character(medium), "_"), first)) 

x <- c(3, 5, 7, -1, -6)
ifelse(x < 0, 0, x)

# Task: Change everything in vector x larger than 4 to 4
ifelse(x > 4, 4, x)

melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value))

melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value)) %>% 
  group_by(medium) %>% 
  mutate(value = (max(value) - min(value))/max(value))

median_dat <- melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value)) %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = median(value)) %>% 
  ungroup

# Subsetting II: conditions

filter(median_dat, strain == "5160")

filter(median_dat, strain == "5160", active %in% c("W1", "W2"))

# Task 1: filter data for strain 5160 and medium LB

filter(median_dat, strain == "5160", medium == "LB")
filter(median_dat, strain == "5160", medium %in% "LB")
filter(median_dat, strain %in% "5160", medium %in% "LB")

# Task 2: filter data for strains 5160 and 5207, and medium LB
filter(median_dat, strain %in% c("5160", "5207"), medium == "LB")

# Task 3: filter data for strains: 5160, 5207 and 5213, and medium LB
filter(median_dat, strain %in% c("5160", "5207", "5213"), 
       medium == "LB")

# Task 3: filter data for strains: 5160, 5207 and 5213, medium LB and
# active W1

filter(median_dat, strain %in% c("5160", "5207", "5213"), 
       medium == "LB",
       active == "W1")

filter(median_dat, strain != "5160")

filter(median_dat, strain == "5160", !(active %in% c("W1", "W2")))

# Task: Select data for strain 5207 and NOT medium LB

filter(median_dat, strain == "5207", medium != "LB")

# Task: Select data for strain 5207 and NOT mediums LB and M63

filter(median_dat, strain == "5207", !(medium %in% c("LB", "M63")))

filter(median_dat, strain == "5160", active == "W3")

filter(median_dat, strain == "5160" & active == "W3")

filter(median_dat, strain == "5160" | active == "W3")

# Final task: filter data only for strain 5160 and active W2; 
# then select only strain, medium and value

ungroup(median_dat) %>% 
  filter(strain == "5160", active == "W2") %>% 
  select(strain, medium, value)

# Grouping

group_by(median_dat, active, medium) %>% 
  summarise(max_value = max(value))

# Task: Instead of maximum, compute minimum (min function) 
# for each level of active and medium

group_by(median_dat, active, medium) %>% 
  summarise(min_value = min(value))

group_by(median_dat, active, medium) %>% 
  summarise(max_value = max(value),
            min_value = min(value))

# Task: Instead of maximum, compute median (median function) 
# for each level of active and medium

group_by(median_dat, active, medium) %>% 
  summarise(median_value = median(value))

# Task: Instead of maximum, compute median (median function) 
# for each level of active

group_by(median_dat, active) %>% 
  summarise(median_value = median(value))

group_by(median_dat, active) %>% 
  filter(value == max(value))

# Task: Find all strains (grouped by active) that have minimum value

group_by(median_dat, active) %>% 
  filter(value == min(value))

# Task: Find all strains (grouped by medium) that have maximum value

group_by(median_dat, medium) %>% 
  filter(value == max(value))

group_by(median_dat, active) %>% 
  mutate(value = value/max(value))

x <- data.frame(value = 1L:10,
                variable = c(rep("A", 5), rep("B", 5)))

mutate(x, norm = value/max(value))

group_by(x, variable) %>% 
  mutate(norm = value/max(value))

group_by(x, variable) %>% 
  mutate(norm = (value - min(value))/(max(value) - min(value)))

mutate(x, norm = (value - min(value))/(max(value) - min(value))) %>% 
  group_by(variable) %>% 
  mutate(norm_grouped = (value - min(value))/(max(value) - min(value)))

# Task: Find all strains (grouped by active) that have value higher 
# than median value (median)

group_by(median_dat, active) %>% 
  filter(value > median(value))

# Task: Find all strains (grouped by active) that have value higher 
# than 0.3

group_by(median_dat, active) %>% 
  filter(value > 0.3)

group_by(median_dat, active) %>% 
  filter(value > 0.3) %>% 
  summarise(count = length(value))

# Task: Find all strains (grouped by medium) that have value lower 
# than 0.05, select columns medium and strain, 
# and count number of strains for each medium

group_by(median_dat, medium) %>% 
  filter(value < 0.05) %>% 
  select(medium, strain) %>% 
  summarise(count = length(strain)) %>% 
  arrange(desc(count))


# Merging

dat_np <- select(dat, -pathotype)
pathotype <- read.csv("https://raw.githubusercontent.com/michbur/IBID-R-introdution/master/data/data2.csv") %>% 
  mutate(strain = as.character(strain))

final_dat <- inner_join(dat_np, pathotype, 
                        by = c("strain" = "strain")) 

# Final task: only for mediums LB and BHI count strains with value 
# larger than 0.1 for each active

filter(median_dat, medium %in% c("LB", "BHI"),
       value > 0.1) %>% 
  group_by(active) %>% 
  summarise(count = length(strain))


# 1. Create a data.frame mean_dat of mean values only for replicates 1-2 for each 
# strain and active substance.
# 2. In mean_dat find the minimum value for each medium and pathotype. 
# 3. Compute the median value for all pathotypes regardless of the active
# 4. Count the number of strains for each pathotype.
# 5. Find strains with the minimum value for each medium.

melt(dat, variable.name = "medium") %>% 
  mutate(medium2 = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value),
         rep_id = sapply(strsplit(as.character(medium), "_"), function(x) x[length(x)])) %>% 
  filter(rep_id != "3") %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = mean(value))

group_by(final_dat, pathotype) %>% 
  summarise(n_strains = length(unique(strain)))


