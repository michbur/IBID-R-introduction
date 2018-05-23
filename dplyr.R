# https://github.com/michbur/IBID-R-introdution/blob/master/dplyr.R

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
  mutate(mean_LB = mean(c(LB_1, LB_2, LB_3)))

# Subsetting I: indices

select(dat, M63_1, M63_2, M63_3)
select(dat, M63_1) %>% head
select_(dat, "M63_1") %>% head

paste0("M63_", 1L:3)

select_(dat, .dots = paste0("M63_", 1L:3)) %>% head

# select selects columns

slice(dat, 25L:27)

# slice: extract rows

# Task: Extract rows 50-57 but only for columns active, pathotype 
# and LB_1

slice(dat, 50L:57) %>% 
  select(active, pathotype, LB_1)
  

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

group_by(median_dat, active, medium) %>% 
  filter(value == max(value))

group_by(median_dat, active, medium) %>% 
  mutate(value = value/max(value))

# Merging

dat_np <- select(dat, -pathotype)
pathotype <- read.csv("https://raw.githubusercontent.com/michbur/IBID-R-introdution/master/data/data2.csv") %>% 
  mutate(strain = as.character(strain))

final_dat <- inner_join(median_dat, pathotype, by = c("strain" = "strain")) %>% 
  ungroup()

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

group_by(final_dat, medium) %>% 
  filter(value == min(value))
