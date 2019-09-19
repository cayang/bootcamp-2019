### Day 2 ### 
# Notes - CY

# Load libraries
library(here)
library(readr)
# library(lubridate)
# library(reshape2)

# Load data
generation <- read_csv("data/ca_energy_generation.csv")
imports <- read_csv("data/ca_energy_imports.csv")

# Melt the generation data in order to get wide data into long
library(reshape2)
long_gen <- melt(generation, id.vars="datetime",
                variable.name="source",
                value.name="usage")

# Merge generation and imports
merged_energy <- merge(generation, imports, by="datetime")

long_merged_energy <- melt(merged_energy, id.vars="datetime",
                 variable.name="source",
                 value.name="usage")

## dplyr ##

library(tidyverse)

# select - choose columns
select(gapminder, continent, pop) # by name
select(gapminder, c(1, 3, 10)) # by position
select(gapminder, country:pop) # by range
select(merged_energy, -biogas, -biomass) # exclude names

# filter - choose rows by condition
tmp <- filter(merged_energy, imports > 7000)
nrow(tmp)

tmp <- filter(merged_energy, imports > 7000, natural_gas < 7000) # multiple conditions
nrow(tmp) 

# mutate
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)

# summarize
# total energy consumption
summarize(long_merged_energy, total=sum(usage, na.rm=TRUE))

# piping
long_merged_energy %>%
    filter(source == "geothermal") %>%
    select(-datetime) %>%
    mutate(log_usage = log(usage)) %>%
    summarize(mean_log_usage = mean(log_usage, na.rm=TRUE))

merged_energy %>%
    select(-datetime) %>%
    mutate(total_usage=rowSums(., na.rm=TRUE)) %>%
    summarize(total_usage=sum(total_usage, na.rm=TRUE))

# class exercise on piping to get the mean hydro usage
merged_energy %>% 
    select(contains("hydro")) %>% 
    mutate(total_hydro=rowSums(., na.rm=TRUE)) %>% 
    summarize(mean_hydro=mean(total_hydro, na.rm=TRUE))

# group_by
long_merged_energy %>% 
    group_by(source) %>% # comma if you want multiple groups
    summarize(sum_usage = sum(usage, na.rm=TRUE)) # comma if you want multiple summary stats

# class exercise to get the mean usage for large_hydro, small_hydro, biogas, biomass
long_merged_energy %>% 
    filter(source %in% c("large_hydro", "small_hydro", "biogas", "biomass")) %>% 
    group_by(source) %>% 
    summarize(mean_usage=mean(usage, na.rm=TRUE))

merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars="datetime",
         variable.name="source",
         value.name="usage") %>% 
    group_by(source) %>%
    summarize(mean_usage=mean(usage, na.rm=TRUE))

## data.table ##

# Load data
library(data.table)

generation_df <- read_csv("data/ca_energy_generation.csv")
generation_dt <- fread("data/ca_energy_generation.csv")

# row filtering
generation_dt[wind > 4400]
generation_dt[wind > 4400 & mday(datetime) == 7]

# class exercise for row filtering
generation_dt[natural_gas <= 5000 & large_hydro > 2000]
generation_dt[coal > 10 & solar > median(solar)]

# column manipulation
generation_dt[, wind+solar]

# create new columns
generation_dt[, newcol := 3*wind+solar*biogas/2] # := in place
generation_dt[, .(newcol=3*wind+solar*biogas/2)] # .() export data

# delete column
generation_dt[, newcol := NULL]

# class exercise on column manipulation
generation_dt[, total_hydro:=small_hydro+large_hydro]
generation_dt[, .(mean(nuclear), mean(biogas))]
new_tbl <- generation_dt[solar == 0, .(datetime, total_thermal=natural_gas+coal)]

# by function - identical to groupby
generation_dt[hour(datetime) > 19, .(mean(nuclear), mean(wind)), by=mday(datetime)]

# class exercise
generation_dt[, .(median(solar)), by=hour(datetime)]
generation_dt[solar > 0, .(max(solar)), by=.(mday(datetime), hour(datetime))]

# class exercise to convert dplyr to data.table
setDT(long_merged_energy)
long_merged_energy[, .(day=as_date(datetime), log_output=log(output)), by=day]

# bonus features
library(benchmark)

