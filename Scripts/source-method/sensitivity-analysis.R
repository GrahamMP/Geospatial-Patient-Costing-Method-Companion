
### Goals: set and define parameters for cost formulas; iterate over different values for sensitivity analysis
# step 1: define defaults parameters and setup variations for SA
# step 2: create function to source script and pass different parameter values
# step 3: return unit cost dataframe for each parameter variation and stack results

# Setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(plyr, tidyverse)

# Set parameters ----------------------------------------------------------

# define base/default parameters >>> more parameters can be extracted from source script!!!
# but need to ensure that the parameters are correctly set and passed to source script

### Travel data ---------------------------------------------------------------

# load
traveldata <- read_csv('Data/Geospatial Data/HA_distance_duration.csv')

# update var names -- remove whitespace
traveldata <- traveldata %>% rename_with(~gsub(' ', '_', .x))

# update travel values
traveldata <- traveldata %>% 
  ## double distance for round trip
  mutate(Street_Distance = Street_Distance * 2,
         ## double time and convert to hours
         Street_Duration = (Street_Duration * 2) / 60)

### Define variables and levels -------------------------------------------

# do this here rather than in source script, since values are needed for variations

HA_Name <- traveldata$HA_Name
service_type <- c('MD', 'ED_CTAS_1', 'ED_CTAS_4', 'Hosp', 'Virtual')
age_group <- c('0-14', '15-64', '65+')

### Single value parameters -------------------------------------------------

# Wage
wage <- 30.54

# Meal and accommodation
meal_cost <- 15 
accomm <- 100

# Car cost
car_cost <- 0.48

# Internet data cost
data_usage <- 1.25

# Source ------------------------------------------------------------------

# test sourcing the script and output the default unit costs --- save PFC for comparisons
source('Scripts/source-method/pfc-to-source.R', echo = T)

# quick view
pfc
summary(pfc)

# SENSITIVITY ANALYSIS ----------------------------------------------------

### setup different methods
# simpler for wage where one value can be varied in a vector
# different method required for parameters stored in a table --- i.e., travel dist/dur

### Wage --------------------------------------------------------------------

##### Function ----------------------------------------------------------------

# define new function to source script with different wage values
pfc_wrapper_wage <- function(new_wage) { 
  wage <- new_wage
  source('Scripts/source-method/pfc-to-source.R', local = T)$value 
}

# check outputs -- should return identical unit cost dataframe as PFC
pfc2 <- pfc_wrapper_wage(new_wage = 30.54)
identical(pfc, pfc2)

##### Vary wage -------------------------------------------------------------

# create vector of different wages and calculate PFC for each
wages <- c(16.75, 20, 25, 30.54, 35, 40, 45, 50)

##### Run -------------------------------------------------------------------

# expected output rows
length(wages) * nrow(pfc)

# run
SA_wage <- ldply(wages, function(x) { data.frame(wage = x, pfc_wrapper_wage(new_wage = x)) }) %>%
  as_tibble()

# checks
summary(SA_wage)
SA_wage %>% count(wage)
SA_wage %>% filter(wage == 30.54) %>% 
  select(any_of(names(pfc))) %>% summary() %>% 
  identical(summary(pfc))

# plot unit costs
SA_wage %>% filter(age_group == '15-64') %>% 
  ggplot(aes(x = wage, y = unit_cost, fill = service_type)) + 
  geom_col(colour = 'black') +
  facet_wrap(~ HA_Name)

### Travel distance/duration ------------------------------------------------

# need to vary values in table and pass entire table to script
traveldata

##### Function --------------------------------------------------------------

# define new function that takes a table
pfc_wrapper_travel <- function(new_travel) {
  traveldata <- new_travel
  source('Scripts/source-method/pfc-to-source.R', local = T)$value
}

# check
pfc2 <- pfc_wrapper_travel(new_travel = traveldata)
identical(pfc, pfc2)

##### Vary travel ----------------------------------------------------------

# add min/q1/mean/q3/max values for each HA and distance/duration
# travel_range <- tibble(HA_Name = HA_Name,
#                    ## NOTE, the values below are for each HA !!!
#                   duration_min = c(2.3, 2.4, 1.2, 1.49, 2.4),
#                    duration_q1 = c(7.3, 14.5, 12.8, 5, 8.7),
#                    duration_mean = c(36.3, 52.8, 72.2, 20.9, 33.8),
#                    duration_q3 = c(12.6, 94, 95.2, 12.1, 38.7),
#                    duration_max = c(387.6, 186.5, 429.6, 197.1, 178.4),
#                    distance_min = c(1.3, 1.6, 0.6, 0.7, 1.1),
#                    distance_q1 = c(4.6, 9.8, 6.6, 2.8, 5.5),
#                    distance_mean = c(16.9, 33.7, 51.7, 9.9, 19.3),
#                    distance_q3 = c(9.9, 47, 69.6, 7.2, 27.2),
#                    distance_max = c(174.4, 116, 302.4, 83, 70.3)) %>%
  ## gather into long format to make calculations simpler
#  gather(type, range_value, -HA_Name)

# check/view
travel_range

### !!! UPDATE using +/- % changes from mean
traveldata
seq(0.5, 1.5, 0.1)

travel_range <- tibble(percent_diff = seq(0.5, 1.5, 0.1)) %>% 
  ddply(., .(percent_diff), function(x) 
    traveldata %>% mutate(Street_Duration = Street_Duration * x$percent_diff,
                          Street_Distance = Street_Distance * x$percent_diff))

##### Update input data -------------------------------------------------------

# update values
travel_range <- travel_range %>% 
  # mutate(stat = str_split_fixed(type, '_', 2)[ , 2],
  #        type = gsub('duration_.*', 'Street_Duration', type),
  #        type = gsub('distance_.*', 'Street_Distance', type)) %>% 
  ## change duration to hours and multiple distance/duration by 2
  # mutate(Street_Distance = Street_Distance * 2,
  #       Street_Duration = (Street_Duration / 60) * 2)
  # mutate(range_value = ifelse(type == 'Street_Duration', range_value / 60, range_value),
  #        range_value = range_value * 2) %>% 
  # spread(type, range_value)

# check/view
travel_range
travel_range %>% count(HA_Name)
travel_range %>% count(percent_diff)
summary(travel_range)

# # define number of SDs to multiply by and join to table
# travel_SD <- expand.grid(HA_Name = HA_Name, SD_factor = seq(-2, 2, by = 0.5)) %>%
#   left_join(travel_SD)
# 
# # reformat original travel data and add to SD table
# travel_SD <- traveldata %>% rename_with(~ gsub('Street_D', 'd', .x)) %>%
#   gather(type, value, -HA_Name) %>%
#   left_join(travel_SD, .)
# 
# # check
# travel_SD
# 
# ### >>> calculate new travel values multiplied by SD
# travel_SD <- travel_SD %>% mutate(value_new = value + (SD_factor * SD_value))
# 
# # checks
# travel_SD %>% filter(value == value_new)
# 
# # update format to match original travel table
# travel_SD <- travel_SD %>% select(HA_Name, SD_factor, type, value_new) %>%
#   spread(type, value_new) %>%
#   rename(Street_Distance = distance, Street_Duration = duration)

##### Run ---------------------------------------------------------------------

# expected output length
length(unique(travel_range$percent_diff)) * nrow(pfc)

# run
SA_travel <- ddply(travel_range, .(percent_diff), function(x) {
  data.frame(pfc_wrapper_travel(new_travel = x))
})

# checks
summary(SA_travel)
SA_travel %>% count(percent_diff)

# >>> no longer identical --- why???
SA_travel %>% filter(percent_diff == 1) %>%
  select(any_of(names(pfc))) %>% summary() %>%
  identical(summary(pfc))

# plot
SA_travel %>% filter(age_group == '15-64') %>% 
  ggplot(aes(x = percent_diff, y = unit_cost, fill = service_type)) + 
  geom_col(colour = 'black') +
  facet_wrap(~ HA_Name)

### Caregiver attendance---------------------------------------

### !!! RUN caregiver last, as the current source approach requires the original coefficients
# or, remove the caregiver vector from environment to skip if-statement in source script

# define new function to source script with different caregiver coefficient values
pfc_wrapper_caregiver <- function(new_caregiver) { 
  caregiver_coeff <- new_caregiver
  source('Scripts/source-method/pfc-to-source.R', local = T)$value 
}

# check outputs -- should return identical unit cost dataframe as PFC 
# >>> THIS IS ONLY TRUE as the if-logic in the source script ignores the new value
pfc2 <- pfc_wrapper_caregiver(new_caregiver = 0.50)
identical(pfc, pfc2)

### create vector of coefficients --- this will trigger if-statement in source script!!!
caregivers <- c(0, 0.25, 0.50, 0.75, 1)

##### Run -------------------------------------------------------------------

# expected output rows
length(caregivers) * nrow(pfc)

# run
SA_caregiver <- ldply(caregivers, function(x) { 
  data.frame(caregiver_coeff = x, pfc_wrapper_caregiver(new_caregiver = x)) 
}) %>%
  as_tibble()

# checks
summary(SA_caregiver)
SA_caregiver %>% count(caregiver_coeff)
# >>> this will no longer be identical across entire original PFC dataset
SA_caregiver %>% filter(caregiver_coeff == 0.5) %>% 
  select(any_of(names(pfc))) %>% summary() %>% 
  identical(summary(pfc))

# plot unit costs
SA_caregiver %>% filter(age_group == '15-64') %>% 
  ggplot(aes(x = caregiver_coeff, y = unit_cost, fill = service_type)) + 
  geom_col(colour = 'black') +
  facet_wrap(~ HA_Name)

# Manuscript output summary
SA_wage_out = select(SA_wage, wage:age_group, unit_cost)

SA_wage_out_1 <- SA_wage_out[SA_wage_out$age_group == '15-64' & SA_wage_out$service_type == 'MD',]
write_csv(SA_wage_out_1, 'SA_wage_out_1.csv')

SA_wage_out_3 <- SA_wage_out[SA_wage_out$age_group == '15-64' & SA_wage_out$service_type == 'ED_CTAS_1',]
write_csv(SA_wage_out_3, 'SA_wage_out_3.csv')

SA_wage_out_4 <- SA_wage_out[SA_wage_out$age_group == '15-64' & SA_wage_out$service_type == 'ED_CTAS_4',]
SA_wage_out_5 <- SA_wage_out[SA_wage_out$age_group == '15-64' & SA_wage_out$service_type == 'Hosp',]
SA_wage_out_6 <- SA_wage_out[SA_wage_out$age_group == '15-64' & SA_wage_out$service_type == 'Virtual',]
