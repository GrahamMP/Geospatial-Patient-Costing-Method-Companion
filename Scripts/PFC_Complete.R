
# Setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(plyr, tidyverse)

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

# Create output matrix ----------------------------------------------------

# define variables and levels
HA_Name <- traveldata$HA_Name
service_type <- c('MD', 'ED_CTAS_1', 'ED_CTAS_4', 'Hosp', 'Virtual')
age_group <- c('0-14', '15-64', '65+')

# expand into table
pfc <- tibble(expand.grid(HA_Name = HA_Name, service_type = service_type, 
                          age_group = age_group, stringsAsFactors = F))
pfc

### Add travel data ---------------------------------------------------------

pfc <- left_join(pfc, traveldata)

# Set parameters ----------------------------------------------------------

### Parking -----------------------------------------------------------------

# by service type -- order by HA alphabetically
HA_Name

parking <- bind_rows(
  tibble(HA_Name = HA_Name,
         service_type = 'MD',
         parking_cost = c(3, 3, 2, 8, 2)),
  tibble(HA_Name = rep(HA_Name, 2),
         service_type = rep(c('ED_CTAS_1', 'ED_CTAS_4'), each = 5),
         parking_cost = rep(c(7.5, 4.5, 1.5, 18, 4.5), 2)),
  tibble(HA_Name = HA_Name,
         service_type = 'Hosp',
         parking_cost = c(16.5, 12, 18, 37.5, 26.75))
)

## join to main table
pfc <- left_join(pfc, parking)
pfc %>% distinct(HA_Name, service_type, parking_cost)

### Appointment times -------------------------------------------------------

# MD times
fm_wait <- 0.5
fm_appt <- 0.26
MD_time <- fm_wait + fm_appt

# ED LOS
ED_CTAS_1_LOS <- 3.9
ED_CTAS_4_LOS <- 2.7

# Hosp LOS
Hosp_LOS <- 53.6

# Virtual times
virtual_wait <- 0.27
virtual_appt <- 0.35
virtual_time <- virtual_wait + virtual_appt

### create table of times in HOURS
service_type
appt_time <- tibble(service_type = service_type,
                    appt_time = c(MD_time, ED_CTAS_1_LOS, ED_CTAS_4_LOS, Hosp_LOS, virtual_time))
appt_time

### join to main table
pfc <- left_join(pfc, appt_time)
pfc %>% distinct(service_type, appt_time)

### Caregiver coefficients ---------------------------------------------------

# add directly to main table by age/service
pfc <- pfc %>% 
  mutate(caregiver_coeff = case_when(age_group == '0-14' & service_type != 'Hosp' ~ 1,
                                     age_group == '0-14' & service_type == 'Hosp' ~ 0.75,
                                     age_group != '0-14' & service_type == 'Hosp' ~ 0.25,
                                     age_group != '0-14' & service_type != 'Hosp' ~ 0.5))
pfc %>% distinct(age_group, service_type, caregiver_coeff)

# Single value parameters -------------------------------------------------

# Wage
wage <- 30.54

# Meal and accommodation
meal_cost <- 15 
accomm <- 100

# Car cost
car_cost <- 0.48

# Internet data cost
data_usage <- 1.25

# Calculate subunit costs -------------------------------------------------

### Lost productivity -------------------------------------------------------

# appt time + travel time * wage --- only for ages 15-64 --- varies by virtual/not
pfc <- pfc %>% 
  mutate(subunit_LP = case_when(age_group == '15-64' & service_type != 'Virtual' ~ wage * (appt_time + Street_Duration),
                                age_group == '15-64' & service_type == 'Virtual' ~ wage * appt_time,
                                age_group != '15-64' ~ 0))

### Informal caregiving -----------------------------------------------------

# appt time + travel time * wage --- for all age groups but accounts for different caregiver coefficients and virtual/not
pfc <- pfc %>% 
  mutate(subunit_IC = case_when(service_type != 'Virtual' ~ caregiver_coeff * wage * (appt_time + Street_Duration),
                                service_type == 'Virtual' ~ caregiver_coeff * wage * appt_time))

### Out of pocket -----------------------------------------------------------

# OOP varies by service type --- plus by HA for Hosp
pfc <- pfc %>% mutate(subunit_OOP = case_when(
  service_type == 'MD' ~ parking_cost + (Street_Distance * car_cost),
  service_type %in% c('ED_CTAS_1', 'ED_CTAS_4') ~ parking_cost + meal_cost + (Street_Distance * car_cost),
  service_type == 'Hosp' & HA_Name == 'Northern' ~ (Street_Distance * car_cost) + (accomm * 2 * caregiver_coeff) + (meal_cost * 3 * 2 * caregiver_coeff) + parking_cost,
  service_type == 'Hosp' & HA_Name != 'Northern' ~ (Street_Distance * car_cost) + (meal_cost * 3 * 2 * caregiver_coeff) + parking_cost,
  service_type == 'Virtual' ~ data_usage
))

pfc
summary(pfc)

# Total unit cost ---------------------------------------------------------

# sum 3 subunits
pfc <- pfc %>% mutate(unit_cost = subunit_IC + subunit_LP + subunit_OOP)

### Summary/check -----------------------------------------------------------------

summary(pfc)

# plot total unit costs by service/HA/age
ggplot(pfc, aes(x = str_wrap(HA_Name, width = 10), y = unit_cost, fill = age_group)) + 
  geom_col(position = position_dodge()) +
  facet_wrap(~service_type, scales = 'free') +
  theme(legend.position = c(0.8, 0.2)) +
  coord_flip()

# Sensitivity analysis ----------------------------------------------------

### Source formulas ---------------------------------------------------------

# run separate script to load all formulas
source('Scripts/PFC_Formulas.R')

# view
pfc_functions <- ls(pattern = 'PFC\\.')
lapply(pfc_functions, function(x) get(x))
ldply(paste0(pfc_functions, '()'), function(x) eval(parse(text = x)))
PFC.ed(); PFC.fmed(); PFC.hosp(); PFC.virtual()

### Wrap function approach --------------------------------------------------

# wrap functions
pfc_combined <- function(wage = 30.54, distance = 35, duration = 0.75, 
                         age = 40, meal = 15, parking = 5, acuity = 4, accommodation = 0) {
  ed <- PFC.ed(wage, distance, duration, age, meal, parking, acuity)
  fmed <- PFC.fmed(wage, distance, duration, age, meal, parking)
  hosp <- PFC.hosp(wage, distance, duration, age, meal, accommodation, parking)
  virtual <- PFC.virtual(wage, age)
  tibble(ed, fmed, hosp, virtual)
}

# check
pfc_combined()
pfc_combined(wage = wage, distance = 50)
pfc_combined(distance = 0, parking = 0, age = -1)

# iterate over different parameter values --- wage and distance
sa1 <- expand.grid(wage = seq(15, 100, by = 5), distance = seq(0, 500, by = 10)) %>% 
  ddply(., .(wage, distance), function(data) {
    pfc_combined(wage = data$wage, distance = data$distance)
    })
sa1
sa1 %>% 
  # select(-virtual) %>% 
  gather(service, cost, -wage, -distance) %>% 
  ggplot(aes(x = wage, y = cost, group = wage)) + geom_boxplot() +
  facet_wrap(~service, scales = 'free')
