
### source this script and pass parameters to calculate unit costs

# Create output matrix ----------------------------------------------------

# expand into table
pfc <- tibble(expand.grid(HA_Name = HA_Name, service_type = service_type, 
                          age_group = age_group, stringsAsFactors = F))

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
appt_time <- tibble(service_type = service_type,
                    appt_time = c(MD_time, ED_CTAS_1_LOS, ED_CTAS_4_LOS, Hosp_LOS, virtual_time))

### join to main table
pfc <- left_join(pfc, appt_time)

### Caregiver coefficients ---------------------------------------------------

# add directly to main table by age/service

if(exists("caregivers") == FALSE) {
  pfc <- pfc %>% 
    mutate(caregiver_coeff = case_when(age_group == '0-14' & service_type != 'Hosp' ~ 1,
                                       age_group == '0-14' & service_type == 'Hosp' ~ 0.75,
                                       age_group != '0-14' & service_type == 'Hosp' ~ 0.25,
                                       age_group != '0-14' & service_type != 'Hosp' ~ 0.5))
} else { pfc }

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

# Total unit cost ---------------------------------------------------------

# sum 3 subunits
pfc <- pfc %>% mutate(unit_cost = subunit_IC + subunit_LP + subunit_OOP)

# Output PFC --------------------------------------------------------------

pfc
