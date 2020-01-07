# dat proc

library(tidyverse)
library(readxl)
library(forcats)
library(stringi)
library(here)

##
# create restdat, reststat

# format raw data
raw <- read.csv(here('data-raw', 'DRAFT_TBEP_Combined_Projects_1971-2017_06222018.csv'), stringsAsFactors = F) %>% 
  rename(
    date = Completion_Date, 
    type = Project_Activity, 
    tech = Project_Technology, 
    lat = ProjectLatitude,
    lon = ProjectLongitude
  ) %>% 
  select(date, type, tech, lat, lon) %>% 
  filter(
    lon > -1e6 & lat > 20 & !is.na(date)
  ) %>% 
  mutate(
    id = stri_rand_strings(nrow(.), length = 4),
    top = fct_recode(type,
                     hab = 'Habitat_Enhancement',
                     hab = 'Habitat_Establishment', 
                     hab = 'Habitat_Protection', 
                     wtr = 'Nonpoint_Source', 
                     wtr = 'Point_Source'
    ),
    type = fct_recode(type,
                      hab_enh = 'Habitat_Enhancement',
                      hab_est = 'Habitat_Establishment',
                      hab_pro = 'Habitat_Protection',
                      non_src = 'Nonpoint_Source',
                      pnt_src = 'Point_Source'
    ),
    tech = toupper(tech),
    date = gsub('\\?$', '', date),
    date = as.numeric(date)
  ) 

# restdat
restdat <- raw %>% 
  select(date, tech, type, top, id)

# reststat
reststat <- raw %>% 
  select(id, lat, lon)

save(restdat, file = here('data', 'restdat.RData'))
save(reststat, file = here('data', 'reststat.RData'))
