## Appendix Results and Robustness Tests
## Version: 1.0
## Last Updated: Feb 11, 2021

library(tidyverse)

## Load Kayser & Lindstadt loss probability measure (available here: http://mark-kayser.com/data.html)
lpr <- read_delim('data/lprdata_distrib_augmented_2015.csv', delim = ';')

## Load elections dataset
elections <- read_csv("data/IntRateCostSocDem_dataset_v2_1.csv")


# pipeline:

# 1. join loss probabilities (lpr) with subsequent election
# 2. plot x=leftPluralityPercentage vs. y=loss probability computed from previous election

data <- elections %>% 
  mutate(isocode = case_when(country_name_short == 'AUS' ~ 'AUL',
                             TRUE ~ country_name_short)) %>% 
  select(isocode, 
         elecyr = election_year, 
         elecmo = election_month,
         leftPluralityPercentage, enpp) %>% 
  left_join(lpr, by = c('isocode', 'elecyr', 'elecmo')) %>% 
  # lag loss probabilities
  arrange(isocode, elecyr, elecmo) %>% 
  group_by(isocode) %>% 
  # lag previous plurality party and loss probabilities
  mutate(previous_plurality_party = lag(PluralityParty),
         loss_probability = lag(lpr)) %>% 
  # compute Shannon entropy measure ('surprise')
  mutate(entropy = if_else(loss_probability == 0, 0,
                           -1 * (loss_probability * log2(loss_probability) + 
                                   (1 - loss_probability) * log2(1 - loss_probability))))
  

data %>% 
  ggplot(aes(x = leftPluralityPercentage, y = entropy)) +
  geom_point() + 
  geom_smooth()



# dross -----

#   
# 
# 
# 
# 
# uk_elections <- elections %>% 
#   filter(country_name_short == 'GBR') %>% 
#   select(country_name_short, election_year, election_month, 
#          leftPluralityPercentage, bond.yield.t, bond.yield.tplus1)
# 
# uk_lpr <- lpr %>% 
#   filter(isocode == 'GBR') %>% 
#   select(country_name_short = isocode, 
#          election_year = elecyr, 
#          election_month = elecmo, 
#          PluralityParty, lpr)
# 
# # join together
# uk <- left_join(uk_elections,
#                 uk_lpr, 
#                 by = c('country_name_short','election_year','election_month')) %>% 
#   group_by(country_name_short) %>% 
#   arrange(election_year, election_month) %>% 
#   # lag previous plurality party and loss probabilities
#   mutate(previous_plurality_party = lag(PluralityParty),
#          loss_probability = lag(lpr),
#          bond_yield_change = bond.yield.tplus1 - bond.yield.t,
#          left_party_elected = as.numeric(leftPluralityPercentage > 0)) %>% 
#   select(-PluralityParty, -lpr)
