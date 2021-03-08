## Appendix Results and Robustness Tests
## Version: 1.0
## Last Updated: Feb 11, 2021

library(tidyverse)
library(rdrobust)

## Load Kayser & Lindstadt loss probability measure (available here: http://mark-kayser.com/data.html)
lpr <- read_delim('data/lprdata_distrib_augmented_2015.csv', delim = ';')

## Load elections dataset
elections <- read_csv("data/IntRateCostSocDem_dataset_v2_1.csv") %>%
  mutate(logpop = log(pop_WDI),
         bond.market.response = bond.yield.tplus1 - bond.yield.t,
         isocode = case_when(country_name_short == 'AUS' ~ 'AUL',
                             TRUE ~ country_name_short),
         plurality_party = if_else(leftPluralityPercentage > 0,
                                   SocDemPartyName, LargestOtherPartyName),
         # harmonize some names
         plurality_party = if_else(plurality_party == 'Fianna Fail (Soldiers of Destiny)',
                                   'Fianna Fail', plurality_party),
         plurality_party = if_else(plurality_party == 'Union for a Popular Movement',
                                   'Union for a Popular Movement | The Republicans',
                                   plurality_party),
         plurality_party = if_else(plurality_party == 'Flemish Christian Peoples Party',
                                   'Flemish Christian Peoples Party | Christian Democrats & Flemish',
                                   plurality_party))

# parlgov party data
parties <- read_csv('data/parlgov_party_2020.csv') %>% 
  mutate(isocode = case_when(country_name_short == 'AUS' ~ 'AUL',
                             TRUE ~ country_name_short))


# pipeline:

# 1. join loss probabilities (lpr) with subsequent election
# 2. identify the previous plurality party
# 3. compute the probability that a left party will lose their plurality (or a non-left party will keep it)
# 4. plot x=leftPluralityPercentage vs. y=left party loss probability computed from previous election

data <- elections %>% 
  select(isocode, 
         elecyr = election_year, 
         elecmo = election_month,
         leftPluralityPercentage, enpp,
         plurality_party, 
         bond.market.response, gdppc_WDI, logpop, 
         inflation_WDI, exp_WDI, tax_rev_WDI) %>% 
  left_join(lpr, by = c('isocode', 'elecyr', 'elecmo')) %>% 
  # lag loss probabilities
  arrange(isocode, elecyr, elecmo) %>% 
  group_by(isocode) %>% 
  # lag previous plurality party and loss probabilities
  mutate(previous_plurality_party = lag(plurality_party),
         loss_probability = lag(lpr)) %>% 
  ungroup %>% 
  # drop election-years missing from Kayser & Lindstadt data
  filter(!is.na(loss_probability)) %>% 
  # merge with Parlgov family names
  left_join(parties %>% 
              select(isocode, previous_plurality_party = party_name_english,
                     previous_party_family = family_name), 
            by = c('isocode', 'previous_plurality_party')) %>% 
  mutate(left_party_loss_probability = if_else(previous_party_family == 'Social democracy',
                                               loss_probability, 1 - loss_probability))




## Appendix figure -------------

enpp_threshold <- 3.5

data_left <- data %>% filter(enpp < enpp_threshold, 
                             !(isocode %in% countries_to_exclude),
                             leftPluralityPercentage < 0)
data_right <- data %>% filter(enpp < enpp_threshold, 
                              !(isocode %in% countries_to_exclude),
                              leftPluralityPercentage > 0)

ggplot() + 
  geom_point(color = 'gray', data = data_left, aes(x=leftPluralityPercentage,y=left_party_loss_probability)) +
  geom_point(color = 'gray', data = data_right, aes(x=leftPluralityPercentage,y=left_party_loss_probability)) +
  geom_smooth(color = 'black', data = data_left, 
              aes(x=leftPluralityPercentage, y= left_party_loss_probability)) +
  geom_smooth(color = 'black', data = data_right, 
              aes(x=leftPluralityPercentage, y= left_party_loss_probability)) +
  xlab("Left Party Plurality") + ylab("Ex Ante Left Party Loss Probability") + 
  ylim(data %>% filter(enpp < enpp_threshold,
                       !(isocode %in% countries_to_exclude)) %>% 
         pull(left_party_loss_probability) %>% min,
       data %>% filter(enpp < enpp_threshold,
                       !(isocode %in% countries_to_exclude)) %>% 
         pull(left_party_loss_probability) %>% max) +
  xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")


ggsave('paper/figures/Figure11.png', width = 8, height = 5)

X <- data %>% 
  filter(enpp < enpp_threshold,
         !(isocode %in% countries_to_exclude),) %>% 
  pull(leftPluralityPercentage)
Y <- data %>% 
  filter(enpp < enpp_threshold,
         !(isocode %in% countries_to_exclude),) %>% 
  pull(left_party_loss_probability)


rdModel <- rdrobust(y = Y, x = X, c = 0)
summary(rdModel)


# include electoral loss probability as a covariate ------------


# All observations
rdrobust(y = data$bond.market.response, 
         x = data$leftPluralityPercentage, 
         c = 0, 
         covs = data %>% 
           select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI, left_party_loss_probability) %>%
           as.matrix
) %>% 
  summary

# Low fragmentation
rdrobust(y = data %>% 
           filter(enpp < enpp_threshold) %>% 
           pull(bond.market.response), 
         x = data %>% 
           filter(enpp < enpp_threshold) %>% 
           pull(leftPluralityPercentage), 
         c = 0, 
         covs = data %>% 
           filter(enpp < enpp_threshold) %>% 
           select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI, left_party_loss_probability) %>%
           as.matrix
) %>% 
  summary


# High fragmentation
rdrobust(y = data %>% 
           filter(enpp > enpp_threshold) %>% 
           pull(bond.market.response), 
         x = data %>% 
           filter(enpp > enpp_threshold) %>% 
           pull(leftPluralityPercentage), 
         c = 0, 
         covs = data %>% 
           filter(enpp > enpp_threshold) %>% 
           select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI, left_party_loss_probability) %>%
           as.matrix
) %>% 
  summary
