## Appendix Results and Robustness Tests
## Version: 1.1
## Last Updated: April 30, 2021

library(tidyverse)
library(rdrobust)
library(cowplot)

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


## Apendix A7 -------------------

# Load Kayser & Lindstadt loss probability measure (available here: http://mark-kayser.com/data.html)
lpr <- read_delim('data/lprdata_distrib_augmented_2015.csv', delim = ';')

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


# Produce Figure

enpp_threshold <- 3.5

data_left <- data %>% filter(enpp < enpp_threshold,
                             leftPluralityPercentage < 0)
data_right <- data %>% filter(enpp < enpp_threshold, 
                              leftPluralityPercentage > 0)

ggplot() + 
  geom_point(color = 'gray', data = data_left, aes(x=leftPluralityPercentage,y=left_party_loss_probability)) +
  geom_point(color = 'gray', data = data_right, aes(x=leftPluralityPercentage,y=left_party_loss_probability)) +
  geom_smooth(color = 'black', data = data_left, 
              aes(x=leftPluralityPercentage, y= left_party_loss_probability)) +
  geom_smooth(color = 'black', data = data_right, 
              aes(x=leftPluralityPercentage, y= left_party_loss_probability)) +
  xlab("Left Party Plurality Margin") + ylab("Ex Ante Left Party Loss Probability") + 
  ylim(data %>% filter(enpp < enpp_threshold) %>% 
         pull(left_party_loss_probability) %>% min,
       data %>% filter(enpp < enpp_threshold) %>% 
         pull(left_party_loss_probability) %>% max) +
  xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")


ggsave('paper/figures/Figure11.png', width = 8, height = 5)

X <- data %>% 
  filter(enpp < enpp_threshold) %>% 
  pull(leftPluralityPercentage)
Y <- data %>% 
  filter(enpp < enpp_threshold) %>% 
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


## Appendix A8: Daily Data --------------------------------
library(janitor)

elections <- read_csv('data/IntRateCostSocDem_dataset_v2_1.csv') %>% 
  clean_names() %>% 
  select(election_id:enpp) %>% 
  rename(country = country_name,
         date = election_date) %>% 
  relocate(country, date, .after = election_id)
  
daily <- read_csv('data/daily.csv')


# match each election with the closest pre-election and post-election bond price
data <- elections %>% 
  select(election_id: date) %>% 
  mutate(pre_election_bond_price = NA,
         post_election_bond_price = NA)

for(i in 1:nrow(data)){
  
  print(i)
  
  pre_election_data_points <- daily %>% 
    filter(country == data$country[i],
           date < data$date[i]) %>% 
    nrow
  
  if(pre_election_data_points == 0){
    print('skip')
    next
  }
  
  data$pre_election_bond_price[i] <- daily %>% 
    filter(country == data$country[i],
           date < data$date[i]) %>% 
    slice_max(date) %>% 
    pull(price)
  
  data$post_election_bond_price[i] <- daily %>% 
    filter(country == data$country[i],
           date > data$date[i]) %>% 
    slice_min(date) %>% 
    pull(price)
  
}

data %>% 
  filter(!is.na(pre_election_bond_price)) %>% 
  nrow

data <- data %>% 
  filter(!is.na(pre_election_bond_price)) %>% 
  mutate(bond_price_movement = post_election_bond_price - pre_election_bond_price) %>% 
  left_join(elections)

# none before 1980, 12 from the 1980s, 29 from the 1990s
table(data$date < '1990-01-01')
table(data$date > '1990-01-01' & data$date < '2000-01-01')

# plot regression discontinuity at the plurality cutoff
ggRD <- function(df, enppThreshold, depvar, yearSubset = 1940:2020, ylabel = "Bond Yield Change (1 Month)"){
  
  Y <- df[,depvar] %>% unlist %>% as.numeric %>% na.omit
  df$Y <- df[,depvar] %>% unlist %>% as.numeric
  
  ggleft1 <- df %>% filter(enpp < enppThreshold, left_plurality_percentage < 0, election_year %in% yearSubset)
  nleft1 <- ggleft1 %>% filter(!is.na(Y)) %>% nrow
  ggright1 <- df %>% filter(enpp < enppThreshold, left_plurality_percentage > 0, election_year %in% yearSubset)
  nright1 <- ggright1 %>% filter(!is.na(Y)) %>% nrow
  
  leftPanel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft1, aes(x=left_plurality_percentage,y=Y),size=0.75) +
    geom_point(color = 'gray', data = ggright1, aes(x=left_plurality_percentage,y=Y),size=0.75) +
    geom_smooth(color = 'black', data = ggleft1, aes(x=left_plurality_percentage, y= Y), se=T) +
    geom_smooth(color = 'black', data = ggright1, aes(x=left_plurality_percentage, y=Y), se=T) +
    xlab("Left Party Plurality Margin") + ylab(ylabel) + 
    ggtitle(paste0("Low Fragmentation (n = ", nleft1+nright1, ")")) +
    xlim(-1,1) + ylim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  ggleft2 <- df %>% filter(enpp > enppThreshold, left_plurality_percentage < 0, election_year %in% yearSubset)
  nleft2<- ggleft2 %>% filter(!is.na(Y)) %>% nrow
  ggright2 <- df %>% filter(enpp > enppThreshold, left_plurality_percentage > 0, election_year %in% yearSubset)
  nright2 <- ggright2 %>% filter(!is.na(Y)) %>% nrow
  rightPanel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft2, aes(x=left_plurality_percentage,y=Y), size = 0.75) +
    geom_point(color = 'gray', data = ggright2, aes(x=left_plurality_percentage,y=Y), size = 0.75) +
    geom_smooth(color = 'black', data = ggleft2, aes(x=left_plurality_percentage, y= Y), se=T) +
    geom_smooth(color = 'black', data = ggright2, aes(x=left_plurality_percentage, y= Y), se=T) +
    xlab("Left Party Plurality Margin") + ylab(ylabel) + 
    ggtitle(paste0("High Fragmentation (n = ", nleft2+nright2, ")")) +
    xlim(-1,1) + ylim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  plot_grid(leftPanel, rightPanel)
}

ggRD(data, enppThreshold = 3.5,
     depvar = 'bond_price_movement',
     ylabel = 'Bond Price Movement (1 Day)')

ggsave(filename = 'paper/figures/Figure13.png', width = 8, height = 5)
