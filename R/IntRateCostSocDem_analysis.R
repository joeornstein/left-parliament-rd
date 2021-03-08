# Close Elections and the Interest Rate Cost of Social Democracy
# Regression Discontinuity estimates of the effect of social democratic plurality in parliament on bond yields

# Version 3.1

# Last Updated by Joe Ornstein (March 11, 2020)

# Change Log:
# v3.1 - Final appendix analyses before submitting to QJPS
# v3.0 - Implementing JOP reviewer suggestions 
# v2.2 - List of low-fragmentation and high-fragmentation country-years
# v2.1 - Cleaned up code
# v2.0 - Cleaned up code
# v1.9 - New Depvar: Exchange rates (per SDR)
# v1.8 - New Depvar: change in bond yields relative to OECD average
# v1.7 - Communists included in 'socDemSeats'; heterogeneous Tfx by ideal point difference
# v1.6 - Covariate balance tests and RD with covariate controls
# v1.5 - Added post-1998 PECs and more historical bond yields
# v1.4 - Cleaned up
# v1.3 - Added ENPP Computations


## Load Packages & Data ----

library(tidyverse)
library(magrittr)
library(rdrobust)
library(cowplot)

# Merge and Clean Dataset
# source("R/IntRateCostSocDem_preprocessing.R") 

# Load Cleaned Dataset
elections <- read_csv("data/IntRateCostSocDem_dataset_v2_1.csv")

# Drop Switzerland, because of the "Magic Formula"
elections %<>% filter(country_name != "Switzerland")


## Section 1: How many elections are "close"? ----

elections %>%
  group_by(!is.na(bond.yield.t), 
           leftPluralityPercentage < 0.05 & leftPluralityPercentage > -0.05) %>%
  summarise(num = n())


## Section 4.1:  Testing the Mechanism ----

# Does a slight plurality of social democratic seats yield a discontinuity in probability of
# having a social democratic party in your cabinet? (Depends on party fragmentation)

ggRDFirstStage <- function(enppThreshold){
  
  ggleft1 <- elections %>% filter(enpp < enppThreshold, leftPluralityPercentage < 0)
  nleft1 <- ggleft1 %>% filter(!is.na(cabinetLeft)) %>% nrow
  ggright1 <- elections %>% filter(enpp < enppThreshold, leftPluralityPercentage > 0)
  nright1 <- ggright1 %>% filter(!is.na(cabinetLeft)) %>% nrow
  
  leftPanel <- ggplot() + 
    geom_point(data = ggleft1, aes(x=leftPluralityPercentage,y=cabinetLeft), color = 'gray') +
    geom_point(data = ggright1, aes(x=leftPluralityPercentage,y=cabinetLeft), color = 'gray') +
    geom_smooth(data = ggleft1, aes(x=leftPluralityPercentage, y= cabinetLeft), method = 'loess', se=F, color = 'black') +
    geom_smooth(data = ggright1, aes(x=leftPluralityPercentage, y= cabinetLeft), method = 'loess', se=F, color = 'black') +
    xlab("Left Party Plurality") + ylab("Left Party in Cabinet") + 
    ggtitle(paste0("Low Fragmentation (n = ", nleft1+nright1, ")")) +
    xlim(-1,1) + ylim(0,1) + theme_bw() + 
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggleft2 <- elections %>% filter(enpp > enppThreshold, leftPluralityPercentage < 0)
  nleft2 <- ggleft2 %>% filter(!is.na(cabinetLeft)) %>% nrow
  ggright2 <- elections %>% filter(enpp > enppThreshold, leftPluralityPercentage > 0)
  nright2 <- ggright2 %>% filter(!is.na(cabinetLeft)) %>% nrow
  
  rightPanel <- ggplot() + 
    geom_point(data = ggleft2, aes(x=leftPluralityPercentage,y=cabinetLeft), color = 'gray') +
    geom_point(data = ggright2, aes(x=leftPluralityPercentage,y=cabinetLeft), color = 'gray') +
    geom_smooth(data = ggleft2, aes(x=leftPluralityPercentage, y= cabinetLeft), method = 'loess', se=F, color = 'black') +
    geom_smooth(data = ggright2, aes(x=leftPluralityPercentage, y= cabinetLeft), method = 'loess', se=F, color = 'black') +
    xlab("Left Party Plurality") + ylab("Left Party in Cabinet") + 
    ggtitle(paste0("High Fragmentation (n = ", nleft2+nright2, ")")) +
    xlim(-1,1) + ylim(0,1) + theme_bw() + 
    geom_vline(xintercept = 0, linetype = "dashed")
  
  plot_grid(leftPanel, rightPanel)
}


enppThreshold <- 3.5
ggRDFirstStage(enppThreshold)

ggsave("paper/figures/Figure2.png", scale = 2, width = 4, height = 2)


Y_all <- elections$cabinetLeft
X_all <- elections$leftPluralityPercentage

XlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(leftPluralityPercentage)
YlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(cabinetLeft)

XhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(leftPluralityPercentage)
YhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(cabinetLeft)

rdplot(y = Y_all, x = X_all, p = 1) 
rdplot(y = YlowENPP, x = XlowENPP, p = 1) 
rdplot(y = YhighENPP, x = XhighENPP, p = 1) 


rdModelAll <- rdrobust(y = Y_all, x = X_all, c = 0)
rdModelLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0)
rdModelHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0)


summary(rdModelAll)
rdModelAll$N %>% sum
rdModelAll$Nh %>% sum
rdModelAll$bws['h','left']
rdModelAll$coef['Bias-Corrected',]
paste0("[",rdModelAll$coef['Bias-Corrected',] - 1.96 * rdModelAll$se['Robust',],
       ", ", rdModelAll$coef['Bias-Corrected',] + 1.96 * rdModelAll$se['Robust',], "]")

summary(rdModelLowENPP)
rdModelLowENPP$N %>% sum
rdModelLowENPP$Nh %>% sum
rdModelLowENPP$bws['h','left']
rdModelLowENPP$coef['Bias-Corrected',]
paste0("[",rdModelLowENPP$coef['Bias-Corrected',] - 1.96 * rdModelLowENPP$se['Robust',],
       ", ", rdModelLowENPP$coef['Bias-Corrected',] + 1.96 * rdModelLowENPP$se['Robust',], "]")

summary(rdModelHighENPP)
rdModelHighENPP$N %>% sum
rdModelHighENPP$Nh %>% sum
rdModelHighENPP$bws['h','left']
rdModelHighENPP$coef['Bias-Corrected',]
paste0("[",rdModelHighENPP$coef['Bias-Corrected',] - 1.96 * rdModelHighENPP$se['Robust',],
       ", ", rdModelHighENPP$coef['Bias-Corrected',] + 1.96 * rdModelHighENPP$se['Robust',], "]")


## Section 4.2: Balance Tests ----

ggBalance <- function(enppThreshold){
  
  ggleft <- elections %>% filter(enpp < enppThreshold, leftPluralityPercentage < 0)
  ggright <- elections %>% filter(enpp < enppThreshold, leftPluralityPercentage > 0)
  gdp_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=gdppc_WDI)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=gdppc_WDI)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= gdppc_WDI), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= gdppc_WDI), se=F) +
    xlab("Social Democratic Plurality") + ylab("GDP Per Capita") + 
    ggtitle("GDP Per Capita") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(gdppc_WDI) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(gdppc_WDI) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  polity_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=polity2_P4)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=polity2_P4)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= polity2_P4), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= polity2_P4), se=F) +
    xlab("Social Democratic Plurality") + ylab("Polity IV") + 
    ggtitle("Polity IV") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(polity2_P4) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(polity2_P4) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  population_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=logpop)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=logpop)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= logpop), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= logpop), se=F) +
    xlab("Social Democratic Plurality") + ylab("Log Population") + 
    ggtitle("Log Population") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(logpop) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(logpop) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  debt_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=debt_cgov_WDI)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=debt_cgov_WDI)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= debt_cgov_WDI), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= debt_cgov_WDI), se=F) +
    xlab("Social Democratic Plurality") + ylab("Debt/GDP") + 
    ggtitle("Central Government Debt (% of GDP)") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(debt_cgov_WDI) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(debt_cgov_WDI) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  expenditures_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=exp_WDI)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=exp_WDI)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= exp_WDI), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= exp_WDI), se=F) +
    xlab("Social Democratic Plurality") + ylab("Government Expenditures/GDP") + 
    ggtitle("Government Expenditures (% of GDP)") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(exp_WDI) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(exp_WDI) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  taxes_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=tax_rev_WDI)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=tax_rev_WDI)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= tax_rev_WDI), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= tax_rev_WDI), se=F) +
    xlab("Social Democratic Plurality") + ylab("Tax Revenue/GDP") + 
    ggtitle("Tax Revenue (% of GDP)") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(tax_rev_WDI) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(tax_rev_WDI) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  inflation_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=inflation_WDI)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=inflation_WDI)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= inflation_WDI), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= inflation_WDI), se=F) +
    xlab("Social Democratic Plurality") + ylab("Inflation") + 
    ggtitle("Inflation (Annual % Change)") +
    ylim(elections %>% filter(enpp < enppThreshold) %>% use_series(inflation_WDI) %>% min,
         elections %>% filter(enpp < enppThreshold) %>% use_series(inflation_WDI) %>% max) +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  oecd_panel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft, aes(x=leftPluralityPercentage,y=oecd.average.tplus1 - oecd.average.t)) +
    geom_point(color = 'gray', data = ggright, aes(x=leftPluralityPercentage,y=oecd.average.tplus1 - oecd.average.t)) +
    geom_smooth(color = 'black', data = ggleft, aes(x=leftPluralityPercentage, y= oecd.average.tplus1 - oecd.average.t), se=F) +
    geom_smooth(color = 'black', data = ggright, aes(x=leftPluralityPercentage, y= oecd.average.tplus1 - oecd.average.t), se=F) +
    xlab("Social Democratic Plurality") + ylab("Bond Yield Change") + 
    ggtitle("OECD Average Bond Yield Change") +
    xlim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
    
  
  plot_grid(gdp_panel, polity_panel, population_panel, debt_panel,
            expenditures_panel, taxes_panel, inflation_panel, oecd_panel)
}

elections$logpop <- log(elections$pop_WDI)
ggBalance(enppThreshold)
ggsave("paper/figures/Figure3.png", scale = 2)

rdBalance <- function(enppThreshold, covariate){
  
  df <- elections %>% 
    filter(enpp < enppThreshold, !is.na(covariate))
  
  Y <- df[,covariate] %>% unlist
  
  X <- df$leftPluralityPercentage
  
  rdrobust(y = Y, x = X, c = 0) %>% return
}

# Population Balance
popBalance <- rdBalance(enppThreshold, covariate = "logpop")
summary(popBalance)

# GDP Per Capita Balance
gdpBalance <- rdBalance(enppThreshold, covariate = "gdppc_WDI")
summary(gdpBalance)

# Polity Balance
polityBalance <- rdBalance(enppThreshold, covariate = "polity2_P4")
summary(polityBalance)

# Expenditures Balance
expBalance <- rdBalance(enppThreshold, covariate = "exp_WDI")
summary(expBalance) 

# Taxes Balance
taxBalance <- rdBalance(enppThreshold, covariate = "tax_rev_WDI")
summary(taxBalance) 

# Inflation Balance
infBalance <- rdBalance(enppThreshold, covariate = "inflation_WDI")
summary(infBalance) 

# OECD Average Balance
elections$oecd.movement <- elections$oecd.average.tplus1 - elections$oecd.average.t
oecdBalance <- rdBalance(enppThreshold, covariate = "oecd.movement")
summary(oecdBalance) 

## Section 4.3: Bond Market Regression Discontinuity ----

ggRD <- function(df, enppThreshold, depvar, yearSubset = 1940:2020, ylabel = "Bond Yield Change (1 Month)"){
  
  Y <- df[,depvar] %>% unlist %>% as.numeric %>% na.omit
  df$Y <- df[,depvar] %>% unlist %>% as.numeric
  
  ggleft1 <- df %>% filter(enpp < enppThreshold, leftPluralityPercentage < 0, election_year %in% yearSubset)
  nleft1 <- ggleft1 %>% filter(!is.na(Y)) %>% nrow
  ggright1 <- df %>% filter(enpp < enppThreshold, leftPluralityPercentage > 0, election_year %in% yearSubset)
  nright1 <- ggright1 %>% filter(!is.na(Y)) %>% nrow
  
  leftPanel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft1, aes(x=leftPluralityPercentage,y=Y),size=0.75) +
    geom_point(color = 'gray', data = ggright1, aes(x=leftPluralityPercentage,y=Y),size=0.75) +
    geom_smooth(color = 'black', data = ggleft1, aes(x=leftPluralityPercentage, y= Y), se=T) +
    geom_smooth(color = 'black', data = ggright1, aes(x=leftPluralityPercentage, y=Y), se=T) +
    xlab("Left Party Plurality") + ylab(ylabel) + 
    ggtitle(paste0("Low Fragmentation (n = ", nleft1+nright1, ")")) +
    xlim(-1,1) + ylim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  ggleft2 <- df %>% filter(enpp > enppThreshold, leftPluralityPercentage < 0, election_year %in% yearSubset)
  nleft2<- ggleft2 %>% filter(!is.na(Y)) %>% nrow
  ggright2 <- df %>% filter(enpp > enppThreshold, leftPluralityPercentage > 0, election_year %in% yearSubset)
  nright2 <- ggright2 %>% filter(!is.na(Y)) %>% nrow
  rightPanel <- ggplot() + 
    geom_point(color = 'gray', data = ggleft2, aes(x=leftPluralityPercentage,y=Y), size = 0.75) +
    geom_point(color = 'gray', data = ggright2, aes(x=leftPluralityPercentage,y=Y), size = 0.75) +
    geom_smooth(color = 'black', data = ggleft2, aes(x=leftPluralityPercentage, y= Y), se=T) +
    geom_smooth(color = 'black', data = ggright2, aes(x=leftPluralityPercentage, y= Y), se=T) +
    xlab("Left Party Plurality") + ylab(ylabel) + 
    ggtitle(paste0("High Fragmentation (n = ", nleft2+nright2, ")")) +
    xlim(-1,1) + ylim(-1,1) + theme_bw() + geom_vline(xintercept = 0, linetype = "dashed")
  
  plot_grid(leftPanel, rightPanel)
}

elections$bond.market.response <- (elections$bond.yield.tplus1 - elections$bond.yield.t)

ggRD(elections, enppThreshold, depvar = "bond.market.response")

ggsave("paper/figures/Figure4.png", scale = 2, width = 4, height = 2)


Y_all <- elections$bond.market.response
YlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(bond.market.response)
YhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(bond.market.response)

X_all <- elections$leftPluralityPercentage
XlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(leftPluralityPercentage)
XhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(leftPluralityPercentage)


rdplot(y = Y_all, x = X_all, p = 1)
rdplot(y = YlowENPP, x = XlowENPP, p = 1)
rdplot(y = YhighENPP, x = XhighENPP, p = 1)

rdModelAll <- rdrobust(y = Y_all, x = X_all, c = 0)
rdModelLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0)
rdModelHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0)

summary(rdModelAll)
summary(rdModelLowENPP)
summary(rdModelHighENPP)

summary(rdModelAll)
rdModelAll$N %>% sum
rdModelAll$Nh %>% sum
rdModelAll$bws['h','left']
rdModelAll$coef['Bias-Corrected',]
paste0("[",rdModelAll$coef['Bias-Corrected',] - 1.96 * rdModelAll$se['Robust',],
       ", ", rdModelAll$coef['Bias-Corrected',] + 1.96 * rdModelAll$se['Robust',], "]")

summary(rdModelLowENPP)
rdModelLowENPP$N %>% sum
rdModelLowENPP$Nh %>% sum
rdModelLowENPP$bws['h','left']
rdModelLowENPP$coef['Bias-Corrected',]
paste0("[",rdModelLowENPP$coef['Bias-Corrected',] - 1.96 * rdModelLowENPP$se['Robust',],
       ", ", rdModelLowENPP$coef['Bias-Corrected',] + 1.96 * rdModelLowENPP$se['Robust',], "]")

summary(rdModelHighENPP)
rdModelHighENPP$N %>% sum
rdModelHighENPP$Nh %>% sum
rdModelHighENPP$bws['h','left']
rdModelHighENPP$coef['Bias-Corrected',]
paste0("[",rdModelHighENPP$coef['Bias-Corrected',] - 1.96 * rdModelHighENPP$se['Robust',],
       ", ", rdModelHighENPP$coef['Bias-Corrected',] + 1.96 * rdModelHighENPP$se['Robust',], "]")




## Section 5:  Heterogeneous Treatment Effects -----

getRDEstimate <- function(enppThreshold, ipd.min = 0, yearSubset = 1940:2020, depvar){
  
  df <- elections %>% filter(enpp < enppThreshold, 
                             idealPointDifference >= ipd.min, 
                             election_year %in% yearSubset)
  
  Y <- df[,depvar] %>% unlist %>% as.numeric 
  X <- df$leftPluralityPercentage
  
  rdModel <- rdrobust(y = Y, x = X, c = 0)
  
  estimate <- rdModel$coef['Bias-Corrected',]
  ci95low <- estimate - 1.96 * rdModel$se['Robust',]
  ci95high <- estimate + 1.96 * rdModel$se['Robust',]
  n <- sum(rdModel$N)
  
  return(c(estimate, ci95low, ci95high, n))
}

# Heterogeneous Treatment Effects by Ideological Distance
df <- tibble(ipd.min = seq(0,3.5,0.25),
             estimate = NA,
             ci95low = NA,
             ci95high = NA,
             n = NA)
for(i in 1:nrow(df)){
  rdResults <- getRDEstimate(enppThreshold = enppThreshold, ipd.min = df$ipd.min[i], depvar = "bond.market.response")
  df$estimate[i] <- rdResults[1]
  df$ci95low[i] <- rdResults[2]
  df$ci95high[i] <- rdResults[3]
  df$n[i] <- rdResults[4]
}

ggplot(df, aes(x=ipd.min, y=estimate)) + geom_point() +
  geom_errorbar(aes(ymin = ci95low, ymax = ci95high), width = 0) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() + xlab("Minimum Ideological Distance") + ylab("Estimate")

ggsave("paper/figures/Figure5.png", scale = 1)


# Heterogeneous Treatment EFfect, Varying Historical Era
df <- tibble(yearMin = 1945:1990,
             yearMax = 1975:2020,
             estimate = NA,
             ci95low = NA,
             ci95high = NA,
             n = NA)

for(i in 1:nrow(df)){
  rdResults <- getRDEstimate(enppThreshold = 3.5, 
                             yearSubset = df$yearMin[i]:df$yearMax[i], 
                             depvar = 'bond.market.response')
  df$estimate[i] <- rdResults[1]
  df$ci95low[i] <- rdResults[2]
  df$ci95high[i] <- rdResults[3]
  df$n[i] <- rdResults[4]
}

ggplot(df, aes(x=yearMin, y=estimate)) + geom_point() +
  geom_errorbar(aes(ymin = ci95low, ymax = ci95high), width = 0) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() + xlab("Year") + ylab("Estimate")

ggsave("paper/figures/Figure6.png", scale = 1)


## Appendix A.1: Low Fragmentation and High Fragmentation Country-Years ----

elections %>%
  filter(!is.na(bond.yield.t),
         enpp < 3.5) %>%
  mutate(country_year = paste0(country_name, election_year)) %>%
  use_series(country_year) %>%
  sort

elections %>%
  filter(!is.na(bond.yield.t),
         enpp > 3.5) %>%
  mutate(country_year = paste0(country_name, election_year)) %>%
  use_series(country_year) %>%
  sort
  
ggdat <- elections %>%
  filter(!is.na(bond.yield.t)) %>%
  mutate(country_year = paste0(country_name_short, election_year)) 

ggplot(ggdat, aes(x = enpp, y=factor(country_name))) + geom_point() +
  xlab("ENPP") + ylab("Country") + theme_bw() + geom_vline(xintercept=3.5, linetype='dashed')

#Change the factor levels to get a prettier plot
correctOrder <- ggdat %>%
  group_by(country_name) %>%
  summarise(mean_ENPP = mean(enpp)) %>%
  arrange(mean_ENPP) %>%
  use_series(country_name)

ggdat$country_name <- factor(ggdat$country_name, levels = correctOrder)

ggplot(ggdat, aes(x = enpp, y=country_name)) + geom_point() +
  xlab("ENPP") + ylab("Country") + theme_bw() + geom_vline(xintercept=3.5, linetype='dashed')

ggsave("paper/figures/Figure7.png", scale = 1.1)


## Appendix A.1: Vary the ENPP Threshold ----

df <- tibble(enppThreshold = seq(2.5,12,0.1),
             estimate = NA,
             ci95low = NA,
             ci95high = NA,
             n = NA)
for(i in 1:nrow(df)){
  rdResults <- getRDEstimate(enppThreshold = df$enppThreshold[i], ipd.min = 0, 
                             depvar = 'bond.market.response')
  df$estimate[i] <- rdResults[1]
  df$ci95low[i] <- rdResults[2]
  df$ci95high[i] <- rdResults[3]
  df$n[i] <- rdResults[4]
}

ggplot(df, aes(x=enppThreshold, y=estimate)) + geom_point() +
  geom_errorbar(aes(ymin = ci95low, ymax = ci95high), width = 0) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() + xlab("Maximum ENPP") + ylab("Estimate") +
  scale_x_continuous(breaks = 1:12)

ggsave("paper/figures/Figure8.png", scale = 1)


## Appendix A.2: RD with covariates ----

rdData <- elections %>% filter(!is.na(bond.market.response),
                               !is.na(gdppc_WDI),
                               !is.na(logpop), 
                               !is.na(inflation_WDI),
                               !is.na(exp_WDI), 
                               !is.na(tax_rev_WDI))

Y_all <- rdData$bond.market.response
X_all <- rdData$leftPluralityPercentage
covs_all <- rdData %>% 
  select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI) %>%
  as.matrix

YlowENPP <- rdData %>% filter(enpp < enppThreshold) %>% use_series(bond.market.response)
XlowENPP <- rdData %>% filter(enpp < enppThreshold) %>% use_series(leftPluralityPercentage)
covslowenpp <- rdData %>% 
  filter(enpp < enppThreshold) %>%
  select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI) %>%
  as.matrix

YhighENPP <- rdData %>% filter(enpp >= enppThreshold) %>% use_series(bond.market.response)
XhighENPP <- rdData %>% filter(enpp >= enppThreshold) %>% use_series(leftPluralityPercentage)
covshighenpp <- rdData %>% 
  filter(enpp >= enppThreshold) %>%
  select(gdppc_WDI, logpop, inflation_WDI, exp_WDI, tax_rev_WDI) %>%
  as.matrix

rdWCovs_all <- rdrobust(y = Y_all, x = X_all, c = 0, covs=covs_all)
rdWCovsLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0, covs = covslowenpp)
rdWCovsHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0, covs = covshighenpp)

summary(rdWCovs_all)
rdWCovs_all$N %>% sum
rdWCovs_all$bws['h','left']
rdWCovs_all$coef['Bias-Corrected',]
paste0("[",rdWCovs_all$coef['Bias-Corrected',] - 1.96 * rdWCovs_all$se['Robust',],
       ", ", rdWCovs_all$coef['Bias-Corrected',] + 1.96 * rdWCovs_all$se['Robust',], "]")

summary(rdWCovsLowENPP)
rdWCovsLowENPP$N %>% sum
rdWCovsLowENPP$bws['h','left']
rdWCovsLowENPP$coef['Bias-Corrected',]
paste0("[",rdWCovsLowENPP$coef['Bias-Corrected',] - 1.96 * rdWCovsLowENPP$se['Robust',],
       ", ", rdWCovsLowENPP$coef['Bias-Corrected',] + 1.96 * rdWCovsLowENPP$se['Robust',], "]")

summary(rdWCovsHighENPP)
rdWCovsHighENPP$N %>% sum
rdWCovsHighENPP$bws['h','left']
rdWCovsHighENPP$coef['Bias-Corrected',]
paste0("[",rdWCovsHighENPP$coef['Bias-Corrected',] - 1.96 * rdWCovsHighENPP$se['Robust',],
       ", ", rdWCovsHighENPP$coef['Bias-Corrected',] + 1.96 * rdWCovsHighENPP$se['Robust',], "]")



## Appendix A.3: Sensitivity to Bandwidth ----

rdData <- elections %>% filter(enpp < enppThreshold)

Y <- rdData$bond.market.response
X <- rdData$leftPluralityPercentage


df <- expand.grid(h = seq(0.05, 0.3, 0.01)) %>%
  mutate(b = 2 * h,
         estimate = NA,
         ci95low = NA,
         ci95high = NA,
         effectiveN = NA)

for (i in 1:nrow(df)){
  rdModel <- rdrobust(y = Y, x = X, c = 0, p = 1,
                      h = df$h[i], b = df$b[i])
  df$estimate[i] <- rdModel$coef["Bias-Corrected",]
  df$ci95low[i] <- rdModel$coef["Bias-Corrected",] - 1.96 * rdModel$se["Robust",]
  df$ci95high[i] <- rdModel$coef["Bias-Corrected",] + 1.96 * rdModel$se["Robust",]
  df$effectiveN[i] <- sum(rdModel$Nh)
}

ggplot(df, aes(x=h, y=estimate)) + geom_point(colour = 'black') +
  geom_point(data = df %>% filter(h == 0.14), colour = 'red') +
  geom_errorbar(aes(ymin = ci95low, ymax = ci95high), width = 0) + 
  geom_errorbar(data = df %>% filter(h == 0.14), aes(ymin = ci95low, ymax = ci95high), width = 0, colour = 'red') + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() + xlab("Bandwidth") + ylab("Estimate")

ggsave("paper/figures/Figure9.png", scale = 1)



## Appendix A.4: Sensitivity to Polynomial Order / Kernel ----

rdData <- elections

Y_all <- rdData$bond.market.response
X_all <- rdData$leftPluralityPercentage

YlowENPP <- rdData %>% filter(enpp < enppThreshold) %>% use_series(bond.market.response)
XlowENPP <- rdData %>% filter(enpp < enppThreshold) %>% use_series(leftPluralityPercentage)

YhighENPP <- rdData %>% filter(enpp >= enppThreshold) %>% use_series(bond.market.response)
XhighENPP <- rdData %>% filter(enpp >= enppThreshold) %>% use_series(leftPluralityPercentage)

LinearTriangularAll <- rdrobust(y = Y_all, x = X_all, c = 0, p = 1, kernel = "tri")
LinearTriangularLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0, p = 1, kernel = "tri")
LinearTriangularHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0, p = 1, kernel = "tri")

LinearUniformAll <- rdrobust(y = Y_all, x = X_all, c = 0, p = 1, kernel = "uni")
LinearUniformLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0, p = 1, kernel = "uni")
LinearUniformHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0, p = 1, kernel = "uni")

summary(LinearUniformAll)
LinearUniformAll$coef["Bias-Corrected",]
summary(LinearUniformLowENPP)
LinearUniformLowENPP$coef["Bias-Corrected",]
summary(LinearUniformHighENPP)
LinearUniformHighENPP$coef["Bias-Corrected",]

QuadraticTriangularAll <- rdrobust(y = Y_all, x = X_all, c = 0, p = 2, kernel = "tri")
QuadraticTriangularLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0, p = 2, kernel = "tri")
QuadraticTriangularHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0, p = 2, kernel = "tri")

summary(QuadraticTriangularAll)
QuadraticTriangularAll$coef["Bias-Corrected",]
summary(QuadraticTriangularLowENPP)
QuadraticTriangularLowENPP$coef["Bias-Corrected",]
summary(QuadraticTriangularHighENPP)
QuadraticTriangularHighENPP$coef["Bias-Corrected",]

QuadraticUniformAll <- rdrobust(y = Y_all, x = X_all, c = 0, p = 2, kernel = "uni")
QuadraticUniformLowENPP <- rdrobust(y = YlowENPP, x = XlowENPP, c = 0, p = 2, kernel = "uni")
QuadraticUniformHighENPP <- rdrobust(y = YhighENPP, x = XhighENPP, c = 0, p = 2, kernel = "uni")

summary(QuadraticUniformAll)
QuadraticUniformAll$coef["Bias-Corrected",]
summary(QuadraticUniformLowENPP)
QuadraticUniformLowENPP$coef["Bias-Corrected",]
summary(QuadraticUniformHighENPP)
QuadraticUniformHighENPP$coef["Bias-Corrected",]



## Section A.5:  Dynamic RD ----

dynamicRDBondYields <- function(df,
                                enpp.min = 0, enpp.max = 11, 
                                ipd.min = 0, ipd.max = 8,
                                ymin = NA, ymax = NA,
                                title = "Dynamic RD Estimates"){
  
  df %<>% 
    filter(enpp > enpp.min, enpp < enpp.max) %>%
    filter(idealPointDifference > ipd.min, idealPointDifference < ipd.max)
  
  t <- seq(-12,12,1)
  fig2data <- tibble(t, RDestimate = NA, RDse = NA)
  
  for (i in 1:nrow(fig2data)){
    t <- fig2data$t[i] %>% as.character %>% 
      paste0("plus",.) %>%
      str_replace("-","minus") %>%
      str_replace("plusminus","minus")
    
    if(t == "plus0") {next} #Don't compute for t = 0
    
    df$bond.yield.change <- df[,paste0("bond.yield.t",t)] %>% unlist %>% 
      subtract(df$bond.yield.t)
    
    df %<>% filter(!is.na(bond.yield.change), !is.na(leftPluralityPercentage))
    
    Y <- df$bond.yield.change
    X <- df$leftPluralityPercentage
    
    #rdplot(y = Y, x = X, p = 1) 
    fig2data$RDestimate[i] <- rdrobust(y = Y, x = X, c = 0)$coef[1]
    fig2data$RDse[i] <- rdrobust(y = Y, x = X, c = 0)$se[2]
    
  }
  
  ggplot(fig2data, aes(x=t, y=RDestimate)) + 
    geom_point() + geom_line() + 
    geom_errorbar(aes(ymin = RDestimate - 1.96 * RDse, 
                      ymax = RDestimate + 1.96 * RDse), width = 0) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0, linetype="dashed") +
    xlab("Months Before/After Election") +
    ylab("Estimated Effect on Bond Yields") +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ggtitle(paste0(title, ", n = ", nrow(df))) +
    theme_bw()
}

dynamicRDLowENPP <- dynamicRDBondYields(df = elections, enpp.min = 0, enpp.max = enppThreshold, 
                                        title = "Low Fragmentation")
dynamicRDHighENPP <- dynamicRDBondYields(df = elections, enpp.min = enppThreshold, enpp.max = 11,
                                         title = "High Fragmentation")
plot_grid(dynamicRDLowENPP, dynamicRDHighENPP, nrow = 2)
ggsave("paper/figures/Figure10.png", scale = 1.5)


## Section A.6: Multiple Cutoff Regression Discontinuity Design ----

library(rdmulti)

# Generate election-specific cutoffs
elections$cutoff <- elections$LargestOtherSeats / elections$seats_total

# Generate seat share
elections$social_democratic_seat_share <- elections$SocDemSeats / elections$seats_total

# Filter out the NAs
elections_filtered <- elections %>% 
  filter(!is.na(cutoff),
         !is.na(social_democratic_seat_share),
         !is.na(bond.market.response)) %>%
  select(cutoff, social_democratic_seat_share, bond.market.response)

# Slice the cutoffs into discrete intervals (see Catteneo et al., 2016 JOP)
breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
elections_filtered$cutoff_interval <- cut(elections_filtered$cutoff,
                                          breaks = c(breaks, 1),
                                          labels = breaks) %>%
  as.character %>%
  as.numeric

rd_multiple_cutoffs <- rdmc(Y = elections_filtered$bond.market.response, 
                            X = elections_filtered$social_democratic_seat_share, 
                            C = elections_filtered$cutoff)

# It may be that continuous cutoffs is inappropriate for this approach. 

## Section A.7: Gaussian Process Regression Discontinuity ----

library(gprd)

Y_all <- elections$bond.market.response
YlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(bond.market.response)
YhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(bond.market.response)

X_all <- elections$leftPluralityPercentage
XlowENPP <- elections %>% filter(enpp < enppThreshold) %>% use_series(leftPluralityPercentage)
XhighENPP <- elections %>% filter(enpp > enppThreshold) %>% use_series(leftPluralityPercentage)

xlab <- 'Social Democratic Plurality'
ylab <- 'Bond Yield Change (1 Month)'

# All Elections
gprd_all <- gprd(x = X_all, y = Y_all)
plot(gprd_all)
summary(gprd_all)

# Low Fragmentation
gprd_lowENPP <- gprd(x = XlowENPP, y = YlowENPP)
summary(gprd_lowENPP)

pdf('paper/figures/Figure11a.pdf', height = 5, width = 8)
plot(gprd_lowENPP, xlab = xlab, ylab = ylab)
dev.off()

# High Fragmentation
gprd_highENPP <- gprd(x = XhighENPP, y = YhighENPP)
summary(gprd_highENPP)

pdf('paper/figures/Figure11b.pdf', height = 5, width = 8)
plot(gprd_highENPP, xlab = xlab, ylab = ylab)
dev.off()



