#Merge and clean dataset for "Close Elections and the Interest Rate Cost of Social Democracy"
#Version 2.1
#Last Updated by Joe Ornstein (November 30, 2018)

#Change Log:
#v2.1 - Cleaned up
#v2.0 - Cleaned up code
#v1.9 - New depvar: Exchange rates (per SDR)
#v1.8 - New depvar: Interest Rate Change Relative to OECD Average
#v1.7 - Includes the communists
#v1.6 - Merge with country-year covariates
#v1.5 - Post-2000 PECs; merge with country-year covariates for balance tests
#v1.4 - Merges IMF and OECD bond yield data
#v1.3 - Computes coalition market-state scores

library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)

#Load ParlGov dataset ------------------------------
party <- read_delim("~/603 - Data/ParlGov/parlgov2015/party.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

election <- read_delim("~/603 - Data/ParlGov/parlgov2015/election.csv", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

cabinet <- read_delim("~/603 - Data/ParlGov/parlgov2015/cabinet.csv", "\t")


#Load Golder (2005) Pre-electoral Coalitions Data ---------------------------

golder <- read_excel("~/603 - Data/Golder_Coalitions/replication_ES/pec_ES.xls")


#Convert Golder's election date variable (all pre-2000, so this is okay)
year <- golder$election %>% substr(1,2) %>% paste0("19",.) %>% as.numeric
month <- golder$election %>% substr(3,4) %>% as.numeric
day <- golder$election %>% substr(5,6) %>% as.numeric
golder$date <- paste(year,month,day, sep="/") %>% as.Date

#Keep the PECs
pecs <- golder %>% 
  filter(pec == 1) %>%
  select(countryname, date, party1, party2) %>%
  set_colnames(c("country_name","election_date","party1","party2")) 

#Recode UK country name
pecs$country_name %<>% recode(`UK` = "United Kingdom")

#Recode France's "Rally for the Republic" party to match ParlGov ID
pecs$party1 %<>% recode(`31625` = 31621)
pecs$party2 %<>% recode(`31625` = 31621)

#Recode Ireland Sinn Fein Worker's Party to match ParlGov ID
pecs$party1 %<>% recode(`53220` = 53951)
pecs$party2 %<>% recode(`53220` = 53951)

#Recode 1996 Christian Democratic Center (Italy) to match ParlGov ID
pecs$party1 %<>% recode(`32521` = 32522)
pecs$party2 %<>% recode(`32521` = 32522)


#Merge with Ornstein's post-1998 PECs
ornstein <- read_excel("PECs/ornstein_PECs.xlsx") %>%
  select(country_name, election_date, party1, party2) %>%
  filter(!is.na(party1) & party1 != ".") 

pecs %<>% mutate(election_date = as.Date(election_date))
ornstein %<>% mutate(election_date = as.Date(election_date),
                     party1 = as.numeric(party1),
                     party2 = as.numeric(party2))
pecs %<>% bind_rows(ornstein)


#Step 1: Assign CMP IDs from party DF to election DF -------------------------

partyIDs <- party %>% select(country_name, party_name, party_name_short, party_name_english, cmp)

election %<>% left_join(partyIDs)

#Assign CMP to Italian Dini List Parties (1996)
election$cmp[election$party_name == "Lista Dini - Rinnovamento Italiano"] <- 32321


#Step 2: Create monadic version of pecs DF -------------------------------

pecs.dyadic <- pecs

#One row for each unique country-year-party
pecs <- bind_rows(
  pecs.dyadic %>% #Party IDs from party1
    select(country_name, election_date, party1) %>%
    set_colnames(c("country_name","election_date","cmp")),
  pecs.dyadic %>% #Party IDs from party2
    select(country_name, election_date, party2) %>%
    set_colnames(c("country_name","election_date","cmp"))) %>%
  unique #Keep only the unique entries

#Inelegant nested for-loops ahoy.
pecs$coalition <- NA
for(i in pecs$country_name) {
  for(j in pecs$election_date){
    coalitionID <- 1 #Start with coaltion ID 1
    while(pecs %>% filter(country_name == i, election_date == j) %>% 
          use_series(coalition) %>% is.na %>% sum > 0){
      #Execute this code for every party without an assigned coalition ID
      
      #Get the ID of the first unassigned party
      first.cmp <- pecs %>% 
        filter(country_name == i,
               election_date == j,
               coalition %>% is.na) %>%
        use_series(cmp) %>%
        extract(1)
      
      #Get all the parties that are linked to the first party  
      linked.parties1 <- pecs.dyadic %>% 
        filter(country_name == i,
               election_date == j, 
               party1 == first.cmp) %>%
        use_series(party2)
      linked.parties2 <- pecs.dyadic %>%
        filter(country_name == i, 
               election_date == j, 
               party2 == first.cmp) %>%
        use_series(party1)
      coalition.members <- c(first.cmp, linked.parties1, linked.parties2)
      
      
      #Assign the coalition ID
      pecs$coalition[pecs$country_name == i &
                       pecs$election_date == j &
                       pecs$cmp %in% coalition.members] <- coalitionID 
      
      coalitionID <- coalitionID + 1 #Increment coalitionID
    }
  }
}

#Step 2a: Fix a few dates in the pecs DF --------------------------------------

#Dates are mislabeled on a few (Golder reports the French first-round date)
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1962-11-18"] <- "1962-11-25"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1967-03-05"] <- "1967-03-12"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1968-06-23"] <- "1968-06-30"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1973-03-04"] <- "1973-03-11"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1978-03-12"] <- "1978-03-19"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1981-06-14"] <- "1981-06-21"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1988-06-05"] <- "1988-06-12"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1993-03-21"] <- "1993-03-28"
pecs$election_date[pecs$country_name == "France" & pecs$election_date == "1997-05-25"] <- "1997-06-01"

#Golder miscodes these German elections
pecs$election_date[pecs$country_name == "Germany" & pecs$election_date == "1976-10-30"] <- "1976-10-03"
pecs$election_date[pecs$country_name == "Germany" & pecs$election_date == "1980-10-09"] <- "1980-10-05"

#Iceland
pecs$election_date[pecs$country_name == "Iceland" & pecs$election_date == "1959-10-25"] <- "1959-10-26"

#Italy
pecs$election_date[pecs$country_name == "Italy" & pecs$election_date == "1994-03-28"] <- "1994-03-27"


#Write to CSV
pecs %>% write_csv("pecs.csv")


#Step 3: Merge election and pecs DF ------------------------------

election %<>% left_join(pecs)

election %>% write_csv("parlgov_golder.csv")

#TROUBLESHOOT ---------------------------------
election$coalition %>% is.na %>% not %>% sum
#Only 301 of the 305 merged with the election dataset

merged.pecs <- election %>% 
  select(country_name, election_date, cmp, coalition) %>%
  na.omit

missing.pecs <- dplyr::setdiff(pecs, merged.pecs)

#NOTES on missing.pecs:
#Still missing a few French parties, but I've confirmed that it is because they don't have any records
#that year in the ParlGov dataset (did not win any seats)
#Italy
#Confirmed that DA (32529) won no seats in 1996
#Spain
#AP-PDP-PL (33438 and 33439) coded as alliances in ParlGov for 1982 and 1986

#Keep only parliamentary elections ----------------------------------

dat <- election %>% 
  filter(election_type == "parliament")

#Merge with party information ----------------------------------

dat %<>% left_join(party %>% 
                     select(party_id, family_name_short, family_name, 
                            left_right, liberty_authority))

#Add CDU/CSU Alliance ------------------------------------

#Golder codes CDU/CSU as a single party rather than a PEC, so we need to combine them here
dat$coalition[dat$cmp == 41521] <- 41521

#Compute ENPP for each election ------------------------------------------

fragmentation <- dat %>% 
  drop_na(seats) %>%
  group_by(election_id) %>%
  summarise(enpp = 1 / sum((seats / seats_total)^2))

#Clean Up -------------------------------------------

#First, merge the pre-electoral coalitions into a single row each
#Assign a left_right score weighted by the number of seats of each coalition member

coalitions <- dat %>%
  filter(!is.na(coalition)) %>%
  select(election_id, seats, coalition, left_right, family_name) %>%
  group_by(election_id, coalition) %>%
  summarise(socDem = sum(family_name == "Social democracy" | family_name == "Communist/Socialist"),
            left_right = weighted.mean(left_right, seats),
            seats = sum(seats)) %>%
  mutate(socDem = ifelse(socDem > 0, 1, 0))

#Keep the party ID of the largest member
largestMember <- dat %>% 
  filter(!is.na(coalition)) %>%
  select(election_id, seats, coalition, cmp, party_name_english) %>%
  group_by(election_id, coalition) %>%
  slice(which.max(seats)) %>%
  select(election_id, coalition, cmp, party_name_english) %>%
  set_colnames(c("election_id","coalition","largestMember","party_name_english"))

coalitions %<>% left_join(largestMember) %>% select(-coalition)

#Merge with the noncoalition parties
noncoalitions <- dat %>%
  filter(coalition %>% is.na) %>%
  mutate(socDem = ifelse(family_name %in% c("Social democracy", "Communist/Socialist"),1,0)) %>%
  select(election_id, socDem, left_right, seats, cmp, party_name_english) %>%
  set_colnames(c("election_id","socDem", "left_right", "seats","largestMember","party_name_english"))

all.PECs <- bind_rows(noncoalitions, coalitions)

#Now create a dataframe where observations are elections, with top-two PEC seat shares as variables
socDem <- all.PECs %>% 
  filter(socDem == 1) %>%
  filter(largestMember != 32720) %>%#Minor adjustment: The Pentepartido in Italy 1994 did contain a social democratic party, but they weren't the "social democratic PEC"
  group_by(election_id) %>%
  slice(which.max(seats)) %>%
  select(election_id, seats, largestMember, party_name_english, left_right) %>%
  set_colnames(c("election_id", "SocDemSeats", "SocDemPartyID", "SocDemPartyName","SocDemLeftRightScore"))

largestOther <- all.PECs %>%
  filter(socDem == 0) %>%
  group_by(election_id) %>%
  slice(which.max(seats)) %>%
  select(election_id, seats, largestMember, party_name_english, left_right) %>%
  set_colnames(c("election_id", "LargestOtherSeats", "LargestOtherPartyID", "LargestOtherPartyName","LargestOtherLeftRightScore"))


#Join and generate variables
elections <- full_join(socDem, largestOther) %>%
  filter(!is.na(SocDemSeats)) %>% 
  mutate(leftSeatMargin = SocDemSeats - LargestOtherSeats,
         leftPluralityPercentage = leftSeatMargin / (SocDemSeats + LargestOtherSeats),
         idealPointDifference = abs(SocDemLeftRightScore - LargestOtherLeftRightScore))



#Remerge with select variables from dat
elections %<>% left_join(dat %>% 
                           select(election_id, election_date, country_name, 
                                  country_name_short, seats_total) %>% 
                           unique)

#Generate election_month and election_year
elections %<>% mutate(election_month = election_date %>% as.Date %>% format("%m") %>% as.numeric,
                      election_year = election_date %>% as.Date %>% format("%Y") %>% as.numeric)

#Merge with fragmentation measure
elections %<>% left_join(fragmentation)

#Troubleshoot
troubleshoot <- function(ID) {
  dat %>% filter(election_id == ID) %>% arrange(-seats) %>% View
}
#troubleshoot(291)
#troubleshoot(163)
#troubleshoot(581)

#Merge OECD/IMF bond yield data ------------------------------------

country_codes <- dat %>% 
  select(country_name_short, country_name) %>% 
  unique

oecd <- read_csv("~/603 - Data/Bond Yields/DP_LIVE_08082018210817798.csv") %>%
  select(LOCATION, FREQUENCY, TIME, Value) %>%
  filter(FREQUENCY == "M") %>%
  mutate(year = TIME %>% substr(1,4) %>% as.numeric,
         month = TIME %>% substr(6,7) %>% as.numeric) %>%
  select(-FREQUENCY) %>%
  set_colnames(c("country_name_short", "TIME", "bond.yield.OECD", "year","month")) %>%
  left_join(country_codes)

IMF <- read_excel("~/603 - Data/IMF/Interest_Rates.xlsx", 
                  na = "...", skip = 6) 
IMF$Country %<>% recode(`Slovak Republic` = "Slovakia")
IMF %<>% filter(Country %in% country_codes$country_name) %>%
  select(-Scale, -`Base Year`) %>%
  gather(date, bond.yield.IMF, -Country) %>%
  mutate(year = date %>% substr(1,4) %>% as.numeric,
         month = date %>% substr(6,7) %>% as.numeric) %>%
  select(Country, year, month, bond.yield.IMF) %>%
  set_colnames(c("country_name", "year", "month", "bond.yield.IMF")) %>%
  left_join(country_codes) %>%
  mutate(bond.yield.IMF = bond.yield.IMF %>% as.numeric)

OECD.IMF <- full_join(oecd, IMF)

#With the exception of Iceland, everything is on the level. 
ggplot(OECD.IMF %>% filter(country_name_short != "ISL"), 
       aes(x=bond.yield.OECD, y=bond.yield.IMF, label = country_name_short)) + 
  geom_text() + geom_abline(intercept = 0, slope = 1)  

#Make a note that we're defaulting to IMF statistics, but that there is a large discrepancy in Iceland.
#The only country where there is a large discrepancy between the two datasets is Iceland. The results
#are robust to whichever dataset we use as the default. 

OECD.IMF %<>% mutate(bond.yield = if_else(bond.yield.IMF %>% is.na, bond.yield.OECD, bond.yield.IMF),
                     TIME = paste0(year, "-", month, "-01") %>% as.Date)

ggplot(OECD.IMF %>% filter(country_name_short == "IRL"), aes(x=TIME, y = bond.yield)) + geom_line()
ggplot(OECD.IMF %>% filter(country_name_short == "IRL", year > 1979, year < 1983), 
       aes(x=TIME, y = bond.yield)) + geom_line()

#Bond yield lags and leads for dynamic RD -----------------------------------------------

#Use lubridate package to merge OECD.IMF with different lags
library(lubridate)

getBondYield <- function(df, country, election_date, offset){
  dateToGet <- election_date %m+% months(offset)
  yearToGet <- dateToGet %>% substr(1,4) %>% as.numeric
  monthToGet <- dateToGet %>% substr(6,7) %>% as.numeric
  
  df %<>% filter(year == yearToGet, month == monthToGet, country_name_short == country)
  
  if(nrow(df) == 0){return(NA)}
  
  return(df$bond.yield)
}

getAverageOtherOECD <- function(df, country, election_date, offset){
  
  dateToGet <- election_date %m+% months(offset)
  yearToGet <- dateToGet %>% substr(1,4) %>% as.numeric
  monthToGet <- dateToGet %>% substr(6,7) %>% as.numeric
  
  df %<>% filter(year == yearToGet, month == monthToGet)
  
  if(nrow(df) == 0){return(NA)}
  
  oecdFounders <- c("AUT", "BEL",  "CAN", "DNK", "FRA", "DEU", "GRC", "ISL", "IRL", 
                    "ITA", "LUX", "NLD", "NOR", "PRT", "ESP", "SWE", "TUR", "GBR", "USA")
  
  #Only OECD countries, not including reference country
  df %<>% filter(country_name_short %in% oecdFounders,
                     country_name_short != country)
  
  df$bond.yield %>% na.omit %>% mean %>% return
}



for (t in -12:12){
  if (t == 0){
    bondyieldvarname <- "bond.yield.t"
    oecdvarname <- "oecd.average.t"
  } else {
    string2 <- t %>% 
      as.character %>%
      paste0("plus",.) %>%
      str_replace("-","minus") %>%
      str_replace("plusminus","minus")
    
    bondyieldvarname <- paste0("bond.yield.t",string2)
    oecdvarname <- paste0("oecd.average.t", string2)
  } 
  
  elections[,bondyieldvarname] <- NA
  elections[,oecdvarname] <- NA
  
  for (i in 1:nrow(elections)){
    elections[i,bondyieldvarname] <- getBondYield(df = OECD.IMF, 
                                            country = elections$country_name_short[i],
                                            election_date = elections$election_date[i],
                                            offset = t)
    elections[i,oecdvarname] <- getAverageOtherOECD(df = OECD.IMF,
                                              country = elections$country_name_short[i],
                                              election_date = elections$election_date[i],
                                              offset = t)
  }
  
}



#Where are we missing interest rates?
elections %>% filter(bond.yield.t %>% is.na) %>% View
#245 observations

#Some that we shouldn't be missing: Croatia, Estonia, Turkey; recent Eastern European rates.
#I checked; they're just not in the IFS. 

#Merge with cabinet data ------------------------------------------

#Generate binary variable (cabinetLeft), equal to 1 if first cabinet after election contains 
#the largest party from the left coalition; and an analogous cabinetRight variable
party <- read_delim("~/603 - Data/ParlGov/parlgov2015/party.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(party_id, cmp)

cabinet <- read_delim("~/603 - Data/ParlGov/parlgov2015/cabinet.csv", "\t") %>%
  filter(cabinet_party == 1, election_id %in% elections$election_id) %>%
  left_join(party) %>%
  select(election_id, cabinet_id, start_date, cmp) %>% 
  group_by(election_id) %>%
  filter(start_date == min(start_date)) %>% #Keep only the first cabinet for each election
  left_join(elections %>% select(election_id, SocDemPartyID, LargestOtherPartyID)) %>%
  mutate(largestLeftInCabinet = ifelse(cmp == SocDemPartyID,1,0),
         largestOtherInCabinet = ifelse(cmp == LargestOtherPartyID,1,0)) %>%
  group_by(election_id) %>%
  summarise(cabinetLeft = sum(na.omit(largestLeftInCabinet)),
            cabinetOther = sum(na.omit(largestOtherInCabinet)))

elections %<>% left_join(cabinet)


#Merge with country-year covariates ------------------------------------------------

wep <- read_csv("~/603 - Data/WEPDataverse/download.csv") %>%
  rename(country_name_short = ifs,
         election_year = year)

wep$country_name_short %<>% recode(ROM = "ROU",
                                   SUN = "BGR")

dat <- elections %>% left_join(wep, by = c("country_name_short","election_year"))


#Write to CSV -----------------------------------------------------------
dat %>% write_csv("~/602 - Research/HaysFranzese_CloseElections_IntRateCostSocDem/Analysis/IntRateCostSocDem_dataset_v2_1.csv")
dat %>% write_csv("~/602 - Research/HaysFranzese_CloseElections_IntRateCostSocDem/Shiny/IntRateCostSocDem_dataset_v2_1.csv")
