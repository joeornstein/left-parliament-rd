## Appendix Results and Robustness Tests
## Version: 1.0
## Last Updated: Jan 15, 2021


library(tidyverse)


## Load Kayser & Lindstadt loss probability measure (available here: http://mark-kayser.com/data.html)
lpr <- read_delim('data/lprdata_distrib_augmented_2015.csv', delim = ';')
