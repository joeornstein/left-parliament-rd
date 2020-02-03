#Expected shape of the curve (Hayes-Franzese-Ornstein)

library(tidyverse)
library(magrittr)

#Parameters
value_left <- 60
value_right <- 100

#Seat Share
V <- seq(-1,1,0.001)

#Standard deviation of polling
pollSD <- 0.15

#Prior probability  of a Left Govt
p <- 1 - pnorm(0, mean = V, sd = pollSD)


ExAnteValue <- p * value_left + (1-p) * value_right

ExPostValue <- ifelse(V >=0, value_left,value_right)

PriceChange <- ExPostValue - ExAnteValue


dat <- tibble(p,V,ExAnteValue,ExPostValue,PriceChange)



#Plot
ggplot(dat, aes(x=V,y=p)) + geom_line()
ggplot(dat, aes(x=V,y=ExAnteValue)) + geom_line()
ggplot(dat, aes(x=V,y=ExPostValue)) + geom_line()
ggplot(dat, aes(x=V,y=PriceChange)) + geom_line()


#Figure for paper:
ggplot() + 
  geom_line(data = dat %>% filter(V < 0), aes(x=V,y=PriceChange)) +
  geom_line(data = dat %>% filter(V > 0), aes(x=V,y=PriceChange)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw()

ggsave("Fig1_R.png")
