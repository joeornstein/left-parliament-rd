#Expected shape of the curve (Hayes-Franzese-Ornstein)

library(tidyverse)
library(magrittr)

#Parameters
value_left <- 50
value_right <- 100

#Seat Share
vSquiggle <- seq(-25,25,0.05)

#Probability of a Left Govt
p <- 1 - pnorm(0, mean = vSquiggle, sd = 5)


ExAnteValue <- p * value_left + (1-p) * value_right

ExPostValue <- ifelse(vSquiggle >=0, value_left,value_right)

PriceChange <- ExPostValue - ExAnteValue


dat <- tibble(p,vSquiggle,ExAnteValue,ExPostValue,PriceChange)



#Plot
ggplot(dat, aes(x=vSquiggle,y=p)) + geom_line()
ggplot(dat, aes(x=vSquiggle,y=ExAnteValue)) + geom_line()
ggplot(dat, aes(x=vSquiggle,y=ExPostValue)) + geom_line()
ggplot(dat, aes(x=vSquiggle,y=PriceChange)) + geom_line()

#A zig zag!

#But it should curve. The tails are more certain....


#So...what's the right signal?