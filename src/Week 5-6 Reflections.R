## Week 5 6 reflection bruh ##
Mlb1 %>% 
  ggplot(aes(x=runs, y=salary, color=runs)) + geom_point()
names(Mlb1)
library(tidyverse)
Mlb1 %>%
  ggplot(aes(x=slugavg, fill=bavg)) + geom_histogram(binwidth = 5)
str(Mlb1)

library(starwars)
starwars
starwars %>%
  ggplot(aes(x=mass, y=height, color=gender))+geom_point()
names(starwars)
starwars %>%
  ggplot(aes(x=height, y=mass, color=species )) + geom_jitter()

starWarsNew <- starwars%>%
  select(species, height,mass) %>%
  filter(species %in% c('Human', 'Rodian', 'Gungan', 'Droid','Ewok',
                        'Wookiee'))
starWarsNew %>%
  ggplot(aes(x=height, y=mass, color=species)) + geom_jitter()
