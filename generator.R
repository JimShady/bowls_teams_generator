rm(list=ls())

library(tidyverse)

men   <- c(paste0('M', 1:12))

women <- c(paste0('F', 1:12))

men_history <- tibble(men = men,
                      partner1 = NA,
                      partner2 = NA,
                      partner3 = NA,
                      opposition1 = NA,
                      opposition1a = NA,
                      opposition2 = NA,
                      opposition2a = NA,
                      opposition3 = NA,
                      opposition3a = NA)

women_history <- tibble(women = women,
                      partner1 = NA,
                      partner2 = NA,
                      partner3 = NA,
                      opposition1 = NA,
                      opposition1a = NA,
                      opposition2 = NA,
                      opposition2a = NA,
                      opposition3 = NA,
                      opposition3a = NA)

# session one
while (length(men) > 0 & length(women) > 0) {
  
  man1   <- sample(men, 1)
  woman1 <- sample(women, 1)
  
  men   <- men[men != man1]
  women <- women[women != woman1]
  
  man2   <- sample(men, 1)
  woman2 <- sample(women, 1)
  
  men   <- men[men != man2]
  women <- women[women != woman2]
  
  # record partners
  men_history[men_history$men == man1,'partner1'] <- man2
  men_history[men_history$men == man2,'partner1'] <- man1
  
  women_history[women_history$women == woman1,'partner1'] <- woman2
  women_history[women_history$women == woman2,'partner1'] <- woman1
  
  # record oppositions
  men_history[men_history$men == man1,'opposition1']  <- woman1
  men_history[men_history$men == man1,'opposition1a'] <- woman2
  
  men_history[men_history$men == man2,'opposition1']  <- woman1
  men_history[men_history$men == man2,'opposition1a'] <- woman2
  
  women_history[women_history$women == woman1,'opposition1'] <- man1
  women_history[women_history$women == woman1,'opposition1a'] <- man2
  
  women_history[women_history$women == woman2,'opposition1'] <- man1
  women_history[women_history$women == woman2,'opposition1a'] <- man2
  
  print(paste(man1, '&', man2))
  print(paste(woman1, '&', woman2))
  
  rm(man1, man2, woman1, woman2)
  
}

# All back into the hat
men   <- c(paste0('M', 1:12))
women <- c(paste0('F', 1:12))

# session two
while (length(men) > 0 & length(women) > 0) {
  man1   <- sample(men, 1)
  woman1 <- sample(women, 1)
  
  men   <- men[men != man1]
  women <- women[women != woman1]
  
  # Here. When sample, exclude the previous partner
  man2   <- sample(men[men != men_history[men_history$men == man1,]$partner1], 1)
  woman2 <- sample(women[women != women_history[women_history$women == woman1,]$partner1], 1)
  
  men   <- men[men != man2]
  women <- women[women != woman2]
  
  # record partners
  men_history[men_history$men == man1,'partner2'] <- man2
  men_history[men_history$men == man2,'partner2'] <- man1
  
  women_history[women_history$women == woman1,'partner2'] <- woman2
  women_history[women_history$women == woman2,'partner2'] <- woman1
  
  # record oppositions
  men_history[men_history$men == man1,'opposition2']  <- woman1
  men_history[men_history$men == man1,'opposition2a'] <- woman2
  
  men_history[men_history$men == man2,'opposition2']  <- woman1
  men_history[men_history$men == man2,'opposition2a'] <- woman2
  
  women_history[women_history$women == woman1,'opposition2'] <- man1
  women_history[women_history$women == woman1,'opposition2a'] <- man2
  
  women_history[women_history$women == woman2,'opposition2'] <- man1
  women_history[women_history$women == woman2,'opposition2a'] <- man2
  
  print(paste(man1, '&', man2))
  print(paste(woman1, '&', woman2))
  
  rm(man1, man2, woman1, woman2)
  
}

# All back into the hat
men   <- c(paste0('M', 1:12))
women <- c(paste0('F', 1:12))

# session three
while (length(men) > 0 & length(women) > 0) {
  man1   <- sample(men, 1)
  woman1 <- sample(women, 1)
  
  men   <- men[men != man1]
  women <- women[women != woman1]
  
  # Here. When sample, exclude the previous two partners
  man2   <- sample(men[men != men_history[men_history$men == man1,]$partner1 & 
                         men != men_history[men_history$men == man1,]$partner2], 
                   1)
  woman2 <- sample(women[women != women_history[women_history$women == woman1,]$partner1 & 
                           women != women_history[women_history$women == woman1,]$partner2], 
                   1)
  
  men   <- men[men != man2]
  women <- women[women != woman2]
  
  # record partners
  men_history[men_history$men == man1,'partner3'] <- man2
  men_history[men_history$men == man2,'partner3'] <- man1
  
  women_history[women_history$women == woman1,'partner3'] <- woman2
  women_history[women_history$women == woman2,'partner3'] <- woman1
  
  # record oppositions
  men_history[men_history$men == man1,'opposition3']  <- woman1
  men_history[men_history$men == man1,'opposition3a'] <- woman2
  
  men_history[men_history$men == man2,'opposition3']  <- woman1
  men_history[men_history$men == man2,'opposition3a'] <- woman2
  
  women_history[women_history$women == woman1,'opposition3'] <- man1
  women_history[women_history$women == woman1,'opposition3a'] <- man2
  
  women_history[women_history$women == woman2,'opposition3'] <- man1
  women_history[women_history$women == woman2,'opposition3a'] <- man2
  
  print(paste(man1, '&', man2))
  print(paste(woman1, '&', woman2))
  
  rm(man1, man2, woman1, woman2)
  
}

## Now figure out who plays who
## Start with the table. First man. Draw against random woman, and so on.
#Roudn two, similar, but check didn't play them before.
