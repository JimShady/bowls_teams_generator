rm(list=ls())

library(tidyverse)
library(compare)

men   <- paste0('m', 1:12)
women <- paste0('f', 1:12)

men_satisfied <- 'N'
women_satisfied <- 'N'

iteration <- 1

while (men_satisfied == 'N' | women_satisfied == 'N') {

partners_draw <- tibble(game = c(rep(1, 12),rep(2,12), rep(3,12)),
                  man = rep(men,3),
                  woman = c(sample(women), sample(women), sample(women))) %>%
                  arrange(game, man, woman)


while (nrow(distinct_all(partners_draw)) != 36) {
  partners_draw <- tibble(game = c(rep(1, 12),rep(2,12), rep(3,12)),
                          man = rep(men,3),
                          woman = c(sample(women), sample(women), sample(women))) %>%
                          arrange(game, man, woman)
}

### Opposition one

half_one <- partners_draw %>% filter(game ==1) %>% 
            .[sample(nrow(.),6),]

half_two <- partners_draw %>% filter(game ==1) %>% 
            setdiff(half_one)

names(half_one) <- c('game1', 'man1', 'woman1')
names(half_two) <- c('game2', 'man2', 'woman2')
            
game_one <- cbind(half_one, half_two) %>%
              select(man1, woman1, man2, woman2) %>%
              mutate(game = 1) %>%
              select(game, man1, woman1, man2, woman2)

### Opposition two

half_one <- partners_draw %>% filter(game ==2) %>% 
  .[sample(nrow(.),6),]

half_two <- partners_draw %>% filter(game ==2) %>% 
  setdiff(half_one)

names(half_one) <- c('game1', 'man1', 'woman1')
names(half_two) <- c('game2', 'man2', 'woman2')

game_two <- cbind(half_one, half_two) %>%
  select(man1, woman1, man2, woman2) %>%
  mutate(game = 2) %>%
  select(game, man1, woman1, man2, woman2)

### Opposition three

half_one <- partners_draw %>% filter(game ==3) %>% 
  .[sample(nrow(.),6),]

half_two <- partners_draw %>% filter(game ==3) %>% 
  setdiff(half_one)

names(half_one) <- c('game1', 'man1', 'woman1')
names(half_two) <- c('game2', 'man2', 'woman2')

game_three <- cbind(half_one, half_two) %>%
  select(man1, woman1, man2, woman2) %>%
  mutate(game = 3) %>%
  select(game, man1, woman1, man2, woman2)

## Joined all together
final_draw <- rbind(game_one, game_two, game_three)

# Check conditions are met for men

men_counts <- list()

for (i in 1:length(men)) {
  person_to_check <- men[i]

player <- final_draw %>%
            filter(man1 == person_to_check | man2 == person_to_check) %>%
            select(-game)

players_interactions <- c(as.character(player[1,]),
                          as.character(player[2,]),
                          as.character(player[3,]))

# remove the player
players_interactions <- players_interactions[players_interactions != person_to_check]

men_counts[[i]] <- length(unique(players_interactions))
}

if (sum(unlist(men_counts)) == 96){
  men_satisfied <- 'Y'
} else {
  men_satisfied <- 'N'
}

# Check conditions are met for the women

women_counts <- list()

for (i in 1:length(women)) {
  person_to_check <- women[i]
  
  player <- final_draw %>%
    filter(woman1 == person_to_check | woman2 == person_to_check) %>%
    select(-game)
  
  players_interactions <- c(as.character(player[1,]),
                            as.character(player[2,]),
                            as.character(player[3,]))
  
  # remove the player
  players_interactions <- players_interactions[players_interactions != person_to_check]
  
  women_counts[[i]] <- length(unique(players_interactions))
}

if (sum(unlist(women_counts)) == 96){
  women_satisfied <- 'Y'
} else {
  women_satisfied <- 'N'
}

print(iteration)

iteration <- iteration + 1

}