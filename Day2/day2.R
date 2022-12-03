# Rock Paper Scissors for the best camping spot
library(dplyr)

# coded plays
# col1 (them): A for Rock, B for Paper, and C for Scissors.
# col2 (me): X for Rock, Y for Paper, and Z for Scissors.

# Total score = my shape + outcome
# my shape (1-Rock, 2 -Paper, 3 -Scissors)
# outcome (0-lost, 3-draw, and 6-won).

# create grid of all possible plays
all.plays <- expand.grid(c("R", "P", "S"), c("R", "P", "S"))
names(all.plays) <- c("them", "me")
all.plays$shoot <- paste0(all.plays$them, all.plays$me)

# assign scores
play.scores <- all.plays %>% mutate(
  shape.score = rep(c(1:3), each=3),
  oc.score = case_when(
    shoot %in% c("SR", "RP", "PS") ~ 6,
    shoot %in% c("PR", "SP", "RS") ~ 0,
    TRUE ~ 3),
  tot.score = shape.score + oc.score
)

# convert to encrypted codes
encrypted.plays <- play.scores %>%
  mutate(shh.them = rep(c("A", "B", "C"), 3),
         shh.me = rep(c("X", "Y", "Z"), each = 3)
         )

# Example strategy guide
example.strat.guide <- data.frame(
  shh.them = c("A", "B", "C"),
  shh.me = c("Y", "X", "Z")
)
sg.test <- example.strat.guide %>% left_join(encrypted.plays)
sum(sg.test$tot.score)

# my strat guide
strat.guide <- read.delim("Day2/input.txt", header=FALSE, sep = " ")
names(strat.guide) <- c("shh.them", "shh.me")

sg <- strat.guide %>% left_join(encrypted.plays)

# tot score according to strat guide
sum(sg$tot.score)

