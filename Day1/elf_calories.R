# packages
library(dplyr)

# import data
raw <- read.delim("Day1/input.txt", blank.lines.skip = FALSE, header = FALSE)

raw <- rename(raw, calories = V1)

# Associate each entry with an elf ----

## each blank line indicates a new elf
next.elf <- c(1, which(is.na(raw))) # where do the next elfs start?
n.elf <- length(next.elf) # how many new elfs?
raw$elfID <- 0 # set index

# index elves
for(e in 1:(n.elf-1)){
  raw$elfID[next.elf[e]:(next.elf[e+1]-1)] <- e
}
# add the last elf
raw$elfID[next.elf[e+1]:NROW(raw)] <- n.elf

# now drop empty rows
elves <- na.omit(raw)

# Which elf has the most calories? ----
elves %>% group_by(elfID) %>%
  summarize(tot.cals = sum(calories)) %>%
  arrange(desc(tot.cals)) %>% slice(1)

# Calories carried by the top three elves ----
top3 <- elves %>% group_by(elfID) %>%
  summarize(tot.cals = sum(calories)) %>%
  arrange(desc(tot.cals)) %>% slice(1:3)
sum(top3$tot.cals)
