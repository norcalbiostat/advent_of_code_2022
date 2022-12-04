# Day 3: Rucksack Reorganization
library(dplyr)
library(stringr)
library(tidyr)

# Example content
ex <- c(
"vJrwpWtwJgWrhcsFMMfFFhFp",
"jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
"PmmdzqPrVvPwwTWBwg",
"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
"ttgJtRGJQctTZtZT",
"CrZsJsPPZsGzwwsLwLmpwMDw")

# test string for multiple dups "newteststringabcdefghijk"

bag <- ex # test bag
input <- read.delim('Day3/input.txt',header=FALSE) # real bags
bag <- input$V1

# splitting into compartments
n.items <- str_length(bag)
rucksack <- data.frame(
  all.things = bag,
  compart1 = substr(bag, 1, n.items/2),
  compart2 = substr(bag, n.items/2+1, n.items),
  dups = ".",
  priority = 0
)

# setup list of things to check. Moved this out of for loop b/c need
# to use the position as a priority indicator
check.list <- c(letters, LETTERS)

#iterate over position, where position = priority
for(p in 1:length(check.list)){
  item <- check.list[p]
  # is the letter in both compartments?
  has.dups <- grepl(item, rucksack$compart1) & grepl(item, rucksack$compart2)
  # if so, add it to the duplicate string list
  rucksack$dups[has.dups] <- paste0(rucksack$dups[has.dups], item)
  # add priority value per duplicate letter
  rucksack$priority[has.dups] <- rucksack$priority[has.dups] + p
}

# sum of priorities
sum(rucksack$priority)
# test bag 157 - checks out!
# Right on part 1 on the first try !!!!!

# ----part 2----
# same idea, but need to find common dups within bags in groups of 3
n.groups <- NROW(rucksack)/3
rucksack$group <- rep(1:n.groups, each = 3)
rucksack$elf <- paste0("elf", 1:3)

# put each elf's bag in a separate column
ruck.group <- rucksack %>%
  pivot_wider(id_cols = group,
              names_from = elf,
              values_from = all.things) %>%
  mutate(priority = 0)

# check for dups now in 3 cols
for(p in 1:length(check.list)){
  item <- check.list[p]
  # is the letter in both compartments?
  has.dups <- grepl(item, ruck.group$elf1) &
              grepl(item, ruck.group$elf2) &
              grepl(item, ruck.group$elf3)
  # add priority value per duplicate letter
  ruck.group$priority[has.dups] <- ruck.group$priority[has.dups] + p
}

sum(ruck.group$priority)
