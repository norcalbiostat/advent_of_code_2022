# Every section has a unique ID number
# Each Elf is assigned a range of section IDs.
# there are overlaps
# In how many assignment pairs does one range fully contain the other?

library(stringr) # coulda done it with substr if I wanted base only

## Example list
ex <- c(
'2-4,6-8',
'2-3,4-5',
'5-7,7-9',
'2-8,3-7',
'6-6,4-6',
'2-6,4-8',
"8-50,49-82")

a <- str_split(ex, ",|-" , simplify = TRUE)

# real list
inlist <- readLines("Day4/input.txt")
a <- str_split(inlist, ",|-" , simplify = TRUE)

# convert to numbered list
lst <- matrix(as.numeric(a), nrow=nrow(a), byrow=FALSE)

# check for overlap
olap <- (lst[,1] <= lst[,3] & lst[,2] >= lst[,4]) | # 1 fully contains 2
        (lst[,3] <= lst[,1] & lst[,4] >= lst[,2]) | # 2 fully contains 1
        (lst[,1] == lst[,3] & lst[,2] == lst[,4]) # same
sum(olap)

cbind(lst, len)
# 359 (no)
# 305 - yes

# --- part 2 ---
# number of pairs that overlap at all.
any.olap <- rep(NA, NROW(lst))

for (i in 1:NROW(lst)){
  any.olap[i] <- length(intersect(lst[i,1]:lst[i,2], lst[i,3]:lst[i,4])) > 0
}
sum(any.olap)

