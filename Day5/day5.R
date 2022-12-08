# Restack crates in order of operations

# Example Crate starting position
#     [D]
# [N] [C]
# [Z] [M] [P]
#  1   2   3

# create stacks as lists
stack1 <- c("N", "Z")
stack2 <- c("D", "C", "M")
stack3 <- c("P")

# example instructions
instructions <- c(
  'move 1 from 2 to 1',
  'move 3 from 1 to 3',
  'move 2 from 2 to 1',
  'move 1 from 1 to 2'
)

# My starting crate position
# [D]                     [N] [F]
# [H] [F]             [L] [J] [H]
# [R] [H]             [F] [V] [G] [H]
# [Z] [Q]         [Z] [W] [L] [J] [B]
# [S] [W] [H]     [B] [H] [D] [C] [M]
# [P] [R] [S] [G] [J] [J] [W] [Z] [V]
# [W] [B] [V] [F] [G] [T] [T] [T] [P]
# [Q] [V] [C] [H] [P] [Q] [Z] [D] [W]
# 1   2   3   4   5   6   7   8   9

# yes, this is manual nonsense. idk how to read columns so f* it
# i know i'm gonna make a mistake here....
stack1 <- c("D", "H", "R", "Z", "S", "P", "W", "Q")
stack2 <- c("F", "H", "Q", "W", "R", "B", "V")
stack3 <- c("H", "S", "V", "C")
stack4 <- c("G", "F", "H")
stack5 <- c("Z", "B", "J", "G", "P")
stack6 <- c("L", "F", "W", "H", "J", 'T", "Q')
stack7 <- c("N", "J", "V", "L", "D", "W", "T", "Z")
stack8 <- c("F", "H", "G", "J", "C", "Z", "T", "D")
stack9 <- c("H", "B", "M", 'V", "P", "W')

instructions <- read.delim("Day5/input.txt", skip = 10, header = FALSE)

# extract numbers from instructions
inst.num <- str_extract_all(instructions$V1, "\\d+", simplify = TRUE)
nbox <- inst.num[,1] %>% as.numeric()
from <- inst.num[,2] %>% as.numeric()
to   <- inst.num[,3] %>% as.numeric()

for(s in 1:length(instructions)){
  # identify the stack to pick up from
  get.from <- paste0("stack", from[s])
  stack.from <- get(get.from)
  # pick nboxes off the top
  lift.boxes <- stack.from[1:nbox[s]]
  remaining.boxes <- stack.from[-(1:nbox[s])]
  assign(get.from, remaining.boxes)

  # identify the stack to put the lifted boxes down onto
  put.down.on <- paste0("stack", to[s])
  # put on top of of the `to` stack
  new.stack <- c(rev(lift.boxes), get(put.down.on))
  assign(put.down.on, new.stack)
}

# top boxes
paste0(stack1[1], stack2[1], stack3[1],
       stack1[4], stack2[5], stack3[6],
       stack1[7], stack2[8], stack3[9], sep="")

# example CMZ
# DFSZRNAWNANA (nope)
