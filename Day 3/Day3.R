#2022 Day 3

input <- read.table("Day_3_input.txt")
input <- lapply(input, strsplit, "")[[1]]

all_letters <- c(letters, LETTERS)

p1_scorer <- function(rucksack) {
    n <- length(rucksack)
    split_ind <- floor((n + 1) / 2)
    first_compartment <- rucksack[1:split_ind]
    second_compartment <- rucksack[(split_ind + 1):n]
    overlap <- intersect(first_compartment, second_compartment)
    score <- which(all_letters == overlap)
    return(score)
}

p1_total_priority <- sum(sapply(input, p1_scorer))

p2_total_priority <-  0
for (i in seq_len(length(input))) {
    if (i %% 3 == 1) {
        first <- input[[i]]
        second <- input[[i + 1]]
        third <- input[[i + 2]]
        overlap <- intersect(intersect(first, second), third)
        score <- which(all_letters == overlap)
        p2_total_priority <- p2_total_priority + score
    }
}

print(p1_total_priority)
print(p2_total_priority)
