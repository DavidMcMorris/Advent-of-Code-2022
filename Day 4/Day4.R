#2022 Day 4

input <- gsub("-", ",", readLines("Day_4_input.txt"))
input <- read.table(text = input, sep = ",")

containment <- function(assignments) {
    if (assignments[1] <= assignments[3] && assignments[2] >= assignments[4] || assignments[1] >= assignments[3] && assignments[2] <= assignments[4]) {
        return(1)
    } else {
        return(0)
    }
}


p1_total <- sum(apply(input, 1,containment))
print(p1_total)