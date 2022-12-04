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

overlap <- function(assignments) {
    if (assignments[1] <= assignments[3] && assignments[2] >= assignments[3] || assignments[1] <= assignments[4] && assignments[2] >= assignments[4]) {
        return(1)
    } else {
        return(0)
    }
}

edge <- function(assignments) {
    if (assignments[1] == assignments[3] && assignments[2] == assignments[4] || assignments[1] == assignments[3] && assignments[2] == assignments[4]) {
        return(1)
    } else {
    return(0)
}

}
p1_total <- sum(apply(input, 1, containment))
p2_total <- sum(apply(input, 1, overlap)) + p1_total - sum(apply(input, 1, edge))

print(p1_total)
print(p2_total)