#2022 Day 2

input <- read.table("Day_2_input.txt")

play_score <- function(letter) {
    letter <- as.character(letter)
    xyz <- c("X", "Y", "Z")
    scores <- c(1, 2, 3)
    score <- scores[which(letter == xyz)]
    return(score)
}

p1_outcome <- function(pair) {
    pair <- as.character(pair)
    score_matrix <- rbind(c(3, 6, 0), c(0, 3, 6), c(6, 0, 3))
    xyz <- c("X", "Y", "Z")
    abc <- c("A", "B", "C")
    score <- score_matrix[which(pair[1] == abc), which(pair[2] == xyz)]
    return(score)
}

p1_total <- 0
for (i in seq_len(nrow(input))) {
    p1_total <- total + play_score(input[i, 2]) + p1_outcome(input[i, ])
}

strategy_score <- function(letter) {
    letter <- as.character(letter)
    xyz <- c("X", "Y", "Z")
    scores <- c(0, 3, 6)
    score <- scores[which(letter == xyz)]
    return(score)
}

p2_outcome <- function(pair) {
    pair <- as.character(pair)
    score_matrix <- rbind(c(3, 6, 0), c(0, 3, 6), c(6, 0, 3))
    xyz <- c("X", "Y", "Z")
    abc <- c("A", "B", "C")
    strat_score <- strategy_score(pair[2])
    play <- xyz[which(score_matrix[which(pair[1] == abc), ] == strat_score)]
    score <- play_score(play) + strat_score
    return(score)
}

p2_total <- 0
for (i in seq_len(nrow(input))) {
    p2_total <- p2_total + p2_outcome(input[i, ])
}

print(p1_total)
print(p2_total)