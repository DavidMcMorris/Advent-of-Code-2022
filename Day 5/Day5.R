#2022 Day 5

initial_stacks <- read.fwf("Day_5_input.txt", widths = rep(4, 9), n = 8)
moves <- read.table("Day_5_input.txt", sep = " ", skip = 10)[, c(2, 4, 6)]

crates <- NULL

for (i in 1:9) {
    x <- gsub(" ", "", initial_stacks[, i])
    x <- x[which(x != "")]
    x <- gsub("[", "", x, fixed = TRUE)
    x <- gsub("]", "", x, fixed = TRUE)
    crates[[i]] <- x
}

for (i in seq_len(nrow(moves))) {
    from <- moves[i, 2]
    to <- moves[i, 3]
    num <- moves[i, 1]
    transfer <- rev(crates[[from]][1:num])  #Delete the "rev" for part 2
    crates[[to]] <- c(transfer, crates[[to]])
    crates[[from]] <- crates[[from]][(num + 1):length(crates[[from]])]
}

tops <- NULL
for (i in seq_len(length(crates))) {
    tops <- c(tops, crates[[i]][1])
}
tops <- paste(tops, collapse = "")
print(tops)