# 2022 Day 10

input <- read.table("Day_10_input.txt", sep = "", fill = TRUE, colClasses = c("character", "numeric"), row.names = NULL, col.names = c("V1", "V2"))

tick <- 0
value <- 1
strength <- NULL

for (i in seq_len(dim(input)[1])) {
    if (input[i, 1] == "noop") {
        tick <- tick + 1
        if (tick %% 40 == 20) {
            strength <- c(strength, value * tick)
        }
    } else {
        tick <- tick + 1
        if (tick %% 40 == 20) {
            strength <- c(strength, value * tick)
        }
        value <- value + input[i, 2]
        tick <- tick + 1
        if (tick %% 40 == 20) {
            strength <- c(strength, value * tick)
        }
    }
}

print(sum(strength))

tick <- 0
value <- 1
pixels <- NULL

drawer <- function(tick, value) {
    if (is.element(tick %% 40, c(value - 1, value, value + 1))) {
        draw <- "#"
    } else {
        draw <- "."
    }
    return(draw)
}

for (i in seq_len(dim(input)[1])) {
    if (input[i, 1] == "noop") {
        draw <- drawer(tick, value)
        pixels <- c(pixels, draw)
        tick <- tick + 1
    } else {
        draw <- drawer(tick, value)
        pixels <- c(pixels, draw)
        tick <- tick + 1
        draw <- drawer(tick, value)
        pixels <- c(pixels, draw)
        tick <- tick + 1
        value <- value + input[i, 2]
    }
}

graphic <- data.frame(matrix(pixels, ncol = 40, byrow = TRUE))
words <- which(x == "#", arr.ind = TRUE)
plot(words[, 2], -words[, 1], ylim = c(-40, 30), pch = 15)
