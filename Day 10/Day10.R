# 2022 Day 10

input <- read.table("Day_10_input.txt", sep = "", fill = TRUE, colClasses = c("character", "numeric"), row.names = NULL, col.names = c("V1", "V2"))

tick <- 1
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