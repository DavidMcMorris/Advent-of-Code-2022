# 2022 Day 10

input <- read.table("test.txt", sep = "", fill = TRUE, colClasses = c("character", "numeric"), row.names = NULL, col.names = c("V1", "V2"))
# input[(dim(input)[1] + 1):(dim(input)[1] + 3), ] <- list("noop", NA)

tick <- 0
value <- 1
buffer_1 <- c(0, 0)
buffer_2 <- c(0, 0)
buffer_3 <- c(0, 0)
strength <- NULL

for (i in seq_len(dim(input)[1])) {
    if (tick == buffer_1[1]) {
        value <- value + buffer_1[2]
        buffer_1 <- buffer_2
        buffer_2 <- buffer_3
        buffer_3 <- c(0, 0)
    }
    print(c(tick, value))
    tick <- tick + 1
    if (input[i,1] == "addx") {
        if (setequal(buffer_1, c(0, 0))) {
            buffer_1 <- c(tick + 2, input[i, 2])
        } else if (setequal(buffer_2, c(0, 0))) {
            buffer_2 <- c(tick + 2, input[i, 2])
        } else {
            buffer_3 <- c(tick + 2, input[i, 2])
        }
    }
    if (tick %% 40 == 20) {
        strength <- c(strength, value * tick)
    }
}

print(sum(strength))