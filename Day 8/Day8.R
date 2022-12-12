# 2022 Day 8

input <- read.table("Day_8_input.txt", colClasses = "character")
height <- nrow(input)
input <- as.numeric(unlist(strsplit(as.matrix(input), split = "")))
input <- matrix(nrow = height, input, byrow = TRUE)
width <- ncol(input)

edges <- 2 * (height + width) - 4

i <- 2  #row
j <- 2  #column
counter <- 0
flag <- "F"

# Part 1
while (flag == "F") {
    l <- sum(input[i, j] > input[i, 1:(j - 1)]) == length(1:(j - 1))
    r <- sum(input[i, j] > input[i, (j + 1):width]) == length((j + 1):width)
    u <- sum(input[i, j] > input[1:(i - 1), j]) == length(1:(i - 1))
    d <- sum(input[i, j] > input[(i + 1):height, j]) == length((i + 1):height)
    if (l + r + u + d > 0) {
        counter <- counter + 1
    }
    i <- i + 1
    if (i == width) {
        i <- 2
        j <- j + 1
        if (j == height) {
            flag <- "T"
        }
    }
}

print(counter + edges)