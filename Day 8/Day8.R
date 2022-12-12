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
max_scenic_score <- 0
flag <- "F"

while (flag == "F") {
    l <- input[i, j] > rev(input[i, 1:(j - 1)])
    r <- input[i, j] > input[i, (j + 1):width]
    u <- input[i, j] > rev(input[1:(i - 1), j])
    d <- input[i, j] > input[(i + 1):height, j]

    # Part 1
    l_vis <- sum(l) == length(1:(j - 1))
    r_vis <- sum(r) == length((j + 1):width)
    u_vis <- sum(u) == length(1:(i - 1))
    d_vis <- sum(d) == length((i + 1):height)
    if (l_vis + r_vis + u_vis + d_vis > 0) {
        counter <- counter + 1
    }

    # Part 2
    l_score <- min(length(l), which(l == FALSE))
    r_score <- min(length(r), which(r == FALSE))
    u_score <- min(length(u), which(u == FALSE))
    d_score <- min(length(d), which(d == FALSE))
    scenic_score <- l_score * r_score * u_score * d_score
    max_scenic_score <- max(max_scenic_score, scenic_score)

    # Move to next tree
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
print(max_scenic_score)