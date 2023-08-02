# 2022 Day 14

input <- readLines("Day_14_input.txt")
input <- unlist(lapply(input, strsplit, split = " -> "), recursive = FALSE)

path2vec <- function(path) {
    path <- as.numeric(unlist(strsplit(path, split = ",")))
}

pathlist <- lapply(input, path2vec)
coordmat <- NULL

for (i in seq_along(pathlist)) {
    path <- pathlist[[i]]
    len <- length(path)
    for (j in 1:(len - 2)) {
        if (j %% 2 == 1) {
            if (path[j] != path[j + 2]) {
                x <- seq(path[j], path[j + 2], sign(path[j + 2] - path[j]))
                y <- rep(path[j + 1], length(x))
                mat <- cbind(x, y)
            } else {
                y <- seq(path[j + 1], path[j + 3], sign(path[j + 3] - path[j + 1]))
                x <- rep(path[j], length(y))
                mat <- cbind(x, y)
            }
            coordmat <- rbind(coordmat, mat)
        }
    }
}

max_coord_x <- max(coordmat[, 1])

width <- max_coord_x - min(coordmat[, 1]) + 2
height <- max(coordmat[, 2]) + 2
source_pt <- c(500, 0)
coordmat[, 1] <- coordmat[, 1] - max_coord_x + width
coordmat[, 2] <- coordmat[, 2] + 1
source_pt[1] <- source_pt[1] - max_coord_x + width
source_pt[2] <- source_pt[2] + 1

cave <- matrix(0, nrow = height, ncol = width)
for (i in seq_len(nrow(coordmat))) {
    cave[coordmat[i, 2], coordmat[i, 1]] <- 1
}

buffer <- 300
cave2 <- cbind(matrix(0, nrow = nrow(cave), ncol = buffer), cave, matrix(0, nrow = nrow(cave), ncol = buffer))
cave2 <- rbind(cave2, rep(1, ncol(cave2)))
source_pt2 <- c(source_pt[1] + buffer, source_pt[2])
rocks <- sum(cave)
rocks2 <- sum(cave2)
previous_number <- -1
current_number <- 0

falling <- function(mat, r, c) {
    r <- min(which(mat[r:nrow(mat), c] == 1)) + r - 2
    if (mat[r + 1, c - 1] == 0) {
        r <- r + 1
        c <- c - 1
        falling(mat, r, c)
    } else if (mat[r + 1, c + 1] == 0) {
        r <- r + 1
        c <- c + 1
        falling(mat, r, c)
    } else {
        return(c(r, c))
    }
}

while (current_number != previous_number) {
    previous_number <- current_number
    if (cave[source_pt[2], source_pt[1]] == 1) {
        break
    } else {
        resting_coord <- falling(cave, source_pt[2], source_pt[1])
        cave[resting_coord[1], resting_coord[2]] <- 1
    }
    cave[height, ] <- rep(0, width)
    current_number <- sum(cave)
}
print(current_number - rocks)

while (cave2[source_pt2[2], source_pt2[1]] != 1) {
    resting_coord <- falling(cave2, source_pt2[2], source_pt2[1])
    cave2[resting_coord[1], resting_coord[2]] <- 1
}

print(sum(cave2) - rocks2)