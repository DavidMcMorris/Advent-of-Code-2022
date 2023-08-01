# 2022 Day 14

input <- readLines("Day_14_test_input.txt")
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
cave[source_pt[2], source_pt[1]] <- 9
for (i in 1:nrow(coordmat)) {
    cave[coordmat[i, 2], coordmat[i, 1]] <- 1
}
