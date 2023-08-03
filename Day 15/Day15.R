# 2022 Day 15

input <- readLines("Day_15_input.txt")
lvl <- 2000000
max <- 4000000


input <- unlist(strsplit(input, split = ":"))
input <- unlist(strsplit(input, split = ","))
input <- as.numeric(gsub("[^0-9-]", "", input))

input <- matrix(input, ncol = 4, byrow = TRUE)

input <- cbind(input, rowSums(abs(input[, 1:2] - input[, 3:4])))

tc_dist <- function(a, b) {
    distance <- sum(abs(a - b))
    return(distance)
}

check_lvl <- function(sensor, level) {
    dist2lvl <- abs(sensor[2] - level)
    if (dist2lvl > sensor[5]) {
        return(NA)
    } else {
        extra_dist <- sensor[5] - dist2lvl
        visible <- seq(sensor[1] - extra_dist, sensor[1] + extra_dist, 1)
    }
    return(visible)
}

seen_at_lvl <- unlist(apply(input, 1, check_lvl, level = lvl))
seen_at_lvl <- length(unique(seen_at_lvl[which(!is.na(seen_at_lvl))]))
beacons_at_lvl <- sum(unique(input[, 3:4])[, 2] == lvl)

print(seen_at_lvl - beacons_at_lvl)
counter <- 0
total <- max * max

for (i in 0:max) {
    for (j in 0:max) {
        vis <- FALSE
        for (k in seq_len(nrow(input))){
            if (vis == FALSE) {
                sensor <- c(input[k, 1], input[k, 2])
                beacon_dist <- input[k, 5]
                vis <- tc_dist(sensor, c(i, j)) <= beacon_dist
            }
        }
        if (vis == FALSE) {
            print(c(i, j))
        }
    }
}