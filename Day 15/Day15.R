# 2022 Day 15

input <- readLines("Day_15_input.txt")
lvl <- 2000000
max <- 4000000


input <- unlist(strsplit(input, split = ":"))
input <- unlist(strsplit(input, split = ","))
input <- as.numeric(gsub("[^0-9-]", "", input))

input <- matrix(input, ncol = 4, byrow = TRUE)

input <- cbind(input, rowSums(abs(input[, 1:2] - input[, 3:4])))

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

# Part 2

tc_dist <- function(a, b) {
    distance <- sum(abs(a - b))
    return(distance)
}

just_outside <- function(sensor, max) {
    d <- sensor[5] + 1
    x <- seq(sensor[1] - d, sensor[1] + d, 1)
    upper_y <- d - abs(x - sensor[1]) + sensor[2]
    lower_y <- -upper_y + 2 * sensor[2]
    locations <- unique(cbind(c(x, x), c(upper_y, lower_y)))
    validity <- locations <= max & locations >= 0
    validity <- which(rowSums(validity) == 2)
    locations <- locations[validity, ]
    return(locations)
}

for (k in seq_len(nrow(input))) {
    search_space <- just_outside(input[k, ], max)
    search_space <- unique(search_space)
    for (i in seq_len(nrow(search_space))) {
        vis <- FALSE
        for (j in seq_len(nrow(input))) {
            if (vis == FALSE) {
                sensor <- c(input[j, 1], input[j, 2])
                beacon_dist <- input[j, 5]
                vis <- tc_dist(sensor, search_space[i, ]) <= beacon_dist
            }
        }
        if (vis == FALSE) {
            print(search_space[i, ])
            break
        }
    }
    if (vis == FALSE) {
    break
    }
}

options(digits = 20)
print(max * search_space[i, 1] + search_space[i, 2])