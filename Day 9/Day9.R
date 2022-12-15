# 2022 Day 9

input <- read.table("Day_9_input.txt", colClasses = c("character", "numeric"), col.names = c("Direction", "Moves"))

part <- 1

t_coord1 <- c(0, 0)
t_coord2 <- c(0, 0)
t_coord3 <- c(0, 0)
t_coord4 <- c(0, 0)
t_coord5 <- c(0, 0)
t_coord6 <- c(0, 0)
t_coord7 <- c(0, 0)
t_coord8 <- c(0, 0)
t_coord9 <- c(0, 0)

h_coord <- c(0, 0)

t_path <- t_coord1
h_path <- h_coord


t_updater <- function(tc, hc) {
    distance <- as.numeric(dist(rbind(tc, hc)))
    if (distance > sqrt(2)) {
        if (tc[1] == hc[1]) {
            if (hc[2] > tc[2]) {
               tc[2] <- tc[2] + 1
            } else {
               tc[2] <- tc[2] - 1
            }
        } else if (tc[2] == hc[2]) {
            if (hc[1] > tc[1]) {
               tc[1] <- tc[1] + 1
            } else {
               tc[1] <- tc[1] - 1
            }
        } else if (hc[1] > tc[1] && hc[2] > tc[2]) {
            tc <- tc + 1
        } else if (hc[1] > tc[1] && hc[2] < tc[2]) {
            tc[1] <- tc[1] + 1
            tc[2] <- tc[2] - 1
        } else if (hc[1] < tc[1] && hc[2] < tc[2]) {
            tc <- tc - 1
        } else {
            tc[1] <- tc[1] - 1
            tc[2] <- tc[2] + 1
        }
    }
    return(tc)
}

for (i in seq_len(dim(input)[1])) {
    if (input[i, 1] == "U") {
        for (j in seq_len(input[i, 2])) {
            h_coord[2] <- h_coord[2] + 1
            if (part == 1) {
               t_coord1 <- t_updater(t_coord1, h_coord)
               t_path <- rbind(t_path, t_coord1)
            } else {
            t_coord1 <- t_updater(t_coord1, h_coord)
            t_coord2 <- t_updater(t_coord2, t_coord1)
            t_coord3 <- t_updater(t_coord3, t_coord2)
            t_coord4 <- t_updater(t_coord4, t_coord3)
            t_coord5 <- t_updater(t_coord5, t_coord4)
            t_coord6 <- t_updater(t_coord6, t_coord5)
            t_coord7 <- t_updater(t_coord7, t_coord6)
            t_coord8 <- t_updater(t_coord8, t_coord7)
            t_coord9 <- t_updater(t_coord9, t_coord8)
            t_path <- rbind(t_path, t_coord9)
            }
        }
   } else if (input[i, 1] == "D") {
        for (j in seq_len(input[i, 2])) {
            h_coord[2] <- h_coord[2] - 1
            if (part == 1) {
               t_coord1 <- t_updater(t_coord1, h_coord)
               t_path <- rbind(t_path, t_coord1)
            } else {
            t_coord1 <- t_updater(t_coord1, h_coord)
            t_coord2 <- t_updater(t_coord2, t_coord1)
            t_coord3 <- t_updater(t_coord3, t_coord2)
            t_coord4 <- t_updater(t_coord4, t_coord3)
            t_coord5 <- t_updater(t_coord5, t_coord4)
            t_coord6 <- t_updater(t_coord6, t_coord5)
            t_coord7 <- t_updater(t_coord7, t_coord6)
            t_coord8 <- t_updater(t_coord8, t_coord7)
            t_coord9 <- t_updater(t_coord9, t_coord8)
            t_path <- rbind(t_path, t_coord9)
            }
        }
    } else if (input[i, 1] == "L") {
        for (j in seq_len(input[i, 2])) {
            h_coord[1] <- h_coord[1] - 1
            if (part == 1) {
               t_coord1 <- t_updater(t_coord1, h_coord)
               t_path <- rbind(t_path, t_coord1)
            } else {
            t_coord1 <- t_updater(t_coord1, h_coord)
            t_coord2 <- t_updater(t_coord2, t_coord1)
            t_coord3 <- t_updater(t_coord3, t_coord2)
            t_coord4 <- t_updater(t_coord4, t_coord3)
            t_coord5 <- t_updater(t_coord5, t_coord4)
            t_coord6 <- t_updater(t_coord6, t_coord5)
            t_coord7 <- t_updater(t_coord7, t_coord6)
            t_coord8 <- t_updater(t_coord8, t_coord7)
            t_coord9 <- t_updater(t_coord9, t_coord8)
            t_path <- rbind(t_path, t_coord9)
            }
        }
    } else {
        for (j in seq_len(input[i, 2])) {
            h_coord[1] <- h_coord[1] + 1
            if (part == 1) {
               t_coord1 <- t_updater(t_coord1, h_coord)
               t_path <- rbind(t_path, t_coord1)
            } else {
            t_coord1 <- t_updater(t_coord1, h_coord)
            t_coord2 <- t_updater(t_coord2, t_coord1)
            t_coord3 <- t_updater(t_coord3, t_coord2)
            t_coord4 <- t_updater(t_coord4, t_coord3)
            t_coord5 <- t_updater(t_coord5, t_coord4)
            t_coord6 <- t_updater(t_coord6, t_coord5)
            t_coord7 <- t_updater(t_coord7, t_coord6)
            t_coord8 <- t_updater(t_coord8, t_coord7)
            t_coord9 <- t_updater(t_coord9, t_coord8)
            t_path <- rbind(t_path, t_coord9)
            }
        }
    }
}

print(dim(unique(t_path))[1])