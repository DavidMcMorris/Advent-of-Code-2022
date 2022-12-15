# 2022 Day 9

input <- read.table("Day_9_input.txt", colClasses = c("character", "numeric"), col.names = c("Direction", "Moves"))

t_coord <- c(0, 0)
h_coord <- c(0, 0)

t_path <- t_coord
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
            t_coord <- t_updater(t_coord, h_coord)
            h_path <- rbind(h_path, h_coord)
            t_path <- rbind(t_path, t_coord)
        }
   } else if (input[i, 1] == "D") {
        for (j in seq_len(input[i, 2])) {
            h_coord[2] <- h_coord[2] - 1
            t_coord <- t_updater(t_coord, h_coord)
            h_path <- rbind(h_path, h_coord)
            t_path <- rbind(t_path, t_coord)
        }
    } else if (input[i, 1] == "L") {
        for (j in seq_len(input[i, 2])) {
            h_coord[1] <- h_coord[1] - 1
            t_coord <- t_updater(t_coord, h_coord)
            h_path <- rbind(h_path, h_coord)
            t_path <- rbind(t_path, t_coord)
        }
    } else {
        for (j in seq_len(input[i, 2])) {
            h_coord[1] <- h_coord[1] + 1
            t_coord <- t_updater(t_coord, h_coord)
            h_path <- rbind(h_path, h_coord)
            t_path <- rbind(t_path, t_coord)
        }
    }
}

print(dim(unique(t_path))[1])