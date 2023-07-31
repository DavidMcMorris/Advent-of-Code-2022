# 2022 Day 13

input <- readLines("Day_13_input.txt")
input_split <- lapply(lapply(input, strsplit, split = ""), unlist)

text_to_list <- function(x) {
    x[which(x == "[")] <- "list("
    x[which(x == "]")] <- ")"
    x <- eval(parse(text = paste(x, collapse = "")))
}

pair_list <- list()
ind <- 0
for (i in which(input == "")){
    ind <- ind + 1
    x <- text_to_list(input_split[[i - 2]])
    y <- text_to_list(input_split[[i - 1]])
    pair_list[[ind]] <- list(x, y)
}
x <- text_to_list(input_split[[i + 1]])
y <- text_to_list(input_split[[i + 2]])
pair_list[[ind + 1]] <- list(x, y)


left_ind <- 1
right_ind <- 1

compare <- function(left_list, right_list, left_ind, right_ind) {
    x <- 0
    left_length <- length(left_list)
    right_length <- length(right_list)
    if (left_ind > left_length && right_ind <= right_length) {
        return(1)
    }
    if (left_ind <= left_length && right_ind > right_length) {
        return(0)
    }
    if (left_ind > left_length && right_ind > right_length) {
        return(2)
    }
    left <- left_list[[left_ind]]
    right <- right_list[[right_ind]]
    if (is.numeric(left) && is.numeric(right)) {
        if (left < right) {
            return(1)
        }
        if (left > right) {
            return(0)
        }
        if (left == right) {
            x <- compare(left_list, right_list, left_ind + 1, right_ind + 1)
        }
    } else if (is.list(left) && is.list(right)) {
        x <- compare(left_list[[left_ind]], right_list[[right_ind]], 1, 1)
    } else if (is.list(left) && is.numeric(right)) {
        x <- compare(left_list[[left_ind]], list(right_list[[right_ind]]), 1, 1)
    } else if (is.list(right) && is.numeric(left)) {
        x <- compare(list(left_list[[left_ind]]), right_list[[right_ind]], 1, 1)
    }
    if (x == 2) {
        x <- compare(left_list, right_list, left_ind + 1, right_ind + 1)
    }
    return(x)
}

a <- NULL
for (i in seq_along(pair_list)) {
    a <- c(a, compare(pair_list[[i]][[1]], pair_list[[i]][[2]], 1, 1))
}

print(sum(which(a == 1)))