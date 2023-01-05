# 2022 Day 11

input <- readLines("Day_11_input.txt")

num_rounds <- 10000
div_3 <- 0  #Set to 0 to not divide by 3

num_monks <- (length(input) + 1) / 7
monk_list <- NULL
num_inspections <- rep(0, num_monks)

for (i in 1:num_monks) {
    item_line <- strsplit(input[7 * (i - 1) + 2], split = "Starting items: ")[[1]][2]
    item_line <- as.numeric(strsplit(item_line, split = ", ")[[1]])
    items <- item_line[which(!is.na(item_line))]
    monk_list[[i]] <- items
}

operation <- function(monk, old) {
    index <- 7 * monk + 3
    operation_line <- strsplit(input[index], split = ": ")[[1]][2]
    eval(parse(text = operation_line))
    return(new)
}

where_next <- function(monk, flag) {
    if (flag == 0) {
        index <- 7 * monk + 5
        where <- as.numeric(strsplit(input[index], split = "If true: throw to monkey ")[[1]][2])
    } else {
        index <- 7 * monk + 6
        where <- as.numeric(strsplit(input[index], split = "If false: throw to monkey ")[[1]][2])
    }
    return(where)
}

div_test_value <- function(monk) {
    index <- 7 * monk + 4
    value <- as.numeric(strsplit(input[index], split = "Test: divisible by ")[[1]][2])
    return(value)
}

inspection <- function(monk, items) {
    items <- operation(monk, items)
    if (div_3 != 0) {
        items <- floor(items / 3)
    }
    return(items)
}

round <- function(monk_list, num_inspections, div_test_prod) {
    for (i in 1:num_monks) {
        monk <- i - 1
        if (!is.na(monk_list[[i]][1])) {
            new_level <- inspection(monk, monk_list[[i]])
            for (k in seq_along(new_level)) {
                if (new_level[k] > div_test_prod) {
                    new_level[k] <- new_level[k] %% div_test_prod
                }
            }
            test_val <- div_test_value(monk)
            div_result <- new_level %% test_val
            where_t <- where_next(monk, 0) + 1
            where_f <- where_next(monk, 1) + 1
            for (j in seq_along(monk_list[[i]])) {
                if (div_result[j] == 0) {
                    monk_list[[where_t]] <- c(monk_list[[where_t]][!is.na(monk_list[[where_t]])], new_level[j])
                } else {
                    monk_list[[where_f]] <- c(monk_list[[where_f]][!is.na(monk_list[[where_f]])], new_level[j])
                }
                num_inspections[i] <- num_inspections[i] + 1
            }
            monk_list[[i]] <- NA
        }
    }
    return_list <- list(monk_list, num_inspections)
    return(return_list)
}

div_test_prod <- 1
for (i in 1:num_monks) {
    div_test_prod <- div_test_prod * div_test_value(i - 1)
}

for (i in 1:num_rounds) {
    output_list <- round(monk_list, num_inspections, div_test_prod)
    monk_list <- output_list[[1]]
    num_inspections <- output_list[[2]]
}

print(prod(rev(sort(num_inspections))[1:2]))