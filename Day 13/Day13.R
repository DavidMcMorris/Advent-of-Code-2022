# 2022 Day 13

input <- readLines("Day_13_test_input.txt")
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
