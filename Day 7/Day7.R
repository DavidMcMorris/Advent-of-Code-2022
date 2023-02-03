# 2022 Day 7

library(data.tree)

input <- readLines("Day_7_input.txt")
filesystem <- Node$new("filesystem", dir = "T")
current <- filesystem

for (i in 2:length(input)){
    out <- strsplit(input[i], split = " ")[[1]]
        if (out[1] == "$") {
            if (out[2] == "cd") {
                if (out[3] == "..") {
                    current <- Navigate(current, "..")
                } else {
                    current <- Navigate(current, out[3])
                }
            }
        } else if (out[1] == "dir") {
            assign(out[2], current$AddChild(out[2], size = 0, dir = "T"))
        } else if (out[1] != "ls") {
            assign(out[2], current$AddChild(out[2], size = as.numeric(out[1]), dir = "F"))
        }
}

filesystem$Do(function(node) node$size <- Aggregate(node, attribute = "size", aggFun = sum), traversal = "post-order")

dir_sizes <- filesystem$Get("size", filterFun = function(x) x$dir == "T")
print(sum(dir_sizes[which(dir_sizes <= 100000)]))

freed_space <- 7e7 - filesystem$size + dir_sizes
print(min(freed_space[which(freed_space >= 3e7)]) - 7e7 + filesystem$size)