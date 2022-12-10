# 2022 Day 6

input <- scan("Day_6_input.txt", sep = "", what = "character")
input <- strsplit(input, split = "")[[1]]
len <- 14

flag <- "F"
i <- 1

while (flag == "F") {
    word <- input[i:(i + len - 1)]
    if (length(unique(word)) == len) {
        flag <- "T"
        print(i + len - 1)
    }
    i <- i + 1
}