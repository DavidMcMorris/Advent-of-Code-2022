# 2022 Day 6

input <- scan("Day_6_input.txt", sep = "", what = "character")
input <- strsplit(input, split = "")[[1]]

flag <- "F"
i <- 1

while (flag == "F") {
    word <- input[i:(i + 3)]
    if (length(unique(word)) == 4) {
        flag <- "T"
        print(i + 3)
    }
    i <- i + 1
}