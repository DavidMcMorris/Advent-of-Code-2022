#2022 Day 1

input <- readLines("Day_1_input.txt")
input <- as.numeric(input)


inds <- c(1,which(is.na(input)))
input[which(is.na(input))]<-0

calories <- NULL

for(i in 1:(length(inds) - 2)){
	calories[i] <- sum(input[inds[i]:inds[i+1]])
}

MaxCal <- max(calories)
MaxThree <- sum(rev(sort(calories))[1:3])