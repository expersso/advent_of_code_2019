has_group_of_adj_digits         <- function(x) any(rle(x)$length >= 2)
has_digits_in_order             <- function(x) all(order(x) == 1:6)
has_group_of_adj_digits_len_two <- function(x) any(rle(x)$length == 2)

# I/O
x <- as.character(265275:781584)
x <- strsplit(x, "")
x <- x[sapply(x, has_group_of_adj_digits)]
x <- x[sapply(x, has_digits_in_order)]
y <- x[sapply(x, has_group_of_adj_digits_len_two)]

length(x) # Answer to Part 1
length(y) # Answer to Part 2
