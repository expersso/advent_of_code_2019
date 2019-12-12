parse_data <- function(filename, height, width) {
  x <- readLines(filename)
  x <- as.numeric(strsplit(x, "")[[1]])
  a <- array(x, c(width, height, length(x) / (height * width)))
  aperm(a, c(2, 1, 3))
}

layer_w_fewest_0s <- function(x) {
  layer_w_min_0s_idx <- which.min(apply(x, 3, function(i) sum(i == 0)))
  x[, , layer_w_min_0s_idx]
}

`%add_color%` <- function(e1, e2) ifelse(e2 == 2, e1, e2)

get_final_image_matrix <- function(x) {
  y <- lapply(seq(dim(x)[3]), function(i) x[, , i])
  img <- Reduce(`%add_color%`, rev(y))
  apply(img, 2, rev)
}

print_img <- function(x) {
  img <- get_final_image_matrix(x)
  image(t(img), col = c("white", "black"))
}

# I/O
x <- parse_data("inputs/8.txt", height = 6, width = 25)
l <- layer_w_fewest_0s(x)
sum(l == 1) * sum(l == 2) # Solution to Part 1
print_img(x)              # Solution to Part 2
