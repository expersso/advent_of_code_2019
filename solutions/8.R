get_layers <- function(x, height, width) {
  n <- height
  m <- width
  k <- n*m
  j <- length(x) %/% k
  lapply(1:j, function(i) matrix(x[((i-1)*k + 1):(j*k)], n, m, TRUE))
}

parse_data <- function(filename, height, width) {
  x <- readLines(filename)
  x <- as.numeric(strsplit(x, "")[[1]])
  get_layers(x, height, width)
}

n_dig_in_layer <- function(l, d) sum(l == d)

layer_w_fewest_0s <- function(x) {
  n_0s_in_layer <- sapply(x, n_dig_in_layer, d = 0)
  x[[order(n_0s_in_layer)[1]]]
}

`%add_color%` <- function(e1, e2) ifelse(e2 == 2, e1, e2)

get_final_image_matrix <- function(layers) {
  img <- Reduce(`%add_color%`, rev(layers))
  apply(img, 2, rev)
}

print_img <- function(x) {
  img <- get_final_image_matrix(x)
  image(t(img), col = c("white", "black"))
}

# I/O
x <- parse_data("inputs/8.txt", height = 6, width = 25)

l <- layer_w_fewest_0s(x)
n_dig_in_layer(l, 1) * n_dig_in_layer(l, 2) # Solution to Part 1

print_img(x) # Solution to Part 2
