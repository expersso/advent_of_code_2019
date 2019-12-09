parse_instructions <- function(x) {
  x <- strsplit(x, ",")[[1]]
  data.frame(
    direction = substr(x, 1, 1),
    steps = as.integer(substring(x, 2)),
    stringsAsFactors = FALSE
  )
}

move <- function(position, direction, steps) {
  switch(
    direction,
    "L" = c(position[1] - steps, position[2]),
    "R" = c(position[1] + steps, position[2]),
    "D" = c(position[1], position[2] - steps),
    "U" = c(position[1], position[2] + steps),
  )
}

get_coordinates <- function(path) {
  output_path <- Reduce(
    f = function(position, i) move(position, path$direction[i], path$steps[i]),
    x = seq_len(nrow(path)),
    init = c(0, 0),
    accumulate = TRUE
  )
  df <- do.call(rbind, output_path)
  df <- as.data.frame(df)
  names(df) <- c("x", "y")
  df
}

trace_path <- function(x1, y1, x2, y2) {
  data.frame(x = seq(x1, x2), y = seq(y1, y2))
}

get_full_path <- function(path) {
  out <- lapply(
    seq_len(nrow(path) - 1),
    function(i) trace_path(path$x[i], path$y[i], path$x[i + 1], path$y[i + 1])
  )
  do.call(rbind, out)
}

find_crossovers <- function(full_paths) {
  coord_tuples <- lapply(full_paths, function(x) split(x, sort(as.numeric(rownames(x)))))
  crossovers <- intersect(coord_tuples[[1]], coord_tuples[[2]])
  crossovers <- do.call(rbind, crossovers)
  crossovers$dist <- abs(crossovers$x) + abs(crossovers$y)
  crossovers
}

add_total_steps_taken <- function(full_path) {
  full_path$steps <- cumsum(c(0, abs(diff(full_path$x)) + abs(diff(full_path$y))))
  full_path
}

steps_taken_to_crossover <- function(full_path, crossovers) {
  mapply(function(x, y) full_path$steps[full_path$x == x & full_path$y == y],
         crossovers$x, crossovers$y)
}

find_total_steps_taken_to_crossovers <- function(full_paths, crossovers) {
  steps_taken <- data.frame(
    path_1 = steps_taken_to_crossover(full_paths[[1]], crossovers),
    path_2 = steps_taken_to_crossover(full_paths[[2]], crossovers)
  )
  steps_taken$total <- steps_taken$path_1 + steps_taken$path_2
  steps_taken
}

# I/O
instructions <- readLines("inputs/3.txt")
instructions <- lapply(instructions, parse_instructions)
coordinates  <- lapply(instructions, get_coordinates)
full_paths   <- lapply(coordinates, get_full_path)
crossovers   <- find_crossovers(full_paths)
full_paths   <- lapply(full_paths, add_total_steps_taken)
steps_taken  <- find_total_steps_taken_to_crossovers(full_paths, crossovers)

sort(crossovers$dist)[2]   # Answer to Part 1
sort(steps_taken$total)[2] # Answer to Part 2

# Plot of wiring
rng <- range(c(full_paths[[1]][, -3], full_paths[[2]][, -3]))
plot(NA, NA, xlim = rng, ylim = rng, xlab = "", ylab = "")
lines(coordinates[[1]], type = "s", col = "blue")
lines(coordinates[[2]], type = "s", col = "red")
points(crossovers)
