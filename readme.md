### Day 1

``` r
fuel_needed <- function(x, include_fuel_weight = FALSE) {
  fuel <- x %/% 3 - 2
  if(!include_fuel_weight) return(fuel)
  if(fuel <= 0) return(0)
  fuel + fuel_needed(fuel, include_fuel_weight)
}

fuel_needed <- Vectorize(fuel_needed, "x")

# I/O
x <- scan("inputs/1.txt")
sum(fuel_needed(x)) # Solution to Part 1
```

    ## [1] 3368364

``` r
sum(fuel_needed(x, include_fuel_weight = TRUE)) # Solution to Part 2
```

    ## [1] 5049684

### Day 2

``` r
run_program <- function(x, i = 1) {
  opcode <- x[i]                 # operator code
  if(opcode == 99) return(x)     # halting condition
  op <- switch(opcode, `+`, `*`) # operator to use
  e1 <- x[i + 1] + 1             # location of 1st element to operate on
  e2 <- x[i + 2] + 1             # location of 2nd element to operate on
  si <- x[i + 3] + 1             # location to store result
  x[si] <- op(x[e1], x[e2])      # overwrite entry
  run_program(x, i + 4)          # recurse
}

run_w_input <- function(x, noun, verb) {
  x[2:3] <- c(noun, verb)
  run_program(x)[1]
}

run_w_input <- Vectorize(run_w_input, c("noun", "verb"))

find_all_possible_outputs <- function(x) {
  df <- expand.grid(noun = 0:99, verb = 0:99)         # DF of valid inputs
  df$output <- run_w_input(program, df$noun, df$verb) # Iterate over inputs
  df
}

find_inputs_to_gen_target <- function(x, target) {
  df <- find_all_possible_outputs(x)
  df[df$output == target, ]
}

puzzle_output <- function(noun, verb) 100 * noun + verb

# I/O
program <- scan("inputs/2.txt", sep = ",")
target_output <- 19690720

# Solution to Part 1
run_w_input(program, 12, 2)
```

    ## [1] 5534943

``` r
# Solution to Part 2
inputs <- find_inputs_to_gen_target(program, target_output)
puzzle_output(inputs$noun, inputs$verb)
```

    ## [1] 7603

### Day 3

``` r
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
```

    ## [1] 2193

``` r
sort(steps_taken$total)[2] # Answer to Part 2
```

    ## [1] 63526

``` r
# Plot of wiring
rng <- range(c(full_paths[[1]][, -3], full_paths[[2]][, -3]))
plot(NA, NA, xlim = rng, ylim = rng, xlab = "", ylab = "")
lines(coordinates[[1]], type = "s", col = "blue")
lines(coordinates[[2]], type = "s", col = "red")
points(crossovers)
```

![](readme_files/figure-markdown_github/day3-1.png)

### Day 4

``` r
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
```

    ## [1] 960

``` r
length(y) # Answer to Part 2
```

    ## [1] 626
