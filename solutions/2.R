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

# Solution to Part 2
inputs <- find_inputs_to_gen_target(program, target_output)
puzzle_output(inputs$noun, inputs$verb)
