run_program <- function(x, input, i = 1) {

  jump_i <- i
  opcode_pars <- x[i]
  opcode <- opcode_pars %% 100
  if(opcode == 99) return(TRUE) # Base case

  instruction <- switch(opcode,
    function(e1, e2, e3) {x[e3 + 1] <- e1 + e2; x},        # Addition
    function(e1, e2, e3) {x[e3 + 1] <- e1 * e2; x},        # Multiplication
    function(e1)         {x[e1 + 1] <- input; x},          # Input
    function(e1)         {cat(e1, "\n"); x},               # Output
    function(e1, e2)     {if( e1) {jump_i <<- e2 + 1}; x}, # Jump-if-true
    function(e1, e2)     {if(!e1) {jump_i <<- e2 + 1}; x}, # Jump-if-false
    function(e1, e2, e3) {x[e3 + 1] <- e1 <  e2; x},       # Less than
    function(e1, e2, e3) {x[e3 + 1] <- e1 == e2; x}        # Equals
  )

  # Get parameter lengths and names
  n      <- length(formals(instruction))
  params <- structure(x[seq_len(n) + i], names = formalArgs(instruction))

  # Parse parameter modes
  parmodes <- floor(opcode_pars / 10 ^ (seq_len(n) + 1)) %% 10

  # Write operations always use immediate mode for write parameter
  if(!opcode %in% 4:6) parmodes[length(parmodes)] <- 1

  # Fetch parameters using given parameter modes
  params <- lapply(seq_along(params),
                   function(j) if(parmodes[j]) params[j] else x[params[j] + 1])

  # Run instruction with given parameters
  x <- do.call(instruction, params)

  # Move pointer to position given by either jump instruction or by param length
  i <- if(jump_i != i) jump_i else i + n + 1

  run_program(x, input, i)
}

x <- scan("inputs/5.txt", sep = ",")
y <- run_program(x, input = 1) # Solution to Part 1
y <- run_program(x, input = 5) # Solution to Part 2
