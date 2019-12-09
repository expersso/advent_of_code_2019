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
sum(fuel_needed(x, include_fuel_weight = TRUE)) # Solution to Part 2
