construct_tree <- function(tree) {
  parse_tree <- function(node, x) {
    children <- sapply(tree, function(i) i[1] == node)
    children_names <- sapply(tree[children], function(n) n[2])
    if(is.null(children)) return(node)
    c(node, lapply(children_names, function(n) parse_tree(n, x[[!children]])))
  }
  parse_tree
}

count_orbits <- function(node, count = 0) {
  t <- tail(node, -1)
  if(!length(t)) return(count)
  count + sum(sapply(t, count_orbits, count = count + 1))
}

make_df <- function(x) {
  df <- as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE)
  names(df) <- c("to", "from")
  df
}

get_path <- function(df, from, to) {
  new_to <- df[df$from == from, "to"]
  if(new_to == to) return(from)
  c(from, get_path(df, new_to, to))
}

get_shortest_path <- function(df, n1, n2) {
  n1_com <- get_path(df, n1, "COM") # Path from node 1 to root node
  n2_com <- get_path(df, n2, "COM") # Path from node 2 to root node

  common_anc <- intersect(n1_com, n2_com)[1] # Find 1st common ancestor

  n1_ca <- get_path(df, n1, common_anc) # Path from node 1 to common ancestor
  n2_ca <- get_path(df, n2, common_anc) # Path from node 2 to common ancestor

  c(n1_ca, common_anc, rev(n2_ca)) # Concatenated path
}

# I/O
x <- strsplit(readLines("inputs/6.txt"), ")")
tree <- construct_tree(x)("COM", x)
count_orbits(tree) # Answer to Part 1

you_san <- get_shortest_path(make_df(x), "YOU", "SAN")
length(you_san) - 3 # Answer to Part 2
