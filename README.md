# find-mean-through-r-programming
# Function to create a cacheable function
make_cacheable <- function(func) {
  cache <- NULL
  function(...) {
    args <- list(...)
    args_str <- paste(args, collapse = "_")
    if (is.null(cache) || !identical(args_str, cache$args_str)) {
      cat("Computing...\n")
      result <- func(...)
      cache <<- list(args_str = args_str, result = result)
    } else {
      cat("Using cached result...\n")
      result <- cache$result
    }
    return(result)
  }
}

# Example function to compute the mean of a numeric vector
mean_cacheable <- make_cacheable(function(x) {
  mean(x)
})

# Test the function
x <- c(1, 2, 3, 4, 5)
mean_cacheable(x)  # Computes and caches the mean
mean_cacheable(x)  # Uses the cached result
