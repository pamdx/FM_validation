# Function for more user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

# A function factory for getting integer axis values on plots.

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}