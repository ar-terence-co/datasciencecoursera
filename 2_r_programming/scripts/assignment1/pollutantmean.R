pollutantmean <- function(directory, pollutant, id = 1:332) {
  means <- vector("numeric")
  counts <- vector("integer")
  
  filenames <- sprintf("%s/%03d.csv", directory, id)
  for (filename in filenames) {
    data <- read.csv(filename)
    values <- data[[pollutant]]
    values <- values[!is.na(values)]
    
    means <- append(means, mean(values))
    counts <- append(counts, length(values))
  }
  
  means[is.na(means)] <- 0
  sum(means * counts) / sum(counts)
}
