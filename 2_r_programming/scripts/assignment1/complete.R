complete <- function(directory, id = 1:332) {
  nobs <- vector("integer")
  
  filenames <- sprintf("%s/%03d.csv", directory, id)
  for (filename in filenames) {
    data <- read.csv(filename)
    
    nobs <- append(nobs, sum(complete.cases(data)))
  }
  
  data.frame(id = id, nobs = nobs)
}
