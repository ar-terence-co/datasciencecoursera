corr <- function(directory, threshold = 0) {
  pollutantcorr <- vector("numeric")
  
  filenames <- sprintf("%s/%03d.csv", directory, 1:332)
  for (filename in filenames) {
    data <- read.csv(filename)
    
    data <- data[complete.cases(data),]
    if (nrow(data) <= threshold) next
    
    pollutantcorr <- append(pollutantcorr, cor(data$sulfate, data$nitrate))    
  }
  
  pollutantcorr
}
