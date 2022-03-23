rankhospital <- function(state, outcome, num = "best") {
  # set OUTCOME_DATA_FILENAME outside this fn
  data <- read.csv(OUTCOME_DATA_FILENAME, stringsAsFactors=FALSE)
  
  outcomePrefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcomeCols <- c(
    "heart attack" = paste0(outcomePrefix, "Heart.Attack"),
    "heart failure"= paste0(outcomePrefix, "Heart.Failure"),
    "pneumonia" = paste0(outcomePrefix, "Pneumonia")
  )
  
  if(
    !is.vector(state) |
    length(state) != 1 |
    !is.element(state, data$State)
  ) {
    stop("invalid state")
  }
  if(
    !is.vector(outcome) |
    length(outcome) != 1 |
    !is.element(outcome, names(outcome_cols))
  ) {
    stop("invalid outcome")
  } 
  if(
    !is.vector(num) |
    length(num) != 1 |
    (!is.element(num, c("best", "worst")) & !is.numeric(num) & !is.integer(num))
  ) {
    stop("invalid num")
  }
  
  outcomeCol <- outcomeCols[[outcome]]
  rankoutcomes(data[data$State == state,], outcomeCol, num)
}

best <- function(state, outcome) {
  rankhospital(state, outcome, "best")
}

rankall <- function(outcome, num = "best") {
  # set OUTCOME_DATA_FILENAME outside this fn
  data <- read.csv(OUTCOME_DATA_FILENAME, stringsAsFactors=FALSE)
  
  outcomePrefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcomeCols <- c(
    "heart attack" = paste0(outcomePrefix, "Heart.Attack"),
    "heart failure"= paste0(outcomePrefix, "Heart.Failure"),
    "pneumonia" = paste0(outcomePrefix, "Pneumonia")
  )
  
  if(
    !is.vector(outcome) |
    length(outcome) != 1 |
    !is.element(outcome, names(outcome_cols))
  ) {
    stop("invalid outcome")
  } 
  if(
    !is.vector(num) |
    length(num) != 1 |
    (!is.element(num, c("best", "worst")) & !is.numeric(num) & !is.integer(num))
  ) {
    stop("invalid num")
  }
  
  outcomeCol <- outcomeCols[[outcome]]
  dataByState <- split(data, as.factor(data$State))
  hospitalByState = sapply(dataByState, rankoutcomes, outcomeCol = outcomeCol, num = num)
  
  data.frame(hospital = hospitalByState, state = names(hospitalByState))
}

rankoutcomes <- function(data, outcomeCol, num = "best") {
  outcomeData <- data[, c("Hospital.Name", "State", outcomeCol)]
  outcomeData[[outcomeCol]] <- as.numeric(outcomeData[[outcomeCol]])
  outcomeData <- outcomeData[!is.na(outcomeData[[outcomeCol]]),]
  
  rank <- if (num == "best") {
    1
  } else if (num == "worst") {
    nrow(outcomeData)
  } else {
    num
  }
  
  outcomeData <- outcomeData[order(outcomeData[[outcomeCol]], outcomeData$Hospital.Name),]
  
  hospital <- outcomeData$Hospital.Name[rank]
  hospital
}

