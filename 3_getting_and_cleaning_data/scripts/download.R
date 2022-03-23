library(data.table)

download_2006housing_to_dt <- function() {
  file <- "3_getting_and_cleaning_data/data/quiz_1_2006housing.csv"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", 
    destfile=file, 
  )
  print(date())
  fread(file)
}

library(readxl)

download_ngap_to_dt <- function() {
  file <- "3_getting_and_cleaning_data/data/quiz_1_ngap.xlsx"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", 
    destfile=file, 
  )
  print(date())
  setDT(read_excel(file, range="G18:O23"))
}

library(XML)

download_restaurants_to_doc <- function() {
  file <- "3_getting_and_cleaning_data/data/quiz_1_restaurants.xml"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", 
    destfile=file, 
  )
  print(date())
  xmlTreeParse(file, useInternal=TRUE)
}

download_2006housing_v2_to_dt <- function() {
  file <- "3_getting_and_cleaning_data/data/quiz_1_2006housing_v2.csv"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", 
    destfile=file, 
  )
  print(date())
  fread(file)
}

library(jpeg)

download_jeff_jpeg <- function() {
  file <- "3_getting_and_cleaning_data/data/jeff.jpeg"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", 
    destfile=file, 
  )
  print(date())
  readJPEG(file, native = TRUE)
}

download_gdp_to_dt <- function() {
  file <- "3_getting_and_cleaning_data/data/gdp.csv"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", 
    destfile=file, 
  )
  print(date())
  fread(file, skip=5, header=FALSE, col.names=c("countrycode", "ranking", "economy", "gdp"), nrow = 190, drop=append(6:10, 3))
}

download_edstats_to_dt <- function() {
  file <- "3_getting_and_cleaning_data/data/edstats.csv"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", 
    destfile=file, 
  )
  print(date())
  fread(file)
}