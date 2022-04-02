library(data.table)

download_data <- function(datadir = "4_exploratory_data_analysis/data", should_download = FALSE) {
  zipfilename = paste0(datadir, "/pm25_emissions.zip")
  if (should_download | !file.exists(zipfilename)) {
    download.file(
      "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
      destfile = zipfilename
    )
    unzip(zipfilename, exdir = datadir)
  }
}