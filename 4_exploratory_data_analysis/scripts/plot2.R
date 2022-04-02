library(data.table)
library(dplyr)

create_plot2 <- function(
  dts = NULL, 
  datadir = "4_exploratory_data_analysis/data", 
  figdir = "4_exploratory_data_analysis/figures"
) {
  if (is.null(dts)) {
    pm25_filename = paste0(datadir, "/summarySCC_PM25.rds")
    scc_filename = paste0(datadir, "/Source_Classification_Code.rds")
  
    dts <- list(
      pm25 = setDT(readRDS(pm25_filename)),
      scc = setDT(readRDS(scc_filename))
    )
  }
  
  pm25 <- dts$pm25
  scc <- dts$scc
  
  dt <- pm25 %>% 
    filter(Pollutant == "PM25-PRI" & fips == "24510") %>%
    group_by(year) %>%
    summarize(Emissions = sum(Emissions, na.rm = TRUE))
  
  fig_filename <- paste0(figdir, "/plot2.png")
  png(fig_filename, width = 480, height = 480)
  with(dt, {
    barplot(
      Emissions ~ year,
      xlab = "Year",
      ylab = "Total PM2.5 Emissions (Tons)",
      main = "Total PM2.5 Emissions in Baltimore City, MD\n between 1999 - 2008",
      col = "orange",
    )
  })
  dev.off()
  
  invisible(dts)
}