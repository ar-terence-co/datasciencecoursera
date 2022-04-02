library(data.table)
library(dplyr)

create_plot1 <- function(
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
    filter(Pollutant == "PM25-PRI") %>%
    group_by(year) %>%
    summarize(Emissions = sum(Emissions, na.rm = TRUE) / 1e6)
  
  fig_filename <- paste0(figdir, "/plot1.png")
  png(fig_filename, width = 480, height = 480)
  with(dt, {
    barplot(
      Emissions ~ year,
      xlab = "Year",
      ylab = "Total PM2.5 Emissions (Millions of Tons)",
      main = "Total PM2.5 Emissions between 1999 - 2008",
      col = "red",
    )
  })
  dev.off()
  
  invisible(dts)
}