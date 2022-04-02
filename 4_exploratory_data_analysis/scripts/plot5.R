library(data.table)
library(dplyr)
library(ggplot2)

create_plot5<- function(
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
    left_join(scc, by = "SCC", all.x = TRUE) %>%
    filter(Pollutant == "PM25-PRI" & fips == "24510" & grepl("Vehicle", EI.Sector)) %>%
    mutate(year = factor(year)) %>%
    group_by(year) %>%
    summarize(Emissions = sum(Emissions, na.rm = TRUE))

  fig_filename <- paste0(figdir, "/plot5.png")
  png(fig_filename, width = 480, height = 480)
  g <- ggplot(dt, aes(year, Emissions))
  g <- g + geom_col(bg = "orange") +
    labs(
      title = "Total PM2.5 Emissions from Vehicles\nin Baltimore City, MD between 1999 - 2008",
      x = "Year",
      y = "Total PM2.5 Emissions from Vehicles (Tons)"
    )
  print(g)
  dev.off()
  
  invisible(dts)
}