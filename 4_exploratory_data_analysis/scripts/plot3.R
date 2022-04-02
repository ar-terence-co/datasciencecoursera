library(data.table)
library(dplyr)
library(ggplot2)

create_plot3<- function(
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
    mutate(type = factor(type), year = factor(year)) %>%
    group_by(year, type) %>%
    summarize(Emissions = sum(Emissions, na.rm = TRUE), groups = "keep")
  
  fig_filename <- paste0(figdir, "/plot3.png")
  png(fig_filename, width = 720, height = 480)
  g <- ggplot(dt, aes(year, Emissions))
  g <- g + facet_grid(. ~ type) +
    geom_col(bg = "orange") +
    labs(
      title = "Total PM2.5 Emissions by Source Type\nin Baltimore City, MD between 1999 - 2008",
      x = "Year",
      y = "Total PM2.5 Emissions (Tons)"
    )
  print(g)
  dev.off()
  
  invisible(dts)
}