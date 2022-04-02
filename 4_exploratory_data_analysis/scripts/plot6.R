library(data.table)
library(dplyr)
library(ggplot2)

create_plot6<- function(
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
  
  loc_names <- data.table(
    fips = c("24510", "06037"),
    Location.Name = c("Baltimore City, MD", "Los Angeles County, CA")
  )

  dt <- pm25 %>%
    left_join(scc, by = "SCC", all.x = TRUE) %>%
    filter(Pollutant == "PM25-PRI" & fips %in% loc_names$fips & grepl("Vehicle", EI.Sector)) %>%
    left_join(loc_names, by = "fips", all.x = TRUE) %>%
    mutate(year = factor(year), Location.Name = factor(Location.Name)) %>%
    group_by(year, Location.Name) %>%
    summarize(Emissions = sum(Emissions, na.rm = TRUE)) %>%
    group_by(Location.Name) %>%
    mutate(Emissions.1999 = first(Emissions, year), Change.Emissions = (Emissions - Emissions.1999) / Emissions.1999)
  
  fig_filename <- paste0(figdir, "/plot6.png")
  png(fig_filename, width = 720, height = 480)
  g <- ggplot(dt, aes(year, Change.Emissions, fill = Change.Emissions))
  g <- g + facet_grid(. ~ Location.Name) +
    geom_col() +
    scale_y_continuous(
      breaks = pretty(dt$Change.Emissions, by = 0.1),
      labels = scales::percent
    ) +
    scale_fill_gradient2(low='green', mid='snow3', high='red', space='Lab') +
    labs(
      title = "% Change of PM2.5 Emissions from Vehicles since 1999\nComparing Baltimore City, MD & Los Angeles County, CA between 1999 - 2008",
      x = "Year",
      y = "% Change of PM2.5 Emissions from Vehicles since 1999",
      fill = "% Change of\nPM2.5 Emissions"
    )
  print(g)
  dev.off()
  
  invisible(dts)
}