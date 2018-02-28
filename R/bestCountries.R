#' @title Generates a poor man's sampling distribution
#'
#' @param samples Specify how many samples to run
#' @param sample_size the number of observations in each sample
#' @param mean population mean
#' @param sd population standard deviation
#'
#' @export
#'
# shift + cmd + d
# or roxygen::roxygenise()

library(tidyverse)
wine <- read_csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv")

wine <- select(wine, -X1)

head(wine)
bestCountries <- function(){
  country <-
    wine %>%
    filter(!is.na(price), !is.na(points)) %>%
    group_by(country) %>%
    summarise(
      count = n()
      ,mean_points = mean(points, na.rm = TRUE)
      ,mean_price = mean(price, na.rm = TRUE)
    ) %>%
    arrange(desc(count)) %>%
    head(10)%>%
    arrange(desc(mean_points))

  hist(country$count)
  x <- seq(64, 74, length.out=100)
  y <- with(Galton, dnorm(x, mean(parent), sd(parent)))
  lines(x, y, col = "red")
}

