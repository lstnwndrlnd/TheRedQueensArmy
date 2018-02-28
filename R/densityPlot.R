#' @title Generates a kernal density plot
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
densityPlot <- function (samples = 50, sample_size = 100,
                        mean = 0, sd = 5){
    sampleMeans <- rep(NA, samples)
    for(i in 1:samples){
      x <-rnorm(sample_size, mean = mean, sd = sd)
      sampleMeans[i] <- mean(x)
    }
    plot(density(sampleMeans))
}



