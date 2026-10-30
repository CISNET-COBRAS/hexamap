# Open a null graphics device for the duration of the calling test
local_null_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off(), envir = env)
}

# Smooth age-period surface with the dimensions of the paper's example
example_rates <- function(ages = 15:70, periods = 1999:2018) {
  outer(ages, periods, function(a, p) 40 * exp(-((a - 40) / 15)^2) * (p - 1990) / 28)
}
