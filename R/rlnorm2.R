rlnorm2 <-
function(n, mean = exp(0.5), sdlog = 1) {
  rlnorm(n, log(mean) - sdlog^2/2, sdlog)
}
