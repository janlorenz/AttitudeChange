sd_sample <- function(x) sqrt(sd(x)^2*(length(x)-1)/(length(x)))
fragmentation <- function(S, bw = 0.1, dx = 0.01, from = -3.5, to = 3.5) {
	d <- density(S, bw = bw, from = from, to = to, n = (to-from)/dx + 1)
	Y <- diff(d$y)/dx
	sum(abs(Y)) * dx * sd_sample(S)
}