#!/usr/bin/env Rscript

library(microbenchmark)
library(data.table)
library(ggplot2)

fft_fit = function(curve) {
    n = length(curve)
    fft_coefficients = fft(curve)
    constant = Re(fft_coefficients[[1]])
    first = fft_coefficients[[2]]
    return(data.table(constant=constant / n,
                      phase=Arg(first),
                      visibility=2 * Mod(first) / constant))
}

ls_fit = function(curve) {
    n = length(curve)
    angles = 2 * pi / n * (0:(n - 1))
    fit_matrix = cbind(cos(angles), sin(angles))
    curve[curve == 0] = 0.01 #avoid zero division
    coefficients = coef(lm(curve ~ fit_matrix, weights=1/curve))
    constant = coefficients[[1]]
    b = coefficients[[3]]
    a = coefficients[[2]]
    return(data.table(constant=constant,
                      phase=atan(-b / a),
                      visibility=(
                        sqrt(a^2 + b^2) / constant)))
}

phase_stepping_curve = function(constant, visibility, phase, steps) {
    angles = 2 * pi / steps * (0:(steps - 1))
    period = 2 * pi / steps
    xs = 0:(steps - 1)
    angles = period * xs + phase
    curve = constant * (1 + visibility * cos(angles))
    return(rpois(n=length(curve), curve))
}

curve = phase_stepping_curve(constant=1000,
                             visibility=0.5,
                             phase=0,
                             steps=10)

steps = seq(4, 40)
curve_list = lapply(steps,
                    phase_stepping_curve, constant=1000,
                    visibility=0.5, phase=0)

#print(curve_list)

fft_list = lapply(curve_list, function(x) call(name="fft_fit", x))
ls_list = lapply(curve_list, function(x) call(name="ls_fit", x))

fft_benchmark = data.table(summary(microbenchmark(list=fft_list, times=1000)))
ls_benchmark = data.table(summary(microbenchmark(list=ls_list, times=1000)))

fft_benchmark$algorithm = "fft"
ls_benchmark$algorithm = "ls"
fft_benchmark$steps = steps
ls_benchmark$steps = steps

benchmark = rbind(fft_benchmark, ls_benchmark)
print(benchmark)
plot = ggplot(benchmark,
              aes(x=steps,
                  y=median,
                  color=algorithm),
              group=type) + geom_line() + ylab(
              "runtime (us)") + xlab(
              "phase steps")

warnings()
x11(width=20, height=12)
print(plot)
ggsave("benchmark_fft_ls.pdf", plot)
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
