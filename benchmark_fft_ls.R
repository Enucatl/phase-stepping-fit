#!/usr/bin/env Rscript

library(microbenchmark)
library(data.table)
library(ggplot2)
library(argparse)
library(fftw)

fft_fit = function(curve) {
    n = length(curve)
    fft_coefficients = FFT(as.numeric(curve))
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

steps = seq(4, 104, by=10)
curve_list = lapply(steps,
                    phase_stepping_curve, constant=1000,
                    visibility=0.5, phase=0)

#print(curve_list)

fft_list = lapply(curve_list, function(x) call(name="fft_fit", x))
ls_list = lapply(curve_list, function(x) call(name="ls_fit", x))

commandline_parser = ArgumentParser(
        description="benchmark the fft vs the ls fit") 

commandline_parser$add_argument('-n',
            type='integer', nargs='?', default=10,
            help='number of tests per benchmark')

args = commandline_parser$parse_args()

fft_benchmark = data.table(summary(microbenchmark(list=fft_list,
                                                  times=args$n)))
ls_benchmark = data.table(summary(microbenchmark(list=ls_list,
                                                 times=args$n)))

fft_benchmark$algorithm = "fft"
ls_benchmark$algorithm = "ls"
fft_benchmark$steps = steps
ls_benchmark$steps = steps

benchmark = rbind(fft_benchmark, ls_benchmark)
plot = ggplot(benchmark,
              aes(x=steps,
                  y=median,
                  color=algorithm),
              group=type) + geom_line() + ylab(
              "runtime (us)") + xlab(
              "phase steps") + geom_ribbon(aes(
              x=steps,
              ymin=lq,
              ymax=uq,
              fill=algorithm,
              linetype=NA
              ),
                               alpha=0.3) + scale_y_continuous()

warnings()
file_name = "benchmark_fft_ls.pdf"
pdf(file_name,
    width=17, height=12)
print(plot)
dev.off()
embed_fonts("benchmark_fft_ls.pdf")
