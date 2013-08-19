#!/usr/bin/env Rscript

library(argparse)
library(plyr)
library(ggplot2)

phase_stepping_curve = function(c, v, phi, n) {
    #Return the phase stepping curve sampled over one period
    #with average c, visibility v, shift phi and n steps.
    p = 2 * pi / n
    xs = 0:(n - 1)
    angles = p * xs + phi
    return(c * (1 + v * cos(angles)))
}

commandline_parser = ArgumentParser(
        description="test various fit methods for the phase stepping curves.") 

commandline_parser$add_argument('-n',
            type='integer', nargs='?', default=100,
            help='number of comparisons')

args = commandline_parser$parse_args()
tries = args$n
constants = rep(40000, tries)
phases = rep(0, tries)
visibilities = rep(0.8, tries)
steps = rep(15, tries)

curves = Map(phase_stepping_curve,
             constants, visibilities, phases, steps)

noisy_curves = lapply(curves,
                      function(x) unlist(lapply(x, rpois, n=1)))

angles = Map(function(n) 2 * pi / n * (0:(n - 1)), steps)

fit_matrices = Map(cbind,
                   Map(cos, angles),
                   Map(sin, angles))

fits = Map(lsfit, fit_matrices, noisy_curves)

#fit with inverse of the square of the noise as weight
weights = lapply(noisy_curves, function(x) 1 / x)
weighted_fits = Map(lsfit, fit_matrices, noisy_curves, wt=weights)
weighted_coefficients = Map(as.numeric,
                            Map(get, "coefficients", weighted_fits))

coefficients = Map(as.numeric, Map(get, "coefficients", fits))
coefficients = data.frame(do.call(rbind, coefficients), row.names=NULL)
weighted_coefficients = data.frame(do.call(rbind, weighted_coefficients),
                            row.names=NULL)
original = data.frame(c=constants, phi=phases, v=visibilities)
original$type = factor("original")
unweighted_fit = data.frame(c=coefficients$X1,
                            phi=atan(-coefficients$X3 / coefficients$X2),
                            v=sqrt(
        coefficients$X2^2 + coefficients$X3^2) / coefficients$X1)
unweighted_fit$type = factor("unweighted")
weighted_fit = data.frame(c=weighted_coefficients$X1,
            phi=atan(-weighted_coefficients$X3 / weighted_coefficients$X2),
            v=(sqrt(
                weighted_coefficients$X2^2 + weighted_coefficients$X3^2) /
                weighted_coefficients$X1))
weighted_fit$type = factor("weighted")
fits = rbind(unweighted_fit, weighted_fit)
print(fits)
#plot histograms
hist_c = ggplot(fits, aes(phi, fill=type)) + geom_histogram(
                    alpha=0.5,
                    aes(y=..density..),
                    position="identity")
x11(type='cairo')
print(hist_c)
#dev.off()
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
