#!/usr/bin/env Rscript

library(argparse)
library(gridExtra)
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
commandline_parser$add_argument('-v', '--visibility',
            type='double', nargs='?', default=0.8,
            help='visibility')
commandline_parser$add_argument('-p', '--phase',
            type='double', nargs='?', default=0,
            help='phase')
commandline_parser$add_argument('-c', '--constant',
            type='double', nargs='?', default=40000,
            help='average intensity')
commandline_parser$add_argument('-s', '--steps',
            type='integer', nargs='?', default=15,
            help='phase steps')

args = commandline_parser$parse_args()
tries = args$n
constants = rep(args$c, tries)
phases = rep(args$p, tries)
visibilities = rep(args$v, tries)
steps = rep(args$s, tries)

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
original = data.frame(constant=constants, phase=phases, visibility=visibilities)
original$type = factor("original")
unweighted_fit = data.frame(constant=coefficients$X1,
                            phase=atan(-coefficients$X3 / coefficients$X2),
                            visibility=sqrt(
        coefficients$X2^2 + coefficients$X3^2) / coefficients$X1)
unweighted_fit$type = factor("unweighted")
weighted_fit = data.frame(constant=weighted_coefficients$X1,
            phase=atan(-weighted_coefficients$X3 / weighted_coefficients$X2),
            visibility=(sqrt(
                weighted_coefficients$X2^2 + weighted_coefficients$X3^2) /
                weighted_coefficients$X1))
weighted_fit$type = factor("weighted")
fits = rbind(unweighted_fit, weighted_fit)
#print(fits)
#plot histograms
hist_constant = ggplot(fits, aes(constant, fill=type)) + geom_histogram(
                    alpha=0.5,
                    position="identity",
                    binwidth=5)
hist_phase = ggplot(fits, aes(phase, fill=type)) + geom_histogram(
                    alpha=0.5,
                    position="identity",
                    binwidth=0.00015)
hist_visibility = ggplot(fits, aes(visibility, fill=type)) + geom_histogram(
                    alpha=0.5,
                    position="identity",
                    binwidth=0.0001)
x11(width=20)
histograms = arrangeGrob(hist_constant, hist_phase, hist_visibility, nrow=1)
print(histograms)
ggsave(sprintf("histograms%.2f.pdf", args$v), histograms)

#statistical tests
#variances
print(var.test(weighted_fit$visibility, unweighted_fit$visibility))
print(var.test(weighted_fit$phase, unweighted_fit$phase))
print(var.test(weighted_fit$constant, unweighted_fit$constant))
#means
print(t.test(weighted_fit$visibility, unweighted_fit$visibility))
print(t.test(weighted_fit$phase, unweighted_fit$phase))
print(t.test(weighted_fit$constant, unweighted_fit$constant, var.equal=TRUE))
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
