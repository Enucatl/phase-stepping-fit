#!/usr/bin/env Rscript

library(argparse)
library(plyr)

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
            type='integer', nargs='?', default=10000,
            help='number of comparisons')

args = commandline_parser$parse_args()
tries = args$n
constants = runif(tries, 100, 100000)
phases = runif(tries, -pi / 2, pi / 2)
visibilities = runif(tries, 0, 1)
steps = sample(4:24, tries, replace=TRUE)

curves = Map(phase_stepping_curve,
             constants, visibilities, phases, steps)

angles = Map(function(n) 2 * pi / n * (0:(n - 1)), steps)

fit_matrices = Map(cbind,
                   Map(cos, angles),
                   Map(sin, angles))

fits = Map(lsfit, fit_matrices, curves)

coefficients = Map(as.numeric, Map(get, "coefficients", fits))
coefficients = data.frame(do.call(rbind, coefficients))
