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
constants = rep(40000, tries)
phases = rep(0, tries)
visibilities = rep(0.8, tries)
steps = rep(15, tries)

curves = Map(phase_stepping_curve,
             constants, visibilities, phases, steps)

print(curves)

noisy_curves = lapply(curves,
                      function(x) unlist(lapply(x, rpois, n=1)))

print(noisy_curves)

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
weighted_coefficients = data.frame(do.call(rbind, weighted_coefficients))

coefficients = Map(as.numeric, Map(get, "coefficients", fits))
coefficients = data.frame(do.call(rbind, coefficients))
coefficients$equal_weights_c = coefficients$X1
coefficients$equal_weights_phi = atan(-coefficients$X3 / coefficients$X2)
coefficients$equal_weights_v = sqrt(
        coefficients$X2^2 + coefficients$X3^2) / coefficients$X1
coefficients$orig_c = constants
coefficients$orig_phi = phases
coefficients$orig_v = visibilities
coefficients$weighted_c = weighted_coefficients$X1
coefficients$weighted_phi = atan(-weighted_coefficients$X3 /
                                 weighted_coefficients$X2)
coefficients$weighted_v = (sqrt(
        weighted_coefficients$X2^2 + weighted_coefficients$X3^2)
        / weighted_coefficients$X1)

coefficients$X1 = NULL
coefficients$X2 = NULL
coefficients$X3 = NULL

print(coefficients)
