#!/usr/bin/env Rscript

library(argparse)
library(foreach)
library(doParallel)
library(data.table)
library(e1071)
library(nortest)

registerDoParallel(cores=7)

phase_stepping_curve = function(constant, visibility, phase, steps) {
    period = 2 * pi / steps
    xs = 0:(steps - 1)
    angles = period * xs + phase
    return(constant * (1 + visibility * cos(angles)))
}

get_noisy_curves = function(n, curve) {
    return(replicate(n,
                  rpois(n=length(curve), curve)))
}

fit_curves = function(curves) {
    #fit with both fft and ls
    n = dim(curves)[[1]]
    angles = 2 * pi / n * (0:(n - 1))
    fit_matrix = cbind(cos(angles), sin(angles))
    fft_coefficients = apply(curves, 2, fft) / n
    fit_coefficients = apply(curves, 2, function(y, ...)
            coef(lm(y ~ fit_matrix, weights=1/y)))
    fft_df = data.frame(constant=Mod(fft_coefficients[1, ]),
                        phase=Arg(fft_coefficients[2, ]),
                        visibility=2 * (Mod(fft_coefficients[2, ]) /
                                        Mod(fft_coefficients[1, ])))
    fft_df$type = factor("fft")
    fit_df = data.frame(constant=fit_coefficients[1, ],
                        phase=atan(-fit_coefficients[3, ] /
                                   fit_coefficients[2, ]),
                        visibility=(sqrt(
                        fit_coefficients[2, ]^2 + fit_coefficients[3, ]^2) /
                        fit_coefficients[1, ]))
    fit_df$type = factor("ls")
    return(data.table(rbind(fft_df, fit_df)))
}

statistical_tests = function(x) {
    return(c(
                mean(x),
                sd(x),
                kurtosis(x),
                skewness(x),
                lillie.test(x)$p.value,
                shapiro.test(x)$p.value))
}

names = c("mean", "sd",
          "kurtosis", "skewness",
          "normality.p.value.lilliefors",
          "normality.p.value.shapiro")

analyse_fits = function(fit_df) {
    #summarise the data from the phase stepping curves with the above list
    #of statistical tests
    setkey(fit_df, type)
    result = fit_df[, c(list(stat=names), lapply(.SD, statistical_tests)), by=type]
    setkeyv(result, c("stat", "type"))
    return(result)
}

commandline_parser = ArgumentParser(
        description="test various fit methods for the phase stepping curves.") 

commandline_parser$add_argument('-n',
            type='integer', nargs='?', default=10,
            help='number of curves per point')

args = commandline_parser$parse_args()

n = args$n

phase = 0
steps = 9

constants = c(
              seq(from=50, to=550, by=100),
              seq(from=1000, to=10000, by=1000),
              seq(from=20000, to=50000, by=10000)
              )

visibilities = c(
                 seq(from=0.01, to=0.20, by=0.01),
                 seq(from=0.25, to=0.55, by=0.05),
                 seq(from=0.55, to=0.95, by=0.10)
                 )


visibility_analysis = foreach(visibility=visibilities,
                              .combine=rbind) %dopar% {
    foreach(constant=c(1000, 10000, 50000),
            .combine=rbind) %do% {
        curve = phase_stepping_curve(constant, visibility, phase, steps)
        noisy_curves = get_noisy_curves(n, curve)
        analysed = analyse_fits(fit_curves(noisy_curves))
        analysed$true_constant = constant
        analysed$true_phase = phase
        analysed$true_visibility = visibility
        analysed
    }
}

constant_analysis = foreach(constant=constants,
                            .combine=rbind) %dopar% {
    foreach(visibility=c(0.05, 0.3, 0.9),
            .combine=rbind) %do% {
        curve = phase_stepping_curve(constant, visibility, phase, steps)
        noisy_curves = get_noisy_curves(n, curve)
        analysed = analyse_fits(fit_curves(noisy_curves))
        analysed$true_constant = constant
        analysed$true_phase = phase
        analysed$true_visibility = visibility
        analysed
    }
}

save(visibility_analysis, constant_analysis, file="data_analysis.rda")
