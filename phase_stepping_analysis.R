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
    return(list(
                mean=mean(x),
                sd=sd(x),
                kurtosis=kurtosis(x),
                skewness=skewness(x),
                normality.p.value.lilliefors=lillie.test(x)$p.value,
                normality.p.value.shapiro=shapiro.test(x)$p.value))
}

names = c("mean", "sd", "kurtosis", "skewness",
          "normality.p.value.lilliefors",
          "normality.p.value.shapiro")
names = c(names, names)

analyse_fits = function(fit_df) {
    #get mean and standard deviation for the three parameters
    setkey(fit_df, type)
    result = fit_df[, lapply(.SD, statistical_tests), by=type]
    result$stat = names
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

constants = seq(from=100, to=20000, by=1000)
visibilities = seq(from=0.01, to=0.90, by=0.01)

visibility_analysis = foreach(visibility=visibilities,
                              .combine=rbind) %dopar% {
    foreach(constant=c(1000, 10000, 50000),
            .combine=rbind) %do% {
        curve = phase_stepping_curve(constant, visibility, phase, steps)
        noisy_curves = get_noisy_curves(n, curve)
        analysed = analyse_fits(fit_curves(noisy_curves))
        rbind(analysed,
              list(type="true",
                   constant=constant,
                   phase=phase,
                   visibility=visibility,
                   stat="mean"
                   ))
    }
}

constant_analysis = foreach(constant=constants,
                            .combine=rbind) %dopar% {
    foreach(visibility=c(0.05, 0.3, 0.9),
            .combine=rbind) %do% {
        curve = phase_stepping_curve(constant, visibility, phase, steps)
        noisy_curves = get_noisy_curves(n, curve)
        analysed = analyse_fits(fit_curves(noisy_curves))
        analysed$constant = constant
        analysed$visibility = visibility
        analysed$steps = steps
        analysed
    }
}

save(visibility_analysis, constant_analysis, file="data_analysis.rda")
