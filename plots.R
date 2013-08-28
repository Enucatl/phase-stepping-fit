#!/usr/bin/env Rscript

library(argparse)
library(gridExtra)
library(ggplot2)
library(data.table)

commandline_parser = ArgumentParser(
        description="draw results for fit analysis") 

commandline_parser$add_argument('-f', '--file',
            type='character', nargs='?', default='data_analysis.rda')

args = commandline_parser$parse_args()
load(args$f) #constant_analysis, visibility_analysis
setkeyv(constant_analysis, c("stat", "algorithm"))
setkeyv(visibility_analysis, c("stat", "algorithm"))

names = c("mean", "sd", "kurtosis", "skewness")
scales = list(
              mean=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_continuous),
              sd=c(scale_y_log10, scale_y_log10,
                     scale_y_log10, scale_y_continuous),
              kurtosis=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_continuous),
              skewness=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_continuous)
              )


for(statistical_test in names) {
    graph_phase_constant = ggplot(constant_analysis[J(statistical_test)],
                        aes(x=true_constant,
                            y=phase,
                            color=algorithm,
                            linetype=as.factor(true_visibility)
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_visibility)))
    graph_phase_constant = graph_phase_constant + scales[[statistical_test]][[1]](
                                name=sprintf("phase %s", statistical_test)
                                ) + scale_linetype_discrete(
                                name="visibility") +
                                scale_x_continuous(name="constant")
    graph_phase_visibility = ggplot(visibility_analysis[J(statistical_test)],
                        aes(x=true_visibility,
                            y=phase,
                            color=algorithm,
                            linetype=as.factor(true_constant),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_constant)))
    graph_phase_visibility = graph_phase_visibility + scales[[statistical_test]][[2]](
                                name=sprintf("phase %s", statistical_test)
                                ) + scale_linetype_discrete(name="constant"
                                ) + annotate(geom="text",
                                x=Inf, y=Inf, hjust=1, vjust=1, label=sprintf("
                                    Parameters for all the plots
                                    phase steps = %i
                                    phase value = %.2f
                                    simulated curves per point = %i
                                    poisson noise
                                ", steps, phase, n)) + scale_x_continuous(
                                name="visibility")   
    graph_visibility_constant = ggplot(constant_analysis[J(statistical_test)],
                        aes(x=true_constant,
                            y=visibility,
                            color=algorithm,
                            linetype=as.factor(true_visibility),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_visibility)))
    graph_visibility_constant = graph_visibility_constant + scales[[statistical_test]][[3]](
                                name=sprintf("visibility %s", statistical_test)
                                ) + scale_linetype_discrete(name="visibility") + scale_x_continuous(
                                name="constant")   
    graph_visibility_visibility = ggplot(visibility_analysis[J(statistical_test)],
                        aes(x=true_visibility,
                            y=visibility,
                            color=algorithm,
                            linetype=as.factor(true_constant),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_constant)))
    graph_visibility_visibility = graph_visibility_visibility + scales[[statistical_test]][[4]](
                                name=sprintf("visibility %s", statistical_test)
                                ) + scale_linetype_discrete(name="constant")    + scale_x_continuous(
                                name="visibility")   

    graphs = arrangeGrob(
                graph_phase_constant, graph_phase_visibility,
                graph_visibility_constant, graph_visibility_visibility,
                nrow=2)
    x11(width=20, height=12)
    print(graphs)
    ggsave(sprintf("graphs_%s_%s.pdf", args$f, statistical_test), graphs)
}
warnings()
#message("Press Return To Continue")
#invisible(readLines("stdin", n=1))
