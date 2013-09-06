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
y_scales = list(
              mean=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_log10),
              sd=c(scale_y_log10, scale_y_log10,
                     scale_y_log10, scale_y_continuous),
              kurtosis=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_continuous),
              skewness=c(scale_y_continuous, scale_y_continuous,
                     scale_y_continuous, scale_y_continuous)
              )
x_scales = list(
              mean=c(scale_x_log10, scale_x_log10,
                     scale_x_log10, scale_x_log10),
              sd=c(scale_x_continuous, scale_x_continuous,
                     scale_x_continuous, scale_x_continuous),
              kurtosis=c(scale_x_log10, scale_x_log10,
                     scale_x_log10, scale_x_log10),
              skewness=c(scale_x_log10, scale_x_log10,
                     scale_x_log10, scale_x_log10)
              )


file_name = "graphs.pdf"
pdf(file_name,
    width=17, height=12)
for(statistical_test in names) {
    graph_phase_constant = ggplot(constant_analysis[J(statistical_test)],
                        aes(x=true_constant,
                            y=phase,
                            color=algorithm,
                            linetype=as.factor(true_visibility)
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_visibility)))
    graph_phase_constant = graph_phase_constant + y_scales[[statistical_test]][[1]](
                                name=sprintf("phase %s", statistical_test)
                                ) + scale_linetype_discrete(
                                name="visibility") +
                                x_scales[[statistical_test]][[1]](name="constant")
    graph_phase_visibility = ggplot(visibility_analysis[J(statistical_test)],
                        aes(x=true_visibility,
                            y=phase,
                            color=algorithm,
                            linetype=as.factor(true_constant),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_constant)))
    graph_phase_visibility = graph_phase_visibility + y_scales[[statistical_test]][[2]](
                                name=sprintf("phase %s", statistical_test)
                                ) + scale_linetype_discrete(name="constant"
                                ) + annotate(geom="text",
                                x=Inf, y=Inf, hjust=1, vjust=1, label=sprintf("
                                    Parameters for all the plots
                                    phase steps = %i
                                    phase value = %.2f
                                    simulated curves per point = %i
                                    poisson noise
                                ", steps, phase, n)) + x_scales[[statistical_test]][[2]](
                                name="visibility")   
    graph_visibility_constant = ggplot(constant_analysis[J(statistical_test)],
                        aes(x=true_constant,
                            y=visibility,
                            color=algorithm,
                            linetype=as.factor(true_visibility),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_visibility)))
    graph_visibility_constant = graph_visibility_constant + y_scales[[statistical_test]][[3]](
                                name=sprintf("visibility %s", statistical_test)
                                ) + scale_linetype_discrete(name="visibility") + x_scales[[statistical_test]][[3]](
                                name="constant")   
    graph_visibility_visibility = ggplot(visibility_analysis[J(statistical_test)],
                        aes(x=true_visibility,
                            y=visibility,
                            color=algorithm,
                            linetype=as.factor(true_constant),
                            )) + geom_line(
                        aes(group=interaction(algorithm, true_constant)))
    graph_visibility_visibility = graph_visibility_visibility + y_scales[[statistical_test]][[4]](
                                name=sprintf("visibility %s", statistical_test)
                                ) + scale_linetype_discrete(name="constant") + x_scales[[statistical_test]][[4]](
                                name="visibility")   

    graphs = arrangeGrob(
                graph_phase_constant, graph_phase_visibility,
                graph_visibility_constant, graph_visibility_visibility,
                nrow=2)
    print(graphs)
}
dev.off()
embed_fonts(file_name)
warnings()
