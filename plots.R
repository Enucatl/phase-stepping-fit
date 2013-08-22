#!/usr/bin/env Rscript

library(argparse)
library(gridExtra)
library(ggplot2)

commandline_parser = ArgumentParser(
        description="draw results for fit analysis") 

commandline_parser$add_argument('-f', '--file',
            type='character', nargs='?', default='data_analysis.rda')

args = commandline_parser$parse_args()
load(args$f)

alpha=0.2
graph_phase_constant = ggplot(constant_analysis,
                       aes(x=constant,
                           y=mean_phase,
                           color=type)) + geom_line()
graph_phase_constant = graph_phase_constant + geom_ribbon(
                        aes(x=constant,
                            ymin=mean_phase-sd_phase,
                            ymax=mean_phase+sd_phase,
                            linetype=NA,
                            fill=type,
                            ),
                            alpha=alpha,
                            )
graph_phase_visibility = ggplot(visibility_analysis,
                       aes(x=visibility,
                           y=mean_phase,
                           color=type)) + geom_line()
graph_phase_visibility = graph_phase_visibility + geom_ribbon(
                        aes(x=visibility,
                            ymin=mean_phase-sd_phase,
                            ymax=mean_phase+sd_phase,
                            linetype=NA,
                            fill=type
                            ), alpha=alpha) + scale_x_log10()
                    
graph_visibility_visibility = ggplot(visibility_analysis,
                       aes(x=visibility,
                           y=mean_visibility,
                           color=type)) + geom_line()
graph_visibility_visibility = graph_visibility_visibility + geom_ribbon(
                        aes(x=visibility,
                            ymin=mean_visibility-sd_visibility,
                            ymax=mean_visibility+sd_visibility,
                            linetype=NA,
                            fill=type
                            )
                        ,alpha=alpha) + scale_x_log10() + scale_y_log10()

graph_visibility_constant = ggplot(constant_analysis,
                       aes(x=constant,
                           y=mean_visibility,
                           color=type)) + geom_line()
graph_visibility_constant = graph_visibility_constant + geom_ribbon(
                        aes(x=constant,
                            ymin=mean_visibility-sd_visibility,
                            ymax=mean_visibility+sd_visibility,
                            linetype=NA,
                            fill=type
                            ),
                            alpha=alpha) + scale_y_log10()
graphs = arrangeGrob(
            graph_phase_constant, graph_phase_visibility,
            graph_visibility_constant, graph_visibility_visibility,
            nrow=2)
x11(width=20, height=10)
print(graphs)
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
