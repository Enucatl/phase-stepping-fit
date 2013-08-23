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
graph_sd_phase_constant = ggplot(constant_analysis,
                       aes(x=constant,
                           y=sd_phase,
                           color=type,
                           linetype=factor(visibility)
                           )) + geom_line(
                       aes(group=interaction(type, visibility)))
graph_sd_phase_constant = graph_sd_phase_constant + scale_y_log10(
                            name="phase standard deviation"
                            ) + scale_linetype_discrete(name="visibility")
graph_sd_phase_visibility = ggplot(visibility_analysis,
                       aes(x=visibility,
                           y=sd_phase,
                           color=type,
                           linetype=factor(constant),
                           )) + geom_line(
                       aes(group=interaction(type, constant)))
graph_sd_phase_visibility = graph_sd_phase_visibility + scale_y_log10(
                            name="phase standard deviation"
                            ) + scale_linetype_discrete(name="constant"
                            ) + annotate(geom="text",
                            x=0.5, y=0.7, label=sprintf("
                            Parameters for all the plots
                            phase steps = %i
                            phase value = %.2f
                            simulated curves per point = 100000
                            poisson noise
                            ", 9, 0))
graph_sd_visibility_constant = ggplot(constant_analysis,
                       aes(x=constant,
                           y=sd_visibility,
                           color=type,
                           linetype=factor(visibility),
                           )) + geom_line(
                       aes(group=interaction(type, visibility)))
graph_sd_visibility_constant = graph_sd_visibility_constant + scale_y_log10(
                            name="visibility standard deviation"
                            ) + scale_linetype_discrete(name="visibility")
graph_sd_visibility_visibility = ggplot(visibility_analysis,
                       aes(x=visibility,
                           y=sd_visibility,
                           color=type,
                           linetype=factor(constant),
                           )) + geom_line(
                       aes(group=interaction(type, constant)))
graph_sd_visibility_visibility = graph_sd_visibility_visibility + scale_y_log10(
                            name="visibility standard deviation"
                            ) + scale_linetype_discrete(name="constant")

graphs = arrangeGrob(
            graph_sd_phase_constant, graph_sd_phase_visibility,
            graph_sd_visibility_constant, graph_sd_visibility_visibility,
            nrow=2)
x11(width=20, height=12)
print(graphs)
ggsave(sprintf("graphs%s.pdf", args$f), graphs)
message("Press Return To Continue")
invisible(readLines("stdin", n=1))
