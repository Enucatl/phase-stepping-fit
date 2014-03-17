#!/usr/bin/env Rscript

library(argparse)
library(ggplot2)
library(data.table)

commandline_parser = ArgumentParser(
        description="draw results for fit analysis") 

commandline_parser$add_argument('-f', '--file',
            type='character', nargs='?', default='data_analysis.rda',
            help='file with the data.tables')
#commandline_parser$add_argument('-o', '--output',
            #type='character', nargs='?', default='pgf',
            #'output format for the graphs')
commandline_parser$add_argument('-w', '--width',
            type='double', nargs='?', default=4.6,
            help='width of each graph (in)')
#commandline_parser$add_argument('-h', '--height',
            #type='double', nargs='?', default=2.3,
            #help='height of each graph (in)')

theme_set(theme_bw(base_size=11) + theme(
legend.key.size=unit(1, 'lines'),
text=element_text(face='plain', family='CM Roman'),
legend.title=element_text(face='plain'),
axis.line=element_line(color='black'),
axis.title.y=element_text(vjust=0.1),
axis.title.x=element_text(vjust=0.1),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.border = element_blank()
))

args = commandline_parser$parse_args()
format <- "pgf"
width <- args$w
height <- width * 0.618
source("scales.R")
source("devices.R")
load(args$f) #constant_analysis, visibility_analysis
setkeyv(constant_analysis, c("stat", "algorithm"))
setkeyv(visibility_analysis, c("stat", "algorithm"))

remove_true_string <- function(string) {
    return(gsub("true_", "", string))
}

phase_stepping_curves_plot <- function(
    dataset,
    statistical_test,
    x_parameter,
    y_parameter,
    linetype_factor,
    x_scale,
    y_scale) {
    plot <- ggplot(dataset[J(statistical_test)],
                aes_string(x=x_parameter,
                y=y_parameter,
                colour="algorithm",
                linetype=linetype_factor))
    plot <- plot + geom_line(
                aes_string(
                group=paste0("interaction(algorithm, ", linetype_factor, ")")))
    plot <- plot + y_scale(name=sprintf("reconstructed %s %s", y_parameter,
                                       statistical_test))
    plot <- plot + scale_linetype_discrete(
                name=remove_true_string(linetype_factor))
    plot <- plot + x_scale(name=remove_true_string(x_parameter))
    plot <- plot + scale_colour_discrete(
                name="algorithm",
                breaks=c("fft", "ls"),
                labels=c("unweighted", "weighted"))
    return(plot)
}

statistical_tests <- c("mean", "median", "sd", "kurtosis", "skewness")
y_parameters <- c("phase", "visibility")
x_parameters <- c("true_visibility", "true_constant")
datasets <- list(true_constant=constant_analysis[,
                    true_visibility:=as.factor(true_visibility)],
                true_visibility=visibility_analysis[,
                    true_constant:=as.factor(true_constant)])
linetype_factors <- list(true_constant="true_visibility",
                true_visibility="true_constant")

x_scales <- x_scales_factory(statistical_tests, x_parameters, y_parameters)
y_scales <- y_scales_factory(statistical_tests, x_parameters, y_parameters)

device <- devices[[format]]
postprocessing_function <- postprocessing_functions[[format]]

for(statistical_test in statistical_tests) {
    for (x_parameter in x_parameters) {
        for (y_parameter in y_parameters) {
            file_name <- sprintf("stats_%s_%s_%s.%s",
                                 y_parameter,
                                 remove_true_string(x_parameter),
                                 statistical_test,
                                 format)
            device(file_name, width=width, height=height)
            dataset <- datasets[[x_parameter]]
            linetype_factor <- linetype_factors[[x_parameter]]
            x_scale <- x_scales[[statistical_test]][[x_parameter]][[y_parameter]]
            y_scale <- y_scales[[statistical_test]][[x_parameter]][[y_parameter]]
            graph <- phase_stepping_curves_plot(dataset, statistical_test,
                                                x_parameter, y_parameter,
                                                linetype_factor,
                                                x_scale, y_scale)
            print(graph)
            dev.off()
            postprocessing_function(file_name)
        }
    }
}

print(
      sprintf("
              Parameters for all the plots
              phase steps = %i
              phase value = %.2f
              simulated curves per point = %i
              poisson noise
              ", steps, phase, n))
warnings()
