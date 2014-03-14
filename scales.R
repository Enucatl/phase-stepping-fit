library(ggplot2)

#set default scales for x/y then change when needed
set_default_scale <- function(statistical_tests, x_parameters, y_parameters, default_scale) {
    scales = list()
    for (statistical_test in statistical_tests) {
        scales[[statistical_test]] <- list()
        for (x_parameter in x_parameters) {
            scales[[statistical_test]][[x_parameter]] <- list()
            for (y_parameter in y_parameters) {
                scales[[
                    statistical_test]][[
                    x_parameter]][[y_parameter]] <- default_scale
            }
        }
    }
    return(scales)
}

x_scales_factory <- function(statistical_tests, x_parameters, y_parameters) {
    scales <- set_default_scale(
                      statistical_tests,
                      x_parameters,
                      y_parameters,
                      scale_x_log10)
    #customization
    for (x_parameter in x_parameters) {
        for (y_parameter in y_parameters) {
            scales[["sd"]][[x_parameter]][[y_parameter]] = scale_x_continuous
        }
    }
    return(scales)
}

y_scales_factory <- function(statistical_tests, x_parameters, y_parameters) {
    scales = set_default_scale(
                                 statistical_tests,
                                 x_parameters,
                                 y_parameters,
                                 scale_y_continuous)
    scales[["mean"]][["true_visibility"]][["visibility"]] = scale_y_log10
    scales[["median"]][["true_visibility"]][["visibility"]] = scale_y_log10
    scales[["sd"]][["true_constant"]][["phase"]] = scale_y_log10
    scales[["sd"]][["true_visibility"]][["phase"]] = scale_y_log10
    scales[["sd"]][["true_constant"]][["visibility"]] = scale_y_log10
    return(scales)
}
