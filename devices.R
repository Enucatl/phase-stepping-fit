library(tikzDevice)
library(RSvgDevice)
library(SVGAnnotation)

devices <- list(
                pdf=pdf,
                pgf=tikz,
                svg=svg
                )

postprocessing_functions <- list(
                       pgf=function(file_name) {},
                       svg=function(file_name) {
                           doc = xmlParse(file_name)
                           addCSS(doc, css='css/plots.css')
                           saveXML(doc, file_name)
                       },
                       pdf=function(file_name) {
                           embed_fonts(file_name)
                       }
                       )
