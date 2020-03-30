
#' Function that renders a model diagram
#'
#' grViz from DiagrammeR returns an SVG diagram based on the user input provided.
#' 
#' It also provides a clean syntax to make sure that we will be able to update the 
#' model diagram easily in the future if needed. 
#' 
#' This is useful for rendering in our Shiny app too, as an SVG is lighter than an 
#' image file and will never have compression artifacts.
#' 
#' Documentation for grViz here: 
#' http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
#' 
#' @importFrom DiagrammeR grViz
plot_model_diagram <- function() {

  grViz("
    digraph {

    graph [overlap = true, fontsize = 10, rankdir = LR]

    node [fontname = Helvetica, shape = box, fillcolor=CornflowerBlue, style=filled]

    S [label = 'Susceptible'];
    E [label = 'Exposed'];
    'UA' [label = 'Undetected\nAsymptomatic'];
    UI [label = 'Undetected\nSymptomatic'];
    {rank = same; UI; UA;}
    DA [label = 'Detected\nAsymptomatic'];
    DI [label = 'Detected\nSymptomatic'];
    R [label = 'Recovered'];
    D [label = 'Dead'];

    S->E [label = 'λ'] 
    E->UA [label = 'δα']
    E->UI [label = 'δ(1-α)']
    UA->'DA' [label = 'd@_{a}']
    UI->'DI' [label = 'd@_{i}']
    UA->R [label = 'γ']
    UI->R [label = '(1-m)γ']
    DA->R [label = 'γ']
    DI->R [label = '(1-m)γ']
    UI->D [label = 'mω']
    DI->D [label = 'mω']

    }
    ")
}

#' Render the model diagram to a PDF file we can give users
#' @import DiagrammeRsvg rsvg
export_model_diagram_to_pdf <- function(output_file) {
  plot_model_diagram() %>% export_svg %>% charToRaw %>% rsvg_pdf(output_file)
}


#' Render the model diagram to a PNG file we can give users
#' @import DiagrammeRsvg rsvg
export_model_diagram_to_png <- function(output_file) {
  plot_model_diagram() %>% export_svg %>% charToRaw %>% rsvg_png(output_file)
}

