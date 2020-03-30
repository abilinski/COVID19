
#' Function that renders a model diagram
#'
#' 
#' 
#' @importFrom DiagrammeR grViz
#' @export
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


