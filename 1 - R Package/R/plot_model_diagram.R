#' @import DiagrammR
#' @export
render_model_diagram <- function() {

  grViz("
    digraph {

    graph [overlap = true, fontsize = 10, rankdir = LR]

    node [fontname = Helvetica, shape = box, fillcolor=CornflowerBlue, style=filled]

    S [label = 'Susceptible'];
    E [label = 'Exposed'];
    UA [label = 'Undetected\nAsymptomatic'];
    UI [label = 'Undetected\nSymptomatic'];
    {rank = same; UI; UA;}
    DA [label = 'Detected\nAsymptomatic'];
    DI [label = 'Detected\nSymptomatic'];
    R [label = 'Recovered'];
    D [label = 'Dead'];

    S->E E->UA E->UI UA->UI UA->DA UI->DI 
    UA->R UI->R DA->R DI->R UA->D UI->D
    DA->D DI->D

    }
    ")
}


