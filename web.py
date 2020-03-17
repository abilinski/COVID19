import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.exceptions import PreventUpdate

from dash.dependencies import Input, Output, State

from webapp.config import Config
from webapp.layout import Layout
from webapp.parameters import Parameters
from webapp.vizualizations import Vizualizations


app = dash.Dash(__name__)
server = app.server

parameters = Parameters()
vizualizations = Vizualizations()

def make_submit_control():
    return html.Div(
        [html.Button("Submit", id="submit-button")],
        #  title="Click to send all of the control values to the spectrometer.",
        #  className="control",
    )

app.layout = Layout().render(
    parameter_renderer=parameters.render,
    submit_renderer=make_submit_control,
    output_renderer=vizualizations.render,
)

@app.callback(
    Output("submit-status", "children"),
    [Input("submit-button", "n_clicks")],
    state=[State(ctrl.selector, ctrl.get_value()) for ctrl in parameters.parameter_controls]
)
def calculate(n_clicks, *state):
    if n_clicks is None:
        raise PreventUpdate

    print(state)
    return "Foo Bar Baz"


if __name__ == "__main__":
    app.run_server(debug=Config.DEBUG)
