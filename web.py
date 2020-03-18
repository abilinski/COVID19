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
        [html.Button("Submit", id="submit-button", n_clicks=0)],
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
    [Input("submit-button", "n_clicks_timestamp")],
    state=[State(ctrl.selector, ctrl.get_value()) for ctrl in parameters.parameter_controls]
)
def calculate(n_clicks_timestamp, *state):
    if n_clicks_timestamp is None:
        raise PreventUpdate

    controls = parameters.parameter_controls
    # dictionary of commands; component id and associated value
    commands = {controls[i].selector: state[i] for i in range(len(controls))}

    #  failed, succeeded = spec.send_control_values(commands)
#
    summary = []

    summary.append("The following parameters were successfully updated: ")
    summary.append(html.Br())
    summary.append(html.Br())

    for key, value in commands.items():
        summary.append(f"{key.upper()}: {value}")
        summary.append(html.Br())


    return html.Div(summary)


if __name__ == "__main__":
    app.run_server(debug=Config.DEBUG)
