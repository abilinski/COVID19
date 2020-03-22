import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.exceptions import PreventUpdate

from dash.dependencies import Input, Output, State
from webapp import model

from webapp.config import Config
from webapp.layout import Layout
from webapp.parameters import Parameters
from webapp.vizualizations import Vizualizations
import os

external_stylesheets = [
    "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
]
external_scripts = [
    "https://code.jquery.com/jquery-3.1.1.min.js",
    "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js",
]

app = dash.Dash(
    __name__,
    external_stylesheets=external_stylesheets,
    external_scripts=external_scripts,
)
server = app.server

chart1 = html.Div(
    id="submit-status",
    title="Contains information about the success or failure of your commands.",
    children=["Status should wind up here."],
)

chart2 = html.Div(children="Something else")

parameters = Parameters()
vizualizations = Vizualizations([chart1, chart2])


def make_submit_control():
    return html.Button(
        "Submit", id="submit-button", className="ui primary button fluid", n_clicks=0
    )


app.layout = Layout().render(
    parameter_renderer=parameters.render,
    submit_renderer=make_submit_control,
    output_renderer=vizualizations.render,
)


@server.route('/buildinfo')
def build_info(*args):
    if os.path.isfile("/buildinfo"):
        with open("/buildinfo") as f:
            return "Running in docker container: %s" % (f.read(),)
    return "Not running in docker"

@server.route('/model')
def run_model(*args):
    model_output = model.run({})
    return str(model_output)

@app.callback(
    Output("submit-status", "children"),
    [Input("submit-button", "n_clicks_timestamp")],
    state=[
        State(ctrl.selector, ctrl.value_key()) for ctrl in parameters.parameter_controls
    ],
)
def calculate(n_clicks_timestamp, *state):
    if n_clicks_timestamp is None:
        raise PreventUpdate

    controls = parameters.parameter_controls
    # dictionary of commands; component id and associated value
    commands = {controls[i].selector: state[i] for i in range(len(controls))}

    model_output = model.run(commands)

    summary = []

    summary.append("The following parameters were successfully updated: ")
    summary.append(html.Br())
    summary.append(html.Br())

    for key, value in commands.items():
        summary.append(f"{key.upper()}: {value}")
        summary.append(html.Br())

    #  summary.append("Model output: %s" % (model_output))
    #  summary.append(html.Br())

    return html.Div(summary)


if __name__ == "__main__":
    if os.environ.get("BIND_ALL_IPS"):
        host="0.0.0.0"
    else:
        host=None

    app.run_server(host=host, debug=Config.DEBUG)
