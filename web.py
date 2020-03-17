import dash
import dash_html_components as html
import dash_core_components as dcc

from dash.dependencies import Input, Output

from webapp.config import Config
from webapp.layout import Layout
from webapp.parameters import Parameters
from webapp.vizualizations import Vizualizations


app = dash.Dash(__name__)
server = app.server

def make_submit_control():
    return html.Div(
        [
            html.Button("Submit", id="submit-button")
        ],
        #  title="Click to send all of the control values to the spectrometer.",
        #  className="control",
    )

control_status = html.Div(
    id="submit-status",
    title="Contains information about the success or failure of your commands.",
    children=[""],
),

app.layout = Layout().render(
    parameter_renderer=Parameters().render,
    submit_renderer=make_submit_control,
    output_renderer=Vizualizations().render
)


if __name__ == "__main__":
    app.run_server(debug=Config.DEBUG)
