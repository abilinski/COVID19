import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.exceptions import PreventUpdate

from dash.dependencies import Input, Output, State

class Vizualizations:
    def __init__(self):
        pass

    def render(self):
        return html.Div(
        id="submit-status",
        title="Contains information about the success or failure of your commands.",
        children=["Test"],
    )
