import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.exceptions import PreventUpdate

from dash.dependencies import Input, Output, State

class Vizualizations:
    def __init__(self, charts):
        self.charts = charts

    def render(self):
        tabs = [dcc.Tab(label=f"Tab {i}", children=[chart]) for i, chart in  enumerate(self.charts)]
        return html.Div([
            dcc.Tabs(id="tabs", value='tab-1', children=tabs)
        ])
