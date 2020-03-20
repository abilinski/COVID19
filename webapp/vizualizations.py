import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.exceptions import PreventUpdate

from dash.dependencies import Input, Output, State


class Vizualizations:
    def __init__(self, charts):
        self.charts = charts

    def render(self):
        html_menu = html.Div(
            className="ui tabular menu",
            children=[
                html.A(className="item", children=i, **{"data-tab": i})
                for i, chart in enumerate(self.charts)
            ],
        )

        html_tabs = [
            html.Div(className="ui tab", children=chart, **{"data-tab": i})
            for i, chart in enumerate(self.charts)
        ]

        return html.Div(className="ui", children=[html_menu] + html_tabs)
