import dash
import dash_html_components as html
import dash_core_components as dcc

from dash.dependencies import Input, Output

from config import Config

from layout import Layout
from parameters import Parameters
from vizualizations import Vizualizations


app = dash.Dash(__name__)

app.layout = Layout().render(
    parameter_renderer=Parameters().render, output_renderer=Vizualizations().render
)

#  @app.callback(Output("tabs-content", "children"), [Input("tabs", "value")])
#  def render_content(tab):
#  if tab == "tab-1":
#  return html.Div([html.H3("Tab content 1")])
#  elif tab == "tab-2":
#  return html.Div([html.H3("Tab content 2")])


if __name__ == "__main__":
    app.run_server(debug=True)
