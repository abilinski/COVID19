import dash
import dash_html_components as html
import dash_core_components as dcc

from dash.dependencies import Input, Output

from webapp.config import Config
from webapp.model_parameters import ModelParameters


app = dash.Dash(__name__, external_stylesheets=Config.STYLESHEET)

server = app.server



app.layout = html.Div([
    html.Span(
        [html.H1(children="Parameters")],
        id='parameters'
        ),
    html.Span([
        dcc.Tabs(id="tabs", value='tab-1', children=[
            dcc.Tab(label='Tab one', value='tab-1'),
            dcc.Tab(label='Tab two TEST', value='tab-2'),
            ]),
        html.Div(id='tabs-content')]
    )
])

@app.callback(Output('tabs-content', 'children'),
        [Input('tabs', 'value')])
def render_content(tab):
    if tab == 'tab-1':
        return html.Div([
            html.H3('Tab content 1')
            ])
    elif tab == 'tab-2':
        return html.Div([
            html.H3('Tab content 2')
            ])

if __name__ == "__main__":
    app.run_server(debug=True
