import dash_html_components as html
import dash_core_components as dcc


class Layout:
    def __init__(self):
        pass

    def render(self, parameter_renderer=None, output_renderer=None):

        parameters = parameter_renderer() if parameter_renderer else None
        outputs = output_renderer() if output_renderer else None

        return html.Div(
            id="app-container",
            children=[
                # Banner
                html.Div(id="banner", className="banner", children="Tester",),
                # Left column
                html.Div(
                    id="left-column", className="four columns", children=parameters,
                ),
                # Right column
                html.Div(
                    id="right-column", className="eight columns", children=outputs,
                ),
            ],
        )
