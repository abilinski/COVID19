import dash_html_components as html
import dash_core_components as dcc


class Layout:
    def __init__(self):
        pass

    def render(
        self, parameter_renderer=None, submit_renderer=None, output_renderer=None
    ):

        parameters = parameter_renderer() if parameter_renderer else None
        submit = submit_renderer() if submit_renderer else None
        outputs = output_renderer() if output_renderer else None

        return html.Div(
            children=[
                html.Div(
                    id="banner",
                    className="ui fixed inverted menu",
                    children=[
                        html.Div(
                            className="ui container fluid",
                            children=[
                                html.Div(
                                    className="header item",
                                    children="C-SPEC: COVID-19 Science, Policy, and Epidemiology Collective",
                                )
                            ],
                        )
                    ],
                ),
                html.Div(
                    className="ui main container fluid",
                    children=[
                        html.Div(
                            className="ui padded grid divided stackable",
                            children=[
                                html.Div(
                                    className="row",
                                    children=[
                                        # Left column
                                        html.Div(
                                            className="four wide column",
                                            children=[
                                                html.H2("Parameters"),
                                                html.Div(
                                                    className="row",
                                                    children=[parameters],
                                                ),
                                                html.Div(
                                                    className="row",
                                                    children=[
                                                        html.Div(
                                                            className="ui basic",
                                                            children=submit,
                                                        )
                                                    ],
                                                ),
                                            ],
                                        ),
                                        # Right column
                                        html.Div(
                                            className="twelve wide column",
                                            children=outputs,
                                        ),
                                    ],
                                )
                            ],
                        )
                    ],
                ),
            ]
        )
