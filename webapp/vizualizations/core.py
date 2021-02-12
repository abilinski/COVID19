import dash
import dash_html_components as html
import dash_core_components as dcc


def make_vizualizations():
    tab_parameter_dump = VizualizationTab(
        "Parameters",
        html.Div(
            id="submit-status",
            title="Contains information about the success or failure of your commands.",
            children=["Status should wind up here."],
        ),
    )

    return Vizualizations(tab_parameter_dump)


class Vizualizations:
    def __init__(self, *args):
        self.vizualizations = args

    def render(self):
        html_menu = [
            html.Div(
                className="ui tabular menu",
                children=[
                    html.A(
                        className="item active" if i == 0 else "item",
                        children=viz.name,
                        **{"data-tab": viz.name},
                    )
                    for i, viz in enumerate(self.vizualizations)
                ],
            )
        ]

        html_tabs = [
            html.Div(
                className="ui tab active" if i == 0 else "ui tab",
                children=viz.render(),
                **{"data-tab": viz.name},
            )
            for i, viz in enumerate(self.vizualizations)
        ]

        return html.Div(className="ui", children=html_menu + html_tabs)


class VizualizationTab:
    def __init__(self, name, vizualization):
        self.name = name
        self.vizualization = vizualization

    def render(self):
        return html.Div(
            id=f"vizualizations-tab-{self.name.lower()}", children=self.vizualization
        )
