import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq

from webapp.parameters.epi import EpiParameters
from webapp.parameters.implementer import ImplementerParameters
from webapp.parameters.queuing import QueuingParameters



def make_parameters():
    tab_implementer = ParameterTab(
        ImplementerParameters.NAME, ImplementerParameters.CONTROLS
    )

    tab_epi = ParameterTab(EpiParameters.NAME, EpiParameters.CONTROLS)

    tab_queuing = ParameterTab(QueuingParameters.NAME, QueuingParameters.CONTROLS)

    return Parameters(tab_implementer, tab_epi, tab_queuing)


class Parameters:
    def __init__(self, *args):
        self.parameter_groups = args

    def render(self):
        html_menu = [
            html.Div(
                className="ui tabular menu",
                children=[
                    html.A(className="item", children=pg.name, **{"data-tab": pg.name})
                    for i, pg in enumerate(self.parameter_groups)
                ],
            )
        ]

        html_tabs = [
            html.Div(className="ui tab", children=pg.render(), **{"data-tab": pg.name})
            for i, pg in enumerate(self.parameter_groups)
        ]

        return html.Div(className="ui", children=html_menu + html_tabs)

    def controls(self):
        for pg in self.parameter_groups:
            for c in pg.controls:
                yield c


class ParameterTab:
    def __init__(self, name, controls):
        self.name = name
        self.controls = controls

    def render(self):
        return html.Div(
            id=f"parameters-tab-{self.name.lower()}",
            children=[c.render() for c in self.controls],
        )
