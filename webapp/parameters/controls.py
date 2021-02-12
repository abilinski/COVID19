import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output, State

from webapp.core import Renderable


class Control(Renderable):
    def __init__(
        self, label, selector, builder, classes=None, description=None, attrs=None
    ):
        super().__init__()

        self.label = label
        self.selector = selector
        self.stat_selector = f"{selector}-stat"
        self.builder = builder
        self.classes = classes
        self.description = description
        self.attrs = attrs

    def render(self):
        children = []
        component = self.builder(id=self.selector, **self.attrs)

        html_icon = None
        if self.description is not None and len(self.description) != 0:
            html_icon = html.Span(
                className="ui icon parameter-info",
                **{
                    "data-tooltip": self.description,
                    "data-inverted": "",
                    "data-position": "top left",
                },
                children=html.I(className="info icon tiny parameter-icon"),
            )

        #  html_stat = html.Div(id=self.stat_selector, children=5)
        html_label = html.P(children=[self.label, html_icon]) #, html_stat])
        html_component = html.Div(className=self.classes, children=component)

        return html.Div(
            className="ui secondary segment", children=[html_label, html_component],
        )

    def register_callbacks(self, app):
        pass
        #  @app.callback(
            #  Output(component_id=self.stat_selector, component_property="children"),
            #  [Input(component_id=self.selector, component_property="value")],
        #  )
        #  def update_stat(input_value):
            #  return str(input_value)

    def value_key(self):
        return "value"

    def update_value(self, new_value):
        raise Exception("Not yet implemented.")
        #  self.component_attr[self.value_key()] = new_value


class SliderControl(Control):
    def __init__(self, label, selector, attrs, description=None):
        super().__init__(
            label, selector, dcc.Slider, attrs=attrs, description=description
        )


class PercentSliderControl(SliderControl):
    def __init__(self, label, selector, default_value=0.10, description=None):
        super().__init__(
            label,
            selector,
            attrs=PercentSliderControl.__generate_attrs(default_value),
            description=description,
        )

    @classmethod
    def __generate_attrs(cls, default_value=0.10):
        def percentify(value):
            return "{:.0%}".format(value)

        step_size = 0.05
        steps = [i * step_size for i in range(0, int(1 / step_size), 1)]

        mark_size = 0.10
        marks = [0] + [i * mark_size for i in range(0, int(1 / mark_size), 1)] + [1]
        marks_dict = {mark: percentify(mark) for mark in marks}

        return {
            "min": 0,
            "max": 1,
            "step": step_size,
            "value": default_value,
            "marks": marks_dict,
        }


class InputControl(Control):
    def __init__(self, label, selector, attrs=None, description=None):
        super().__init__(
            label,
            selector,
            dcc.Input,
            classes="ui fluid input",
            attrs=attrs,
            description=description,
        )


class ToggleControl(Control):
    def __init__(self, label, selector, attrs, description=None):
        super().__init__(
            label,
            selector,
            dcc.RadioItems,
            classes="ui radio checkbox",
            attrs=attrs,
            description=description,
        )

    def value_key(self):
        return "value"
