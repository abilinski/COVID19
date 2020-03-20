import functools

import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq


class Control:
    def __init__(self, label, selector, builder, classes=None, attrs=None):
        self.label = label
        self.selector = selector
        self.builder = builder
        self.classes = classes
        self.attrs = attrs

    def render(self):
        component = self.builder(id=self.selector, **self.attrs)

        html_label = html.P(self.label)
        html_component = html.Div(className=self.classes, children=component)

        return html.Div(
            className="ui secondary segment", children=[html_label, html_component,],
        )

    def value_key(self):
        return "value"

    def update_value(self, new_value):
        raise Exception("Not yet implemented.")
        #  self.component_attr[self.value_key()] = new_value


class SliderControl(Control):
    def __init__(self, label, selector, attrs):
        super().__init__(label, selector, dcc.Slider, attrs=attrs)


class PercentSliderControl(SliderControl):
    def __init__(self, label, selector, default_value=0.10):
        super().__init__(
            label, selector, attrs=PercentSliderControl.__generate_attrs(default_value)
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
    def __init__(self, label, selector, attrs=None):
        super().__init__(
            label, selector, dcc.Input, classes="ui fluid input", attrs=attrs
        )


class ToggleControl(Control):
    def __init__(self, label, selector, attrs):
        super().__init__(
            label, selector, dcc.RadioItems, classes="ui radio checkbox", attrs=attrs
        )

    def value_key(self):
        return "value"


CONTROLS = [
    InputControl(
        "Number of Cases",
        "parameter-cases-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
    ),
    InputControl(
        "Number of ICU Beds",
        "parameter-icu-beds-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
    ),
    InputControl(
        "Number of Ventilators",
        "parameter-ventilators-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
    ),
    PercentSliderControl("Proportion of Cases Reported", "parameter-cases-reported"),
    ToggleControl(
        "School Closures",
        "parameter-schools-closed",
        dict(
            options=[{"label": "On", "value": 1}, {"label": "Off", "value": 0},],
            value=0,
        ),
    ),
    PercentSliderControl(
        f"Proportion of 0-20 in no contact zone.", f"parameter-no-contact-0-20",
    ),
    PercentSliderControl(
        f"Proportion of 21-59 in no contact zone.", f"parameter-no-contact-21-59",
    ),
    PercentSliderControl(
        f"Proportion of 60+ in no contact zone.", f"parameter-no-contact-60+",
    ),
]


class Parameters:
    def __init__(self, controls=CONTROLS):
        self.parameter_controls = controls

    def render(self):
        return html.Div(
            id="parameters", children=[c.render() for c in self.parameter_controls],
        )
