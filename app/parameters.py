import functools

import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq


def __percent_selector_params():
    def percentify(value):
        return "{:.0%}".format(value)

    step_size = 0.05
    steps = [i * step_size for i in range(0, int(1 / step_size), 1)]

    mark_size = 0.10
    marks = [i * step_size for i in range(0, int(1 / mark_size), 1)]
    marks_dict = {step: percentify(step) for step in steps}

    return {"min": 0, "max": 1, "step": step_size, "value": 0.10, "marks": marks_dict}


def number_of_cases_input():
    return (
        "Number of Cases",
        "parameter-cases_n",
        dcc.Input,
        dict(id="cases-input", placeholder=0, type="number", min=0),
    )


def proportion_of_cases_slider():
    return (
        "Proportion of Cases Reported",
        "parameter-cases-reported",
        dcc.Slider,
        __percent_selector_params(),
    )


def school_closures_toggle():
    return (
        "School Closures",
        "parameter-schools-closed",
        #  daq.BooleanSwitch(on=False,),
        dcc.RadioItems,
        dict(
            options=[{"label": "On", "value": 1}, {"label": "Off", "value": 0},],
            value=0,
        ),
    )


def no_contact_zone(age_group):
    return (
        f"Proportion of {age_group} in no contact zone.",
        f"parameter-no-contact-{age_group}",
        dcc.Slider,
        __percent_selector_params(),
    )


def render_control(control):
    try:
        ctor = control.klass
    except AttributeError:
        ctor = control.klass

    component = ctor(**control.attrs)

    control = html.Div(
        id=control.selector, children=[html.P(control.label), component,],
    )

    return control


class ParameterControl:
    def __init__(self, label, selector, klass, attrs):
        self.label = label
        self.selector = selector
        self.klass = klass
        self.attrs = attrs

    # gets whether we look for "value", "on", etc.
    def val_string(self):
        if "value" in self.component_attr:
            return "value"
        elif "on" in self.component_attr:
            return "on"

    # changes value ('on' or 'value', etc.)
    def update_value(self, new_value):
        self.component_attr[self.val_string()] = new_value


class Parameters:
    PARAMETER_FUNCS = [
        number_of_cases_input,
        proportion_of_cases_slider,
        school_closures_toggle,
        functools.partial(no_contact_zone, "0-20"),
        functools.partial(no_contact_zone, "21-64"),
        functools.partial(no_contact_zone, "65+"),
    ]

    def __init__(self):
        self.parameter_controls = []

    def render(self):
        for f in Parameters.PARAMETER_FUNCS:
            attrs = f()
            ctrl = ParameterControl(*attrs)
            self.parameter_controls.append(ctrl)

        return html.Div(
            id="parameters",
            children=[render_control(c) for c in self.parameter_controls],
        )
