import functools

import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq


def __percent_selector_params():
    def percentify(value):
        print(value)
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
        dcc.Input(id="cases-input", placeholder=0, type="number", min=0),
    )


def proportion_of_cases_slider():
    return (
        "Proportion of Cases Reported",
        dcc.Slider(**__percent_selector_params()),
    )


def school_closures_toggle():
    return (
        "School Closures",
        #  daq.BooleanSwitch(on=False,),
        dcc.RadioItems(
        options=[{"label": "On", "value": 1}, {"label": "Off", "value": 0},],
        value=0,
        ),
    )


def no_contact_zone(age_group):
    return (
        f"Proportion of {age_group} in no contact zone.",
        dcc.Slider(**__percent_selector_params()),
    )


class Parameters:
    CONTROL_FUNCS = [
        number_of_cases_input,
        proportion_of_cases_slider,
        school_closures_toggle,
        functools.partial(no_contact_zone, "0-20"),
        functools.partial(no_contact_zone, "21-64"),
        functools.partial(no_contact_zone, "65+"),
    ]

    def __init__(self):
        pass

    def render(self):
        controls = [
            c for cf in Parameters.CONTROL_FUNCS for c in self._make_control(cf)
        ]
        return html.Div(id="parameters", children=controls)

    def _make_control(self, func):
        label, control = func()
        return [
            html.P(label),
            control,
            html.Br(),
        ]
