import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq

from webapp.parameters.controls import InputControl, ToggleControl, PercentSliderControl


class QueuingControls:
    DELTA = InputControl(
        "Proportion presenting to ED",
        "parameter-queuing-delta",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    PHI = InputControl(
        "Recovery Rate",
        "parameter-queuing-phi",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_N= InputControl(
        "Mortaility Rate Not Presenting",
        "parameter-queuing-mun",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_P = InputControl(
        "Mortaility Rate Presenting",
        "parameter-queuing-mup",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_IW = InputControl(
        "Mortaility Rate Waiting for ICU",
        "parameter-queuing-muiw",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_I = InputControl(
        f"Mortaility Rate in ICU",
        f"parameter-queuing-mui",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_FW = InputControl(
        f"Mortality Rate Waiting for Bed",
        f"parameter-queuing-mufw",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    MU_F = InputControl(
        f"Mortality Rate in Bed",
        f"parameter-queuing-muf",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    SIGMA_FW = InputControl(
        f"Arrival Rate Waiting for Bed",
        f"parameter-queuing-sigmafw",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    SIGMA_IW = InputControl(
        f"Arrival Rate Waiting for ICU",
        f"parameter-queuing-sigmaiw",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    XM = InputControl(
        f"Duration in ICU",
        f"parameter-queuing-xm",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    XN = InputControl(
        f"Duration in Bed",
        f"parameter-queuing-xn",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    M = InputControl(
        f"Expected Available ICU Beds in US",
        f"parameter-queuing-m",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )

    N = InputControl(
        f"Expected Available non-ICU Beds in US",
        f"parameter-queuing-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description=None,
    )


class QueuingParameters:
    NAME = "Queuing"
    CONTROLS = [
        QueuingControls.DELTA,
        QueuingControls.PHI,
        QueuingControls.MU_N,
        QueuingControls.MU_P,
        QueuingControls.MU_IW,
        QueuingControls.MU_I,
        QueuingControls.MU_FW,
        QueuingControls.MU_F,
        QueuingControls.SIGMA_FW,
        QueuingControls.SIGMA_IW,
        QueuingControls.XM,
        QueuingControls.XN,
        QueuingControls.M,
        QueuingControls.N,
    ]

