import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq

from webapp.parameters.controls import InputControl, ToggleControl, PercentSliderControl


class ImplementerControls:
    CASES_N = InputControl(
        "Number of Cases",
        "parameter-cases-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="",
    )

    ICU_BEDS_N = InputControl(
        "Number of ICU Beds",
        "parameter-icu-beds-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="",
    )

    VENTILATORS_N = InputControl(
        "Number of Ventilators",
        "parameter-ventilators-n",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="",
    )

    CASES_REPORTED = PercentSliderControl(
        "Proportion of Cases Reported", "parameter-cases-reported"
    )

    SCHOOL_CLOSURES = ToggleControl(
        "School Closures",
        "parameter-schools-closed",
        dict(
            options=[{"label": "On", "value": 1}, {"label": "Off", "value": 0},],
            value=0,
        ),
        description="",
    )

    NO_CONTACT_0_20 = PercentSliderControl(
        f"Proportion of 0-20 in no contact zone.",
        f"parameter-no-contact-0-20",
        description="",
    )

    NO_CONTACT_21_59 = PercentSliderControl(
        f"Proportion of 21-59 in no contact zone.",
        f"parameter-no-contact-21-59",
        description="",
    )

    NO_CONTACT_60_PLUS = PercentSliderControl(
        f"Proportion of 60+ in no contact zone.",
        f"parameter-no-contact-60+",
        description="",
    )


class ImplementerParameters:
    NAME = "Basic"
    CONTROLS = [
        ImplementerControls.CASES_N,
        ImplementerControls.ICU_BEDS_N,
        ImplementerControls.VENTILATORS_N,
        ImplementerControls.CASES_REPORTED,
        ImplementerControls.SCHOOL_CLOSURES,
        ImplementerControls.NO_CONTACT_0_20,
        ImplementerControls.NO_CONTACT_21_59,
        ImplementerControls.NO_CONTACT_60_PLUS,
    ]
