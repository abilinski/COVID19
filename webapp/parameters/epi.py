import dash_html_components as html
import dash_core_components as dcc
import dash_daq as daq

from webapp.parameters.controls import InputControl


# TODO a configuration file and a builder/factory
# pattern would be best here
class EpiControls:
    V11 = InputControl(
        "Group 1 Daily Contacts Base Case",
        "parameter-epi-v11",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v11: base case average daily contacts between susceptibles in group 1 and infected individuals",
    )

    V12 = InputControl(
        "Daily Group 1 Contact w/Group 2",
        "parameter-epi-v12",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v12: average daily contacts between susceptibles in group 1 and infected individuals in group 2",
    )

    V13 = InputControl(
        "Daily Group 1 Contact w/Group 3",
        "parameter-epi-v13",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v13: average daily contacts between susceptibles in group 1 and infected individuals in group 3",
    )

    V21 = InputControl(
        "Daily Group 2 Contact w/Group 1",
        "parameter-epi-v21",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v21: average daily contacts between susceptibles in group 2 and infected individuals in group 1",
    )

    V22 = InputControl(
        "Group 2 Daily Contacts Base Case",
        "parameter-epi-v22",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v22: base case average daily contacts between susceptibles in group 2 and infected individuals",
    )

    V23 = InputControl(
        "Daily Group 2 Contact w/Group 3",
        "parameter-epi-v23",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v23: average daily contacts between susceptibles in group 2 and infected individuals in group 3",
    )

    V31 = InputControl(
        "Daily Group 3 Contact w/Group 1",
        "parameter-epi-v31",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v31: average daily contacts between susceptibles in group 3 and infected individuals in group 1",
    )

    V32 = InputControl(
        "Daily Group 3 Contact w/Group 2",
        "parameter-epi-v32",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v32: average daily contacts between susceptibles in group 3 and infected individuals in group 2",
    )

    V33 = InputControl(
        "Group 3 Daily Contacts Base Case",
        "parameter-epi-v33",
        attrs=dict(placeholder=0, type="number", min=0, value=0),
        description="v33: base case average daily contacts between susceptibles in group 3 and infected individuals",
    )

    S = InputControl(
        "Percentage of Population Social Distancing",
        "parameter-epi-s",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="s: percentage of population in social distancing",
    )

    E = InputControl(
        "Proportion of Usual Contacts Social Distancing",
        "parameter-epi-e",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="e: proportion of usual contacts under social dist",
    )

    P = InputControl(
        "Probability of Transmission",
        "parameter-epi-p",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="p: probability of transmission given contact with infected person",
    )

    R0 = InputControl(
        "Reproductive Number",
        "parameter-epi-r",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="r: basic reproductive number",
    )

    KAPPA = InputControl(
        "Rate of Asymptomatic Transmission",
        "parameter-epi-kappa",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="kappa: relative rate of asymptomatic transmission",
    )

    ALPHA1 = InputControl(
        "Group 1 Asymptomatic",
        "parameter-epi-asymptomatic1",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="alpha1: proportion of cases in group 1 that do not go onto experience symptoms",
    )

    ALPHA2 = InputControl(
        "Group 2 Asymptomatic",
        "parameter-epi-asymptomatic2",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="alpha1: proportion of cases in group 2 that do not go onto experience symptoms",
    )

    ALPHA3 = InputControl(
        "Group 3 Asymptomatic",
        "parameter-epi-asymptomatic3",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="alpha1: proportion of cases in group 3 that do not go onto experience symptoms",
    )

    EPSILON = InputControl(
        "Average Pre-Symptomatic Infected Period",
        "parameter-epi-epislon",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="epislon: 1/average length of pre-symptomatic infected period",
    )

    DELTA = InputControl(
        "Average Incubation Period",
        "parameter-epi-delta",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="delta: 1/average length of incubation period",
    )

    GAMMA = InputControl(
        "Average Infection Duration",
        "parameter-epi-gamma",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="gamma: 1/average duration of infection",
    )

    C = InputControl(
        "Percentage of Observed Cases Baseline",
        "parameter-epi-c",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="c: percentage of cases observed at baseline",
    )

    OBS = InputControl(
        "Number of Observed Cases Baseline",
        "parameter-epi-obs",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="obs: number of cases observed at baseline",
    )

    E_RATIO = InputControl(
        "Ratio of Latent to Infected Baseline",
        "parameter-epi-e-ratio",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="e-ratio: ratio of latent group to infected group at baseline",
    )

    K_REPORT = InputControl(
        "Relative Kids Testing Rate",
        "parameter-epi-k-report",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="k-report: relative testing rate of kids",
    )

    K_INF = InputControl(
        "Relative Infectiousness of Kids",
        "parameter-epi-k-inf",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="k-inf: relative testing rate of kids",
    )

    K_SUSP = InputControl(
        "Relative Susceptibility of Kids",
        "parameter-epi-k-susp",
        attrs=dict(placeholder=0, type="number", min=0, value=1),
        description="k-susp: relative susceptibility of kids",
    )


class EpiParameters:
    NAME = "EPI"
    CONTROLS = [
        EpiControls.V11,
        EpiControls.V12,
        EpiControls.V13,
        EpiControls.V21,
        EpiControls.V22,
        EpiControls.V23,
        EpiControls.V31,
        EpiControls.V32,
        EpiControls.V33,
        EpiControls.S,
        EpiControls.E,
        EpiControls.P,
        EpiControls.R0,
        EpiControls.KAPPA,
        EpiControls.ALPHA1,
        EpiControls.ALPHA2,
        EpiControls.ALPHA3,
        EpiControls.EPSILON,
        EpiControls.DELTA,
        EpiControls.GAMMA,
        EpiControls.C,
        EpiControls.OBS,
        EpiControls.E_RATIO,
        EpiControls.K_REPORT,
        EpiControls.K_INF,
        EpiControls.K_SUSP,
    ]
