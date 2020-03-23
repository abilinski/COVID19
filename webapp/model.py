"""
Actually run the model
"""

import subprocess
import os
import csv
import tempfile

R_SCRIPT = "./1 - Model/Most recent/run_model.R"
DEFAULT_PARAMETERS = "./0 - Parameters/parameters_17_mar_2020.csv"
DEFAULT_SCENARIO = "Base case"

# Map of controls in app to parameter names in file
CONTROLS_TO_PARAMETERS_MAP = {
    "parameter-cases-n" : "n"
}

## TODO: Figure out mapping between controls:
# {'parameter-cases-n': 0, 'parameter-icu-beds-n': 0, 'parameter-ventilators-n': 0, 'parameter-cases-reported': 0.1, 'parameter-schools-closed': 0, 'parameter-no-contact-0-20': 0.1, 'parameter-no-contact-21-59': 0.1, 'parameter-no-contact-60+': 0.1
# and params
# Scenario,v11,v12,v13,v21,v22,v23,v31,v32,v33,s,e,p,R0,kappa,alpha1,alpha2,alpha3,epsilon,delta,gamma,m1,m2,m3,c,obs,e_ratio,k_report,k_inf,k_susp,young,medium,old,n


def get_default_parameters():
    with open(DEFAULT_PARAMETERS) as f:
        csvfile = csv.DictReader(f)
        for row in csvfile:
            if row.get('Scenario') == DEFAULT_SCENARIO:
                return row
    raise ValueError("Could not find default scenario %s" % (DEFAULT_SCENARIO))

def get_parameters(controls):

    ret = get_default_parameters()
    for control, value in controls.items():
        if control in CONTROLS_TO_PARAMETERS_MAP:
            ret[CONTROLS_TO_PARAMETERS_MAP[control]] = value
        else:
            # TODO: raise ValueError("Could not find mapping for %s", control)
            pass
    return ret

def write_parameters_csv(tempfile, parameters):

    with open(tempfile, 'w', newline='') as csvfile:
        fieldnames = list(parameters.keys())
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerow(parameters)


def run(controls):
    abspath = os.path.abspath(R_SCRIPT)

    parameters = get_parameters(controls)
    fp = tempfile.NamedTemporaryFile(delete=False)
    write_parameters_csv(fp.name, parameters)

    cmd = ["Rscript", abspath, "--parameters", fp.name]

    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    out, err = proc.communicate()
    os.unlink(fp.name)

    if proc.returncode != 0:
        raise ValueError("Error running Rscript: %s %s" % (err, out))
    out = out.decode("utf-8")
    err = err.decode("utf-8")
    csvfile = out.split("\n")
    reader = csv.DictReader(csvfile)
    result = list(reader)
    # Result is an ordered dict; remove this line to keep it that way
    result = [dict(x) for x in result]
    return result
