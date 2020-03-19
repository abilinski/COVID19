
"""
Actually run the model
"""

import subprocess
import os

R_SCRIPT="1 - Model/Most recent/model_3strat_18_mar_2020.R"


def run(parameters):

    abspath = os.path.abspath(R_SCRIPT)


    cmd = ["Rscript", abspath]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.returncode != 0:
        raise ValueError("Error running Rscript: %s %s" % (err, out))
    return out
