"""
Actually run the model
"""

import subprocess
import os
import csv

R_SCRIPT = "1 - Model/Most recent/run_model.R"


def run(parameters):
    abspath = os.path.abspath(R_SCRIPT)

    cmd = ["Rscript", abspath]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.returncode != 0:
        raise ValueError("Error running Rscript: %s %s" % (err, out))
    out = out.decode('utf-8')
    err = err.decode('utf-8')
    csvfile = out.split('\n')
    reader = csv.DictReader(csvfile)
    result = list(reader)
    # Result is an ordered dict; remove this line to keep it that way
    result = [dict(x) for x in result]
    return result
