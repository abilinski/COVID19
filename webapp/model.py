
"""
Actually run the model
"""

import subprocess


def run(parameters):

    cmd = ["Rscript", "helloworld.R"]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.returncode != 0:
        raise ValueError("Error running Rscript: %s" % (err))
    return out
