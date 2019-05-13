import subprocess
import numpy as np
import matplotlib.pyplot as plt

from BOCS import BOCS
from quad_mat import quad_mat
from sample_models import sample_models


def runBOCS(lrn, data):
	command = "Rscript"
	path2script = "test.R"


# Define command and arguments
command = 'Rscript'
path2script = '../../test.R'

# given a task, a learner, carry out BOCS

# Variable number of args in a list
args = ["1", "1", "1", "0"]

# Build subprocess command
cmd = [command, path2script]

# check_output will run the command and store to result
x = subprocess.check_output(cmd, universal_newlines=True)

# Save inputs in dictionary
inputs = {}
inputs['n_vars']     = 4
inputs['evalBudget'] = 20
inputs['n_init']     = 4
inputs['lambda']     = 1e-4

# Save objective function and regularization term
inputs['model']    = lambda x: float(subprocess.check_output(cmd + [str(i) for i in x], universal_newlines =True))

# compute x^TQx row-wise
inputs['penalty']  = lambda x: inputs['lambda']*np.sum(x, axis=1)

# Generate initial samples for statistical models
inputs['x_vals']   = sample_models(inputs['n_init'], inputs['n_vars'])
inputs['y_vals']   = np.array([inputs["model"](inputs["x_vals"][i]) for i in range(inputs['n_init'])])


