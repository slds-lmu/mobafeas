import sys
sys.path.insert(0, 'BOCS/BOCSpy')

import subprocess
import numpy as np

from BOCS import BOCS
from quad_mat import quad_mat
from sample_models import sample_models

def runBOCS(n_vars, n_init, eval_budget, sim_anneal, lamb):

	# Define command and arguments
	command = 'Rscript'
	path2script = "../../objective.R"

	# Build subprocess comman
	cmd = [command, path2script]

	# Save inputs in dictionar
	inputs = {}
	inputs['n_vars']     = n_vars
	inputs['evalBudget'] = eval_budget
	inputs['n_init']     = n_init
	inputs['lambda']     = lamb

	# Save objective function and regularization term
	inputs['model']    = lambda x: float(subprocess.check_output(cmd + [str(i) for i in x], universal_newlines =True))

	# compute x^TQx row-wise
	inputs['penalty']  = lambda x: inputs['lambda']*np.sum(x, axis=1)

	# Generate initial samples for statistical models
	inputs['x_vals']   = sample_models(inputs['n_init'], inputs['n_vars'])
	inputs['y_vals']   = np.array([inputs["model"](inputs["x_vals"][i]) for i in range(inputs['n_init'])])

	if sim_anneal:
		(BOCS_SA_model, BOCS_SA_obj)   = BOCS(inputs.copy(), 2, 'SA')
	else: # this does not work
		(BOCS_SA_model, BOCS_SA_obj)   = BOCS(inputs.copy(), 2, 'SDP-l1')

	return (inputs, BOCS_SA_model, BOCS_SA_obj)


