

makeMBFControl <- function(propose.points = 1) {
  # TODO: propose point method
  # TODO: give infill crit
  ctrl <- makeMBOControl(n.objectives = 1, propose.points = propose.points, final.evals = 0, y.name = "y")

}

# infill crits take values, model predictions, model SEs, nugget info, residual variance info
# this is then integrated over the pareto fron
# (i.e. EI with points with same FF + (EI with points with same FF + points with one more feature) + (EI with points with same FF + pts with one or two more features))


