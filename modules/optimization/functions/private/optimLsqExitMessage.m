%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [message, messageDetailed] = optimLsqExitMessage(solverName, kind, info, opts)
  % Builds the expected exit messages of the least-squares
  % solvers. kind identifies the stopping reason, info carries the measured
  % value and the tolerance it was compared against.
  if strcmp(solverName, 'fsolve')
    [message, detail] = fsolveMessage(kind, info, opts);
  else
    [message, detail] = lsqnonlinMessage(solverName, kind, info, opts);
  end
  if isempty(detail)
    messageDetailed = message;
  else
    messageDetailed = sprintf('%s\n\n<stopping criteria details>\n\n%s', message, detail);
  end
end
%=============================================================================
function [message, detail] = lsqnonlinMessage(solverName, kind, info, opts)
  detail = '';
  switch kind
    case 'optimality'
      message = sprintf(_('Local minimum found.\n\nOptimization completed because the size of the gradient is less than\nthe value of the optimality tolerance.'));
      detail = sprintf(_('Optimization completed: The first-order optimality measure, %e,\nis less than options.OptimalityTolerance = %e.'), info.value, info.tolerance);
    case 'optimality-relative'
      message = sprintf(_('Local minimum found.\n\nOptimization completed because the size of the gradient is less than\n1e-4 times the value of the function tolerance.'));
      detail = sprintf(_('Optimization completed: The relative first-order optimality measure, %e,\nis less than 1e-4*options.FunctionTolerance = %e.'), info.value, info.tolerance);
    case 'step'
      message = sprintf(_('Local minimum possible.\n\n%s stopped because the size of the current step is less than\nthe value of the step size tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the norm of the current step, %e,\nis less than options.StepTolerance = %e.'), info.value, info.tolerance);
    case 'funchange'
      message = sprintf(_('Local minimum possible.\n\n%s stopped because the final change in the sum of squares relative to\nits initial value is less than the value of the function tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the relative sum of squares (r) is changing\nby less than options.FunctionTolerance = %e.'), info.tolerance);
    case 'direction'
      message = sprintf(_('Local minimum possible.\n\n%s stopped because the magnitude of the search direction is less than\nthe value of the step size tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the norm of the search direction, %e,\nis less than options.StepTolerance = %e.'), info.value, info.tolerance);
    case 'maxiter'
      message = sprintf(_('Solver stopped prematurely.\n\n%s stopped because it exceeded the iteration limit,\noptions.MaxIterations = %e.'), solverName, info.tolerance);
    case 'maxfeval'
      message = sprintf(_('Solver stopped prematurely.\n\n%s stopped because it exceeded the function evaluation limit,\noptions.MaxFunctionEvaluations = %e.'), solverName, info.tolerance);
    case 'outputfcn'
      message = sprintf(_('Solver stopped by an output function or plot function.'));
    case 'infeasible'
      message = sprintf(_('Exiting due to infeasibility: at least one lower bound exceeds the corresponding upper bound.'));
    case 'regularization'
      message = sprintf(_('No further progress possible.\n\n%s stopped because the regularization parameter of the Levenberg-Marquardt\nalgorithm became too large.'), solverName);
    otherwise
      message = sprintf(_('Solver stopped prematurely.'));
  end
end
%=============================================================================
function [message, detail] = fsolveMessage(kind, info, opts)
  detail = '';
  solverName = 'fsolve';
  switch kind
    case 'optimality'
      message = sprintf(_('Equation solved.\n\n%s completed because the vector of function values is near zero\nas measured by the value of the function tolerance, and\nthe problem appears regular as measured by the gradient.'), solverName);
      detail = sprintf(_('Equation solved: The sum of squared function values, r = %e,\nis less than options.FunctionTolerance = %e.'), info.value, info.tolerance);
    case 'step'
      message = sprintf(_('Equation solved, inaccuracy possible.\n\n%s stopped because the size of the current step is less than the value of\nthe step size tolerance and the vector of function values is near zero\nas measured by the value of the function tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the norm of the current step, %e,\nis less than options.StepTolerance = %e.'), info.value, info.tolerance);
    case 'funchange'
      message = sprintf(_('Equation solved, inaccuracy possible.\n\n%s stopped because the change in the sum of squared function values is less\nthan the value of the function tolerance and the vector of function values\nis near zero as measured by the value of the function tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the relative sum of squares (r) is changing\nby less than options.FunctionTolerance = %e.'), info.tolerance);
    case 'direction'
      message = sprintf(_('Equation solved, inaccuracy possible.\n\n%s stopped because the magnitude of the search direction is less than the\nvalue of the step size tolerance and the vector of function values is near\nzero as measured by the value of the function tolerance.'), solverName);
      detail = sprintf(_('Optimization stopped because the norm of the search direction, %e,\nis less than options.StepTolerance = %e.'), info.value, info.tolerance);
    case 'notsolved'
      message = sprintf(_('No solution found.\n\n%s stopped because the last step was ineffective and the vector of function\nvalues is not near zero as measured by the value of the function tolerance.'), solverName);
    case 'regularization'
      message = sprintf(_('No solution found.\n\n%s stopped because the trust region radius or the regularization parameter\nbecame too small, and the vector of function values is not near zero\nas measured by the value of the function tolerance.'), solverName);
    case 'maxiter'
      message = sprintf(_('Solver stopped prematurely.\n\n%s stopped because it exceeded the iteration limit,\noptions.MaxIterations = %e.'), solverName, info.tolerance);
    case 'maxfeval'
      message = sprintf(_('Solver stopped prematurely.\n\n%s stopped because it exceeded the function evaluation limit,\noptions.MaxFunctionEvaluations = %e.'), solverName, info.tolerance);
    case 'outputfcn'
      message = sprintf(_('Solver stopped by an output function or plot function.'));
    otherwise
      message = sprintf(_('Solver stopped prematurely.'));
  end
end
%=============================================================================
