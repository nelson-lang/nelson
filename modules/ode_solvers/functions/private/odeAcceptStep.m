%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solverState = odeAcceptStep(problem, solverState, oldT, oldY, oldSlope, h, t, y, slope)
  if ~odeUsesSolverHistory(problem.solver)
    return
  end
  if ~odeStepReachedEndOfAttempt(problem, oldT, h, t)
    solverState = odeInitialSolverState(problem, t, y, slope);
    solverState.historyValid = false;
    return
  end
  solverState.t(end + 1) = t;
  solverState.y(:, end + 1) = y(:);
  solverState.slope(:, end + 1) = slope(:);
  maxHistory = odeMaxSolverHistory(problem.solver);
  if length(solverState.t) > maxHistory
    first = length(solverState.t) - maxHistory + 1;
    solverState.t = solverState.t(first:end);
    solverState.y = solverState.y(:, first:end);
    solverState.slope = solverState.slope(:, first:end);
  end
  solverState.historyValid = true;
end
%=============================================================================
function tf = odeUsesSolverHistory(solver)
  tf = strcmp(solver, 'ode113') || strcmp(solver, 'ode15s');
end
%=============================================================================
function maxHistory = odeMaxSolverHistory(solver)
  if strcmp(solver, 'ode15s')
    % BDF-5 needs five history points plus one more for the same-order
    % predictor.
    maxHistory = 6;
  else
    maxHistory = 5;
  end
end
%=============================================================================
function tf = odeStepReachedEndOfAttempt(problem, oldT, h, t)
  expectedT = oldT + h;
  tf = abs(t - expectedT) <= eps * (1 + abs(expectedT));
end
%=============================================================================
