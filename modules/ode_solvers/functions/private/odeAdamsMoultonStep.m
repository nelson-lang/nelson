%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yhigh, ylow, evals] = odeAdamsMoultonStep(problem, t, y, h, solverState)
  if odeHasAdamsHistory(solverState, h, 5)
    [yhigh, ylow, evals] = odeAdamsMoultonFiveStep(problem, t, y, h, solverState);
  elseif odeHasAdamsHistory(solverState, h, 4)
    [yhigh, ylow, evals] = odeAdamsMoultonFourStep(problem, t, y, h, solverState);
  else
    [yhigh, ylow, evals] = odeDormandPrinceStep(problem, t, y, h);
    return
  end
end
%=============================================================================
function [yhigh, ylow, evals] = odeAdamsMoultonFourStep(problem, t, y, h, solverState)
  f = solverState.slope;
  n = size(f, 2);
  f0 = f(:, n);
  f1 = f(:, n - 1);
  f2 = f(:, n - 2);
  f3 = f(:, n - 3);
  ylow = y + h * (55 * f0 - 59 * f1 + 37 * f2 - 9 * f3) / 24;
  fpred = odeRhs(problem, t + h, ylow);
  yhigh = y + h * (9 * fpred + 19 * f0 - 5 * f1 + f2) / 24;
  evals = 1;
end
%=============================================================================
function [yhigh, ylow, evals] = odeAdamsMoultonFiveStep(problem, t, y, h, solverState)
  f = solverState.slope;
  n = size(f, 2);
  f0 = f(:, n);
  f1 = f(:, n - 1);
  f2 = f(:, n - 2);
  f3 = f(:, n - 3);
  f4 = f(:, n - 4);
  ylow = y + h * (1901 * f0 - 2774 * f1 + 2616 * f2 - 1274 * f3 + 251 * f4) / 720;
  fpred = odeRhs(problem, t + h, ylow);
  yhigh = y + h * (251 * fpred + 646 * f0 - 264 * f1 + 106 * f2 - 19 * f3) / 720;
  evals = 1;
end
%=============================================================================
function tf = odeHasAdamsHistory(solverState, h, count)
  tf = false;
  if ~isfield(solverState, 'historyValid') || ~solverState.historyValid
    return
  end
  if ~isfield(solverState, 't') || length(solverState.t) < count
    return
  end
  dt = diff(solverState.t);
  recent = dt(length(dt) - count + 2:length(dt));
  if any(abs(recent) <= eps)
    return
  end
  scale = max(abs(h), eps);
  tf = all(abs(recent - h) <= 0.25 * scale);
end
%=============================================================================
