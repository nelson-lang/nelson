%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [jac, evals] = odeFiniteDifferenceRhsJacobian(problem, t, y, f0)
  n = length(y);
  jac = zeros(n, n);
  pattern = odeJacobianPattern(problem, n);
  activeColumns = any(pattern, 1);
  if ~any(activeColumns)
    evals = 0;
    return
  end
  steps = zeros(n, 1);
  trials = y * ones(1, n);
  for j = 1:n
    if activeColumns(j)
      steps(j) = sqrt(eps) * max(abs(y(j)), 1);
      trials(j, j) = trials(j, j) + steps(j);
    end
  end
  if strcmpi(problem.options.Vectorized, 'on')
    trialColumns = find(activeColumns);
    ftrials = odeCallFcn(problem.odefun, t, trials(:, trialColumns), problem.options);
    if ~isempty(problem.options.Mass)
      mass = odeMassValue(problem, t, y);
      ftrials = mass \ ftrials;
    end
    for k = 1:length(trialColumns)
      j = trialColumns(k);
      jac(:, j) = (ftrials(:, k) - f0(:)) / steps(j);
    end
    evals = double(~isempty(trialColumns));
    jac(~pattern) = 0;
    return
  end
  for j = 1:n
    if activeColumns(j)
      ftrial = odeRhs(problem, t, trials(:, j));
      jac(:, j) = (ftrial(:) - f0(:)) / steps(j);
    end
  end
  evals = length(find(activeColumns));
  jac(~pattern) = 0;
end

function pattern = odeJacobianPattern(problem, n)
  pattern = problem.options.JPattern;
  if isempty(pattern)
    pattern = true(n, n);
  else
    pattern = pattern ~= 0;
  end
end

function mass = odeMassValue(problem, t, y)
  mass = problem.options.Mass;
  if isa(mass, 'function_handle')
    mass = odeCallFcn(mass, t, y, problem.options);
  end
end
%=============================================================================
