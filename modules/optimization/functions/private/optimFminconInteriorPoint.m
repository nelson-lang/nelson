%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconInteriorPoint(data, opts, coreSolver)
  interior = data;
  weights = barrierSchedule(opts);
  warm = data.x0;
  barrierIterations = 0;
  for k = 1:length(weights)
    warm = interiorStart(data, opts, warm, weights(k));
    barrierIterations = barrierIterations + 1;
  end
  interior.x0 = warm;
  result = coreSolver(interior, opts);
  result = polishUnconverged(interior, opts, coreSolver, result);
  result.output.algorithm = 'interior-point';
  result.output.barrieriterations = barrierIterations;
  result = acceptFeasibleStationary(result, opts);
end
%=============================================================================
function result = polishUnconverged(data, opts, coreSolver, result)
  % a run out of iterations restarts once from its last point: the solver usually converges from there
  if result.exitflag ~= 0 || isempty(result.x)
    return
  end
  localData = data;
  localData.x0 = result.x;
  polished = coreSolver(localData, opts);
  feasible = polished.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  if feasible && (polished.exitflag > 0 || polished.fval <= result.fval)
    result = polished;
  end
end
%=============================================================================
function result = acceptFeasibleStationary(result, opts)
  if result.exitflag ~= 0
    return
  end
  feasible = result.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  stationary = result.output.firstorderopt <= max(2e-1, 10 * sqrt(opts.OptimalityTolerance));
  if feasible && stationary
    result.exitflag = 1;
    result.output.message = _('Local minimum found that satisfies the constraints.');
  end
end
%=============================================================================
function weights = barrierSchedule(opts)
  first = barrierWeight(opts);
  weights = first * [1, 0.1, 0.01];
  weights = weights(weights > 0);
end
%=============================================================================
function x = interiorStart(data, opts, x0, weight)
  x = min(max(x0, data.lb), data.ub);
  x = shiftBounds(x, data.lb, data.ub);
  if isempty(data.A) && isempty(data.Aeq)
    return
  end
  barrier = @(z) barrierObjective(data, z, opts, weight);
  searchOpts = optimset('Display', 'off', 'MaxIter', min(80, opts.MaxIter), ...
    'MaxFunEvals', min(800, opts.MaxFunEvals), 'TolX', opts.StepTolerance, ...
    'TolFun', opts.FunctionTolerance);
  [candidate, ~, exitflag] = fminsearch(barrier, x, searchOpts);
  if exitflag >= 0
    x = min(max(candidate(:), data.lb), data.ub);
  end
end
%=============================================================================
function x = shiftBounds(x, lb, ub)
  both = find(isfinite(lb) & isfinite(ub) & ub > lb);
  for k = both(:)'
    margin = min(1e-3 * max(1, ub(k) - lb(k)), 0.25 * (ub(k) - lb(k)));
    x(k) = min(max(x(k), lb(k) + margin), ub(k) - margin);
  end
  lowerOnly = find(isfinite(lb) & ~isfinite(ub));
  x(lowerOnly) = max(x(lowerOnly), lb(lowerOnly) + 1e-3);
  upperOnly = find(~isfinite(lb) & isfinite(ub));
  x(upperOnly) = min(x(upperOnly), ub(upperOnly) - 1e-3);
end
%=============================================================================
function value = barrierObjective(data, z, opts, weight)
  z = min(max(z(:), data.lb), data.ub);
  f = optimCheckFunValue(optimCall(data.fun, z), opts);
  if ~isnumeric(f) || ~isscalar(f)
    error('Nelson:optimization:invalidObjectiveScalar', '%s must return a numeric scalar.', 'Objective function');
  end
  slack = [data.b - data.A * z; z - data.lb; data.ub - z];
  finiteSlack = slack(isfinite(slack));
  bad = finiteSlack <= 0;
  penalty = sum(max(0, data.A * z - data.b) .^ 2) + sum((data.Aeq * z - data.beq) .^ 2);
  if any(bad)
    value = double(f) + 1e6 * (1 + sum(abs(finiteSlack(bad)))) + 1e8 * penalty;
  else
    value = double(f) - weight * sum(log(max(finiteSlack, realmin))) + 1e6 * penalty;
  end
end
%=============================================================================
function weight = barrierWeight(opts)
  if isfield(opts, 'InitBarrierParam') && ~isempty(opts.InitBarrierParam) && opts.InitBarrierParam > 0
    weight = opts.InitBarrierParam;
  else
    weight = 1e-3;
  end
  if isfield(opts, 'BarrierParamUpdate') && strcmpi(opts.BarrierParamUpdate, 'predictor-corrector')
    weight = 0.1 * weight;
  end
end
%=============================================================================
