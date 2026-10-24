%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconSQPLegacy(data, opts)
  opts = legacyOptions(opts);
  result = legacyCore(data, opts);
  result.output.algorithm = 'sqp-legacy';
end
%=============================================================================
function opts = legacyOptions(opts)
  opts.SubproblemAlgorithm = 'active-set';
  opts.HessUpdate = 'bfgs';
  opts.EnableFeasibilityMode = false;
  opts.FiniteDifferenceType = 'forward';
  opts.MaxIter = max(1, min(opts.MaxIter, 200));
  opts.MaxIterations = opts.MaxIter;
end
%=============================================================================
function result = legacyCore(data, opts)
  x = min(max(data.x0, data.lb), data.ub);
  H = optimFminconInitialHessian(x, opts);
  [state, funcCount] = optimFminconState(data, x, opts);
  best = legacyBest([], x, state, opts);
  exitflag = 0;
  message = _('Maximum number of iterations or function evaluations reached.');
  iter = 0;
  for iter = 1:opts.MaxIter
    [step, qpLambda] = legacyStep(H, state, opts);
    if norm(step, Inf) <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    [trial, trialState, accepted, alpha, added] = legacyLineSearch(data, opts, x, state, step);
    funcCount = funcCount + added;
    if ~accepted
      exitflag = 0; message = 'Line search failed to find a lower merit value.'; break
    end
    s = trial - x;
    y = trialState.grad - state.grad;
    H = legacyHessian(H, s, y);
    x = trial;
    state = trialState;
    best = legacyBest(best, x, state, opts);
    if state.violation <= opts.ConstraintTolerance && legacyOptimality(state, qpLambda) <= opts.OptimalityTolerance
      exitflag = 1; message = _('Local minimum found that satisfies the constraints.'); break
    end
    if alpha * norm(step, Inf) <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    if funcCount >= opts.MaxFunEvals
      break
    end
  end
  lambda = legacyLambda(data, state, H, opts);
  if exitflag == 0 && state.violation <= opts.ConstraintTolerance && ~isempty(best)
    exitflag = 1;
    message = _('Local minimum found that satisfies the constraints.');
  end
  result.x = x;
  result.fval = state.f;
  result.exitflag = exitflag;
  result.lambda = lambda;
  result.grad = state.grad;
  result.hessian = H;
  result.output = legacyOutput(iter, funcCount, state, data, message, best, norm(x - data.x0, Inf));
end
%=============================================================================
function [step, lambda] = legacyStep(H, state, opts)
  n = length(state.grad);
  maxIter = max(10, 3 * n + size(state.Jineq, 1) + size(state.Jeq, 1));
  localOpts = opts;
  localOpts.ScaleProblem = 'obj-and-constr';
  [step, lambda] = optimFminconScaledQP(H + 1e-8 * eye(n), state, localOpts, maxIter);
  limit = max(1, norm(state.grad, Inf));
  scale = max(abs(step) / limit);
  if scale > 1
    step = step / scale;
  end
end
%=============================================================================
function [trial, trialState, accepted, alpha, count] = legacyLineSearch(data, opts, x, state, step)
  accepted = false;
  alpha = 1;
  count = 0;
  trial = x;
  trialState = state;
  base = legacyMerit(state);
  for k = 1:30
    candidate = min(max(x + alpha * step, data.lb), data.ub);
    [candidateState, added] = optimFminconState(data, candidate, opts);
    count = count + added;
    if legacyMerit(candidateState) <= base - 1e-5 * alpha * max(1, norm(step) ^ 2)
      trial = candidate;
      trialState = candidateState;
      accepted = true;
      return
    end
    alpha = alpha / 2;
  end
end
%=============================================================================
function H = legacyHessian(H, s, y)
  if s' * y <= 1e-10 * max(1, norm(s) * norm(y))
    H = 0.5 * H + 0.5 * eye(size(H));
    return
  end
  Hs = H * s;
  H = H - (Hs * Hs') / max(s' * Hs, eps) + (y * y') / (s' * y);
  H = (H + H') / 2;
end
%=============================================================================
function value = legacyMerit(state)
  value = state.f + (25 + norm(state.grad, Inf)) * (sum(max(0, state.cineq)) + sum(abs(state.ceq)));
end
%=============================================================================
function value = legacyOptimality(state, lambda)
  value = norm(state.grad, Inf);
  if isfield(lambda, 'ineqlin')
    active = state.cineq >= -sqrt(eps);
    if any(active)
      value = min(value, norm(state.grad + state.Jineq(active, :)' * ones(nnz(active), 1), Inf));
    end
  end
end
%=============================================================================
function lambda = legacyLambda(data, state, H, opts)
  active = find(state.cineq >= -opts.ConstraintTolerance);
  W = [state.Jeq; state.Jineq(active, :)];
  if isempty(W)
    multipliers = zeros(0, 1);
  else
    K = [H, W'; W, zeros(size(W, 1))];
    sol = legacySolve(K, -[state.grad; zeros(size(W, 1), 1)]);
    multipliers = sol(length(state.grad) + 1:end);
  end
  lambda = splitLambda(data, state, active, multipliers);
end
%=============================================================================
function lambda = splitLambda(data, state, active, multipliers)
  n = length(state.grad);
  neq = size(state.Jeq, 1);
  eq = slice(multipliers, 1, neq);
  ineq = zeros(size(state.cineq));
  ineq(active) = max(0, slice(multipliers, neq + 1, length(multipliers)));
  nLin = size(data.A, 1);
  nEqLin = size(data.Aeq, 1);
  nNon = length(state.cnonlin);
  lambda.eqlin = slice(eq, 1, nEqLin);
  lambda.eqnonlin = slice(eq, nEqLin + 1, length(eq));
  lambda.ineqlin = slice(ineq, 1, nLin);
  lambda.ineqnonlin = slice(ineq, nLin + 1, nLin + nNon);
  lowerStart = nLin + nNon + 1;
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.lower(data.lowerIndex) = slice(ineq, lowerStart, lowerStart + length(data.lowerIndex) - 1);
  lambda.upper(data.upperIndex) = slice(ineq, lowerStart + length(data.lowerIndex), ...
    lowerStart + length(data.lowerIndex) + length(data.upperIndex) - 1);
end
%=============================================================================
function value = slice(x, first, last)
  if last < first
    value = zeros(0, 1);
  else
    value = x(first:last);
  end
end
%=============================================================================
function best = legacyBest(best, x, state, opts)
  if state.violation > opts.ConstraintTolerance
    return
  end
  if isempty(best) || state.f < best.fval
    best.x = x;
    best.fval = state.f;
    best.firstorderopt = norm(state.grad, Inf);
    best.constrviolation = state.violation;
  end
end
%=============================================================================
function output = legacyOutput(iter, funcCount, state, data, message, best, stepSize)
  output.iterations = iter;
  output.funcCount = funcCount;
  output.constrviolation = state.violation;
  output.stepsize = stepSize;
  output.algorithm = data.algorithm;
  output.firstorderopt = norm(state.grad, Inf);
  output.cgiterations = 0;
  output.message = message;
  output.bestfeasible = best;
end
%=============================================================================
function x = legacySolve(A, b)
  try
    x = A \ b;
    if all(isfinite(x))
      return
    end
  catch
  end
  x = pinv(A) * b;
end
%=============================================================================
