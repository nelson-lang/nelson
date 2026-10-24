%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconActiveSet(data, opts)
  opts = activeSetOptions(opts);
  data.x0 = activeSetStart(data, opts);
  activeRows = initialLinearActiveRows(data, opts);
  result = activeSetCore(data, opts, activeRows);
  result.output.algorithm = 'active-set';
  result.output.activeconstraints = activeRows(:);
end
%=============================================================================
function opts = activeSetOptions(opts)
  opts.HessianApproximation = 'bfgs';
  opts.SubproblemAlgorithm = 'active-set';
  opts.HonorBounds = true;
end
%=============================================================================
function x = activeSetStart(data, opts)
  x = min(max(data.x0, data.lb), data.ub);
  for k = 1:min(20, max(1, opts.MaxIter))
    old = x;
    x = projectEqualities(x, data.Aeq, data.beq);
    violated = data.A * x - data.b;
    rows = find(violated > opts.ConstraintTolerance);
    for row = rows(:)'
      a = data.A(row, :)';
      denom = max(a' * a, eps);
      x = x - ((a' * x - data.b(row)) / denom) * a;
      x = min(max(x, data.lb), data.ub);
    end
    if norm(x - old, Inf) <= opts.StepTolerance
      break
    end
  end
end
%=============================================================================
function activeRows = initialLinearActiveRows(data, opts)
  activeRows = [];
  if isempty(data.A)
    return
  end
  residual = data.A * data.x0 - data.b;
  activeRows = find(abs(residual) <= max(opts.ConstraintTolerance, sqrt(eps)));
end
%=============================================================================
function result = activeSetCore(data, opts, initialRows)
  x = data.x0;
  H = optimFminconInitialHessian(x, opts);
  working = initialWorkingSet(data, x, opts, initialRows);
  [state, funcCount] = optimFminconState(data, x, opts);
  best = feasibleBest([], x, state, opts);
  exitflag = 0;
  message = _('Maximum number of iterations or function evaluations reached.');
  iter = 0;
  released = [];
  for iter = 1:opts.MaxIter
    [step, multipliers] = activeStep(H, state, working, opts);
    if norm(step, Inf) <= opts.StepTolerance
      [drop, negative] = removableConstraint(multipliers, size(state.Jeq, 1), working, released);
      if isempty(drop)
        if state.violation <= opts.ConstraintTolerance && ~negative
          exitflag = 1; message = _('Local minimum found that satisfies the constraints.');
        else
          exitflag = 2; message = _('Step size tolerance reached.');
        end
        break
      end
      working(drop == working) = [];
      released(end + 1) = drop;
      continue
    end
    [trial, trialState, accepted, alpha, added] = activeLineSearch(data, opts, x, state, step, working);
    funcCount = funcCount + added;
    if ~accepted
      exitflag = 0; message = 'Line search failed to find a lower merit value.'; break
    end
    moved = norm(trial - x, Inf) > opts.StepTolerance;
    s = trial - x;
    y = trialState.grad - state.grad;
    H = activeBfgs(H, s, y);
    x = trial;
    state = trialState;
    working = updateWorkingSet(state, working, opts);
    best = feasibleBest(best, x, state, opts);
    if moved
      released = [];
    end
    if state.violation <= opts.ConstraintTolerance && activeOptimality(state, working) <= opts.OptimalityTolerance
      [drop, negative] = negativeMultiplierConstraint(state, working, H, released);
      if isempty(drop)
        if negative
          exitflag = 2; message = _('Step size tolerance reached.');
        else
          exitflag = 1; message = _('Local minimum found that satisfies the constraints.');
        end
        break
      end
      working(drop == working) = [];
      released(end + 1) = drop;
      continue
    end
    if abs(alpha) * norm(step, Inf) <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    if funcCount >= opts.MaxFunEvals
      break
    end
  end
  lambda = activeLambda(data, state, working, H);
  firstorderopt = activeOptimality(state, working);
  if exitflag == 0 && state.violation <= max(opts.ConstraintTolerance, 1e-5) ...
      && firstorderopt <= max(2e-1, 20 * sqrt(opts.OptimalityTolerance)) && ~isempty(best) ...
      && isempty(negativeMultiplierConstraint(state, working, H, []))
    exitflag = 1;
    message = _('Local minimum found that satisfies the constraints.');
  end
  result.x = x;
  result.fval = state.f;
  result.exitflag = exitflag;
  result.lambda = lambda;
  result.grad = state.grad;
  result.hessian = H;
  result.output = activeOutput(iter, funcCount, state, data, message, best, norm(x - data.x0, Inf), firstorderopt);
end
%=============================================================================
function working = initialWorkingSet(data, x, opts, initialRows)
  [state, ~] = optimFminconState(data, x, opts);
  linearRows = initialRows(:)';
  nonlinearOffset = size(data.A, 1);
  nonlinearRows = find(state.cnonlin >= -opts.ConstraintTolerance)' + nonlinearOffset;
  boundOffset = nonlinearOffset + length(state.cnonlin);
  boundRows = find(state.cineq(boundOffset + 1:end) >= -opts.ConstraintTolerance)' + boundOffset;
  working = unique([linearRows, nonlinearRows, boundRows], 'stable');
end
%=============================================================================
function [step, multipliers] = activeStep(H, state, working, opts)
  localState = state;
  inactive = setdiff(1:length(state.cineq), working);
  localState.Jineq = state.Jineq([working(:); inactive(:)], :);
  localState.cineq = state.cineq([working(:); inactive(:)]);
  maxIter = max(20, 5 * length(state.grad) + size(localState.Jineq, 1) + size(state.Jeq, 1));
  [step, lambda] = optimFminconScaledQP(H, localState, optimActiveOptions(opts), maxIter);
  multipliers = [lambda.eqlin; lambda.ineqlin(1:length(working))];
end
%=============================================================================
function opts = optimActiveOptions(opts)
  opts.Display = 'off';
  opts.ScaleProblem = 'obj-and-constr';
end
%=============================================================================
function [drop, negative] = removableConstraint(multipliers, neq, working, forbidden)
  values = multipliers(neq + 1:end);
  [drop, negative] = pickDroppable(values, working, forbidden);
end
%=============================================================================
function [drop, negative] = negativeMultiplierConstraint(state, working, H, forbidden)
  % Return the working inequality whose Lagrange multiplier is most negative
  % (and therefore should be released), or [] when every active inequality
  % has a non-negative multiplier. Stationarity alone (activeOptimality) is
  % not sufficient for a KKT point: an active inequality with a negative
  % multiplier signals that leaving it lets the objective decrease further.
  % negative reports whether any active inequality is a drop candidate even
  % when all such candidates are excluded by the anti-cycling guard.
  drop = [];
  negative = false;
  if isempty(working)
    return
  end
  n = length(state.grad);
  neq = size(state.Jeq, 1);
  W = [state.Jeq; state.Jineq(working, :)];
  K = [H, W'; W, zeros(size(W, 1))];
  sol = solveSystem(K, -[state.grad; zeros(size(W, 1), 1)]);
  multipliers = sol(n + 1:end);
  ineq = multipliers(neq + 1:end);
  [drop, negative] = pickDroppable(ineq, working, forbidden);
end
%=============================================================================
function [drop, negative] = pickDroppable(values, working, forbidden)
  % Bland-style selection: consider the active inequalities from the most
  % negative multiplier upwards and release the first one that has not
  % already been released since the last productive step. Skipping the
  % forbidden set prevents the drop/re-add cycling that a degenerate vertex
  % can otherwise trigger. negative is true as soon as a genuine drop
  % candidate exists, regardless of whether it is currently forbidden.
  drop = [];
  negative = false;
  [sorted, order] = sort(values(:), 'ascend');
  for t = 1:numel(sorted)
    if sorted(t) >= -sqrt(eps)
      break
    end
    negative = true;
    candidate = working(order(t));
    if ~any(candidate == forbidden)
      drop = candidate;
      return
    end
  end
end
%=============================================================================
function [trial, trialState, accepted, alpha, count] = activeLineSearch(data, opts, x, state, step, working)
  alpha = maxFeasibleAlpha(state, step, working);
  alpha = min(1, max(alpha, 0));
  base = activeMerit(state);
  trial = x;
  trialState = state;
  accepted = false;
  count = 0;
  for k = 1:25
    candidate = min(max(x + alpha * step, data.lb), data.ub);
    [candidateState, added] = optimFminconState(data, candidate, opts);
    count = count + added;
    if activeMerit(candidateState) <= base - 1e-4 * alpha * max(1, norm(step) ^ 2)
      trial = candidate;
      trialState = candidateState;
      accepted = true;
      return
    end
    alpha = alpha / 2;
  end
end
%=============================================================================
function alpha = maxFeasibleAlpha(state, step, working)
  alpha = 1;
  inactive = setdiff(1:length(state.cineq), working);
  if isempty(inactive)
    return
  end
  derivative = state.Jineq(inactive, :) * step;
  rows = find(derivative > sqrt(eps));
  if ~isempty(rows)
    alpha = min(alpha, min(max(0, -state.cineq(inactive(rows)) ./ derivative(rows))));
  end
end
%=============================================================================
function working = updateWorkingSet(state, working, opts)
  rows = find(state.cineq >= -opts.ConstraintTolerance)';
  working = unique([working, rows], 'stable');
  if length(working) > length(state.grad)
    violation = state.cineq(working);
    [~, order] = sort(violation, 'descend');
    working = working(order(1:length(state.grad)));
    working = sort(working);
  end
end
%=============================================================================
function value = activeOptimality(state, working)
  W = [state.Jeq; state.Jineq(working, :)];
  if isempty(W)
    value = norm(state.grad, Inf);
    return
  end
  multipliers = solveSystem(W * W', W * (-state.grad));
  value = norm(state.grad + W' * multipliers, Inf);
end
%=============================================================================
function value = activeMerit(state)
  value = state.f + (10 + norm(state.grad, Inf)) * (sum(max(0, state.cineq)) + sum(abs(state.ceq)));
end
%=============================================================================
function H = activeBfgs(H, s, y)
  if s' * y <= 1e-12 * max(1, norm(s) * norm(y))
    H = H + 1e-8 * eye(size(H));
    return
  end
  Hs = H * s;
  H = H - (Hs * Hs') / max(s' * Hs, eps) + (y * y') / (s' * y);
  H = (H + H') / 2;
end
%=============================================================================
function lambda = activeLambda(data, state, working, H)
  n = length(state.grad);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.ineqlin = zeros(size(data.A, 1), 1);
  lambda.eqlin = zeros(size(data.Aeq, 1), 1);
  lambda.ineqnonlin = zeros(length(state.cnonlin), 1);
  lambda.eqnonlin = zeros(length(state.ceqnonlin), 1);
  W = [state.Jeq; state.Jineq(working, :)];
  if isempty(W)
    return
  end
  K = [H, W'; W, zeros(size(W, 1))];
  sol = solveSystem(K, -[state.grad; zeros(size(W, 1), 1)]);
  multipliers = sol(n + 1:end);
  neq = size(state.Jeq, 1);
  eq = multipliers(1:neq);
  ineq = zeros(length(state.cineq), 1);
  ineq(working) = max(0, multipliers(neq + 1:end));
  lambda.eqlin = take(eq, 1, size(data.Aeq, 1));
  lambda.eqnonlin = take(eq, size(data.Aeq, 1) + 1, length(eq));
  lambda.ineqlin = take(ineq, 1, size(data.A, 1));
  lambda.ineqnonlin = take(ineq, size(data.A, 1) + 1, size(data.A, 1) + length(state.cnonlin));
  lowerStart = size(data.A, 1) + length(state.cnonlin) + 1;
  lowerValues = take(ineq, lowerStart, lowerStart + length(data.lowerIndex) - 1);
  upperValues = take(ineq, lowerStart + length(data.lowerIndex), lowerStart + length(data.lowerIndex) + length(data.upperIndex) - 1);
  lambda.lower(data.lowerIndex) = lowerValues;
  lambda.upper(data.upperIndex) = upperValues;
end
%=============================================================================
function value = take(x, first, last)
  if last < first
    value = zeros(0, 1);
  else
    value = x(first:last);
  end
end
%=============================================================================
function best = feasibleBest(best, x, state, opts)
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
function output = activeOutput(iter, funcCount, state, data, message, best, stepSize, firstorderopt)
  output.iterations = iter;
  output.funcCount = funcCount;
  output.constrviolation = state.violation;
  output.stepsize = stepSize;
  output.algorithm = data.algorithm;
  output.firstorderopt = firstorderopt;
  output.cgiterations = 0;
  output.message = message;
  output.bestfeasible = best;
end
%=============================================================================
function x = projectEqualities(x, Aeq, beq)
  if isempty(Aeq)
    return
  end
  residual = Aeq * x - beq;
  x = x - Aeq' * solveSystem(Aeq * Aeq', residual);
end
%=============================================================================
function x = solveSystem(A, b)
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
