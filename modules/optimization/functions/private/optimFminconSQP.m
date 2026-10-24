%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconSQP(data, opts)
  if strcmp(data.algorithm, 'trust-region-reflective')
    result = optimFminconTRR(data, opts);
    return
  end
  if strcmp(data.algorithm, 'interior-point')
    result = optimFminconInteriorPoint(data, opts, @optimFminconSQPCore);
  elseif strcmp(data.algorithm, 'active-set')
    result = optimFminconActiveSet(data, opts);
  elseif strcmp(data.algorithm, 'sqp-legacy')
    result = optimFminconSQPLegacy(data, opts);
  else
    result = optimFminconSQPCore(data, opts);
  end
  result = restoreNonlinearFeasibility(data, opts, result);
  result = flagInfeasibleResult(result, opts);
end
%=============================================================================
function result = flagInfeasibleResult(result, opts)
  % A stopped solver whose final point still violates the constraints must
  % report exitflag -2.
  if result.exitflag < 0
    return
  end
  if result.output.constrviolation > max(opts.ConstraintTolerance, 1e-5)
    result.exitflag = -2;
    result.output.message = sprintf(_('Converged to an infeasible point.\n\nfmincon stopped because the size of the current step is less than\nthe value of the step size tolerance but constraints are not\nsatisfied to within the value of the constraint tolerance.'));
  end
end
%=============================================================================
function result = restoreNonlinearFeasibility(data, opts, result)
  if isempty(data.nonlcon)
    return
  end
  if result.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5)
    if result.exitflag == 0 && result.output.firstorderopt <= max(2e-2, 20 * sqrt(opts.OptimalityTolerance))
      result.exitflag = 1;
      result.output.message = localMinimumMessage();
    end
    return
  end
  restoreData = data;
  restoreData.x0 = result.x;
  restoreOpts = opts;
  restoreOpts.Display = 'off';
  penaltyResult = optimFminconPenaltySearch(restoreData, restoreOpts);
  if isBetterNonlinearResult(penaltyResult, result, opts)
    result = penaltyResult;
    result.output.algorithm = data.algorithm;
    result.output.restoration = true;
  else
    result.output.restoration = false;
  end
end
%=============================================================================
function tf = isBetterNonlinearResult(candidate, current, opts)
  cFeasible = candidate.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  rFeasible = current.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  tf = (cFeasible && ~rFeasible) || (cFeasible && rFeasible && candidate.fval < current.fval) ...
    || (~cFeasible && candidate.output.constrviolation < current.output.constrviolation);
end
%=============================================================================
function result = optimFminconSQPCore(data, opts)
  x = data.x0;
  n = length(x);
  H = optimFminconInitialHessian(x, opts);
  funcCount = 0;
  exitflag = 0;
  best = [];
  message = sprintf(_('Solver stopped prematurely.\n\nfmincon stopped because it exceeded the iteration limit,\noptions.MaxIterations = %e.'), sqpIterationLimit(opts));
  [state, added] = optimFminconState(data, x, opts);
  funcCount = funcCount + added;
  displayHeader(opts);
  if optimOutputStop(opts.OutputFcn, x, makeValues(state, 0, funcCount, 0), 'init')
    result = stoppedResult(data, opts, x, state, H, funcCount);
    return
  end
  iter = 0;
  maxIter = sqpIterationLimit(opts);
  for iter = 1:maxIter
    best = updateBest(best, x, state, opts);
    if state.violation <= opts.ConstraintTolerance && state.f <= opts.ObjectiveLimit
      exitflag = -3; message = sprintf(_('Problem appears unbounded.\n\nfmincon stopped because the objective function value is less than\nthe value of the objective function limit.')); break
    end
    [step, qpLambda] = sqpStep(H, state, opts);
    if state.violation <= opts.ConstraintTolerance && norm(step, Inf) <= opts.StepTolerance
      exitflag = 2; message = stepToleranceMessage(); break
    end
    [trial, trialState, accepted, alpha, used] = lineSearch(data, opts, x, state, step, qpLambda);
    funcCount = funcCount + used;
    if accepted
      s = trial - x;
      y = trialState.grad - state.grad;
      H = updateHessian(H, s, y);
      if abs(state.f - trialState.f) <= opts.FunctionTolerance && trialState.violation <= opts.ConstraintTolerance
        % The reference reserves exitflag 3 for trust-region-reflective; the
        % other algorithms report a small step as exitflag 2.
        x = trial; state = trialState; exitflag = 2; message = stepToleranceMessage(); break
      end
      x = trial; state = trialState;
    else
      exitflag = 0; message = 'Line search failed to find a lower merit value.'; break
    end
    values = makeValues(state, iter, funcCount, norm(alpha * step, Inf));
    if optimOutputStop(opts.OutputFcn, x, values, 'iter')
      exitflag = -1; message = _('Solver stopped by an output function or plot function.'); break
    end
    displayIter(opts, values);
    if state.violation <= opts.ConstraintTolerance && firstOrderEstimate(state, qpLambda) <= opts.OptimalityTolerance
      exitflag = 1; message = localMinimumMessage(); break
    end
    if funcCount >= opts.MaxFunEvals
      break
    end
  end
  best = updateBest(best, x, state, opts);
  lambda = finalMultipliers(H, state, data, opts);
  firstorderopt = optimalityFromLambda(state, data, lambda);
  if exitflag == 0 && state.violation <= opts.ConstraintTolerance && firstorderopt <= max(2e-2, sqrt(opts.OptimalityTolerance))
    exitflag = 1;
    message = localMinimumMessage();
  end
  result.x = x;
  result.fval = state.f;
  result.exitflag = exitflag;
  result.lambda = lambda;
  result.grad = state.grad;
  result.hessian = H;
  result.output = makeOutput(iter, funcCount, state, data, message, best, normStepValue(x, data.x0), firstorderopt);
  optimOutputStop(opts.OutputFcn, x, makeValues(state, iter, funcCount, 0), 'done');
  displayFinal(opts, result.output.message);
end
%=============================================================================
function message = localMinimumMessage()
  message = sprintf(_('Local minimum found that satisfies the constraints.\n\nOptimization completed because the objective function is non-decreasing in\nfeasible directions, to within the value of the optimality tolerance,\nand constraints are satisfied to within the value of the constraint tolerance.'));
end
%=============================================================================
function message = stepToleranceMessage()
  message = sprintf(_('Local minimum possible.\n\nfmincon stopped because the size of the current step is less than\nthe value of the step size tolerance and constraints are\nsatisfied to within the value of the constraint tolerance.'));
end
%=============================================================================
function [step, lambda] = sqpStep(H, state, opts)
  n = length(state.grad);
  maxIter = max(20, 5 * n + size(state.Jineq, 1) + size(state.Jeq, 1));
  [step, lambda] = optimFminconScaledQP(H, state, opts, maxIter);
end
%=============================================================================
function result = stoppedResult(data, opts, x, state, H, funcCount)
  lambda = finalMultipliers(H, state, data, opts);
  firstorderopt = optimalityFromLambda(state, data, lambda);
  result.x = x;
  result.fval = state.f;
  result.exitflag = -1;
  result.lambda = lambda;
  result.grad = state.grad;
  result.hessian = H;
  result.output = makeOutput(0, funcCount, state, data, _('Solver stopped by an output function or plot function.'), [], 0, firstorderopt);
end
%=============================================================================
function displayHeader(opts)
  if ~isIterDisplay(opts)
    return
  end
  disp(' Iteration   Func-count         Fval   Feasibility   First-order opt')
end
%=============================================================================
function displayIter(opts, values)
  if ~isIterDisplay(opts)
    return
  end
  disp([int2str(values.iteration), '           ', int2str(values.funccount), '        ', ...
    num2str(values.fval, 12), '    ', num2str(values.constrviolation, 6), ...
    '       ', num2str(values.firstorderopt, 6)])
end
%=============================================================================
function displayFinal(opts, message)
  if isNoDisplay(opts)
    return
  end
  if isIterDisplay(opts)
    disp(' ')
  end
  if isFinalDisplay(opts) || isIterDisplay(opts) || isNotifyDisplay(opts)
    disp(message)
  end
end
%=============================================================================
function tf = isIterDisplay(opts)
  tf = strcmpi(opts.Display, 'iter') || strcmpi(opts.Display, 'iter-detailed');
end
%=============================================================================
function tf = isFinalDisplay(opts)
  tf = strcmpi(opts.Display, 'final') || strcmpi(opts.Display, 'final-detailed');
end
%=============================================================================
function tf = isNoDisplay(opts)
  tf = strcmpi(opts.Display, 'off') || strcmpi(opts.Display, 'none');
end
%=============================================================================
function tf = isNotifyDisplay(opts)
  tf = strcmpi(opts.Display, 'notify') || strcmpi(opts.Display, 'notify-detailed');
end
%=============================================================================
function [trial, trialState, accepted, alpha, count] = lineSearch(data, opts, x, state, step, qpLambda)
  alpha = 1;
  step = boundedLineSearchStep(x, step, opts);
  rho = meritPenalty(state, qpLambda);
  base = meritValue(state, rho);
  accepted = false;
  count = 0;
  trial = x;
  trialState = state;
  for k = 1:25
    candidate = min(max(x + alpha * step, data.lb), data.ub);
    [candidateState, added] = optimFminconState(data, candidate, opts);
    count = count + added;
    if ~isempty(data.nonlcon) && meritValue(candidateState, rho) > base
      [candidate, candidateState, correctedCount] = feasibilityCorrection(data, opts, candidate, candidateState);
      count = count + correctedCount;
    end
    if meritValue(candidateState, rho) <= base - 1e-4 * alpha * max(1, scaledStepNorm(step, x, opts) ^ 2)
      trial = candidate; trialState = candidateState; accepted = true; return
    end
    alpha = alpha / 2;
  end
end
%=============================================================================
function value = scaledStepNorm(step, x, opts)
  if strcmpi(opts.ScaleProblem, 'none')
    value = norm(step);
    return
  end
  if isempty(opts.TypicalX)
    scale = max(1, abs(x(:)));
  elseif isscalar(opts.TypicalX)
    scale = max(1, abs(double(opts.TypicalX))) * ones(length(step), 1);
  else
    scale = max(1, abs(double(opts.TypicalX(:))));
  end
  if length(scale) ~= length(step)
    scale = ones(length(step), 1);
  end
  value = norm(step(:) ./ scale(:));
end
%=============================================================================
function [x, state, count] = feasibilityCorrection(data, opts, x, state)
  count = 0;
  active = find(state.cineq >= -max(opts.ConstraintTolerance, sqrt(eps)));
  residual = state.ceq;
  W = state.Jeq;
  % Appending the inequality block only when it is non-empty avoids relying on
  % max(0, emptyVector), which in Nelson collapses to a scalar 0 and would add
  % a spurious residual row (mismatching W on equality-only problems).
  if ~isempty(active)
    residual = [residual; max(state.cineq(active), 0)];
    W = [W; state.Jineq(active, :)];
  end
  if isempty(W) || isempty(residual)
    return
  end
  correction = W' * solveLinearSystem(W * W' + sqrt(eps) * eye(size(W, 1)), residual);
  trial = min(max(x - correction, data.lb), data.ub);
  [trialState, added] = optimFminconState(data, trial, opts);
  count = count + added;
  if trialState.violation <= state.violation || trialState.merit <= state.merit
    x = trial;
    state = trialState;
  end
end
%=============================================================================
function maxIter = sqpIterationLimit(opts)
  maxIter = opts.MaxIter;
  if isfield(opts, 'MaxSQPIter') && ~isempty(opts.MaxSQPIter)
    maxIter = min(maxIter, opts.MaxSQPIter);
  end
end
%=============================================================================
function step = boundedLineSearchStep(x, step, opts)
  if ~isfield(opts, 'RelLineSrchBnd') || isempty(opts.RelLineSrchBnd)
    return
  end
  if opts.RelLineSrchBnd <= 0 || ~isfinite(opts.RelLineSrchBnd)
    return
  end
  limit = opts.RelLineSrchBnd * max(1, abs(x(:)));
  scale = max(abs(step(:)) ./ max(limit, eps));
  if scale > 1
    step = step / scale;
  end
end
%=============================================================================
function value = meritValue(state, rho)
  value = state.f + rho * (sum(max(0, state.cineq)) + sum(abs(state.ceq)));
end
%=============================================================================
function rho = meritPenalty(state, qpLambda)
  % For the L1 exact-penalty merit function to make the full QP (Newton) step a
  % descent direction, the penalty weight must dominate the constraint
  % multipliers (Nocedal & Wright, eq. 18.36). A fixed weight stalls whenever
  % the multipliers are large, e.g. equality-heavy collocation NLPs.
  floorRho = 10 + norm(state.grad, Inf);
  maxLambda = 0;
  if nargin > 1 && isstruct(qpLambda)
    names = {'eqlin', 'ineqlin', 'eqnonlin', 'ineqnonlin', 'lower', 'upper'};
    for k = 1:numel(names)
      if isfield(qpLambda, names{k})
        component = qpLambda.(names{k});
        if ~isempty(component)
          maxLambda = max(maxLambda, max(abs(double(component(:)))));
        end
      end
    end
  end
  rho = max(floorRho, 1.1 * maxLambda + 1);
end
%=============================================================================
function H = updateHessian(H, s, y)
  if s' * y <= 1e-12 * max(1, norm(s) * norm(y))
    H = H + 1e-8 * eye(size(H));
    return
  end
  Hs = H * s;
  H = H - (Hs * Hs') / max(s' * Hs, eps) + (y * y') / (s' * y);
  H = (H + H') / 2;
end
%=============================================================================
function value = firstOrderEstimate(state, lambda)
  value = norm(state.grad, Inf);
  if isfield(lambda, 'eqlin') && isfield(lambda, 'ineqlin')
    m = size(state.Jineq, 1);
    if m == 0
      return
    end
    active = state.cineq(:) >= -sqrt(eps);
    active = active(1:min(length(active), m));
    multipliers = zeros(m, 1);
    multipliers(active) = 1;
    value = min(value, norm(state.grad + state.Jineq' * multipliers, Inf));
  end
end
%=============================================================================
function best = updateBest(best, x, state, opts)
  if state.violation > opts.ConstraintTolerance
    return
  end
  if isempty(best) || state.f < best.fval
    best.x = x; best.fval = state.f;
    best.firstorderopt = norm(state.grad, Inf);
    best.constrviolation = state.violation;
  end
end
%=============================================================================
function values = makeValues(state, iter, funcCount, stepSize)
  values = struct();
  values.fval = state.f;
  values.iteration = iter;
  values.funccount = funcCount;
  values.constrviolation = state.violation;
  values.stepsize = stepSize;
  values.firstorderopt = norm(state.grad, Inf);
end
%=============================================================================
function output = makeOutput(iter, funcCount, state, data, message, best, stepSize, firstorderopt)
  output.iterations = iter;
  output.funcCount = funcCount;
  output.constrviolation = state.violation;
  output.stepsize = stepSize;
  output.algorithm = data.algorithm;
  output.firstorderopt = firstorderopt;
  output.cgiterations = 0;
  output.message = message;
  if isempty(best)
    output.bestfeasible = [];
  else
    output.bestfeasible = best;
  end
end
%=============================================================================
function value = optimalityFromLambda(state, data, lambda)
  lagGrad = state.grad;
  lagGrad = lagGrad + data.A' * lambda.ineqlin + data.Aeq' * lambda.eqlin;
  lagGrad = lagGrad + state.Jcnonlin' * lambda.ineqnonlin + state.Jceqnonlin' * lambda.eqnonlin;
  lagGrad = lagGrad - lambda.lower + lambda.upper;
  value = norm(lagGrad, Inf);
end
%=============================================================================
function lambda = finalMultipliers(H, state, data, opts)
  n = length(state.grad);
  mLin = size(data.A, 1);
  mNon = length(state.cnonlin);
  % Interior-point iterates stop within a small distance of the active
  % constraints, so activity detection needs a looser tolerance than
  % ConstraintTolerance alone.
  activeTolerance = max(opts.ConstraintTolerance, sqrt(max(opts.StepTolerance, eps)));
  active = find(state.cineq >= -activeTolerance);
  W = [state.Jeq; state.Jineq(active, :)];
  if isempty(W)
    multipliers = zeros(0, 1);
  else
    K = [H, W'; W, zeros(size(W, 1))];
    rhs = -[state.grad; zeros(size(W, 1), 1)];
    sol = solveLinearSystem(K, rhs);
    multipliers = sol(n + 1:end);
  end
  neq = size(state.Jeq, 1);
  eqMultipliers = sliceVector(multipliers, 1, neq);
  ineqMultipliers = zeros(size(state.cineq));
  ineqMultipliers(active) = max(0, multipliers(neq + 1:end));
  nEqLin = size(data.Aeq, 1);
  lambda.eqlin = sliceVector(eqMultipliers, 1, nEqLin);
  lambda.eqnonlin = sliceVector(eqMultipliers, nEqLin + 1, length(eqMultipliers));
  lambda.ineqlin = sliceVector(ineqMultipliers, 1, mLin);
  lambda.ineqnonlin = sliceVector(ineqMultipliers, mLin + 1, mLin + mNon);
  lowerStart = mLin + mNon + 1;
  lowerCount = length(data.lowerIndex);
  upperCount = length(data.upperIndex);
  lowerValues = sliceVector(ineqMultipliers, lowerStart, lowerStart + lowerCount - 1);
  upperValues = sliceVector(ineqMultipliers, lowerStart + lowerCount, ...
    lowerStart + lowerCount + upperCount - 1);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.lower(data.lowerIndex) = lowerValues;
  lambda.upper(data.upperIndex) = upperValues;
end
%=============================================================================
function value = sliceVector(x, first, last)
  if last < first
    value = zeros(0, 1);
  else
    value = x(first:last);
  end
end
%=============================================================================
function value = normStepValue(x, x0)
  value = norm(x - x0, Inf);
end
%=============================================================================
function x = solveLinearSystem(A, b)
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
