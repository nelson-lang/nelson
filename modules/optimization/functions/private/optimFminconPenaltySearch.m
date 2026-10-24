%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconPenaltySearch(data, opts)
  penalties = [1e4, 1e6, 1e8];
  starts = startPoints(data);
  bestPenalty = Inf;
  bestF = Inf;
  bestFeasible = false;
  totalIterations = 0;
  totalFuncCount = 0;
  exitflag = 0;
  x = data.x0;
  [state, initCount] = optimFminconState(data, x, opts);
  totalFuncCount = totalFuncCount + initCount;
  displayHeader(opts);
  if optimOutputStop(opts.OutputFcn, x, makeValues(state, 0, totalFuncCount, 0), 'init')
    result = makeResult(data, opts, x, state, eye(length(x)), -1, 0, totalFuncCount, _('Stopped by output function.'));
    return
  end
  for p = 1:length(penalties)
    objective = @(z) penaltyObjective(data, z, opts, penalties(p));
    searchOpts = penaltyOptions(opts, p == length(penalties));
    for k = 1:size(starts, 2)
      [candidate, penaltyValue, localExit, searchOutput] = fminsearch(objective, starts(:, k), searchOpts);
      candidate = min(max(candidate(:), data.lb), data.ub);
      [candidateState, ~] = optimFminconState(data, candidate, opts);
      feasible = candidateState.violation <= max(opts.ConstraintTolerance, 1e-5);
      take = (feasible && (~bestFeasible || candidateState.f < bestF)) || (~bestFeasible && penaltyValue < bestPenalty);
      if take
        x = candidate;
        bestPenalty = penaltyValue;
        bestF = candidateState.f;
        bestFeasible = feasible;
        exitflag = localExit;
      end
      totalIterations = totalIterations + searchOutput.iterations;
      totalFuncCount = totalFuncCount + searchOutput.funcCount;
      values = makeValues(candidateState, totalIterations, totalFuncCount, norm(candidate - data.x0, Inf));
      displayIter(opts, values);
      if optimOutputStop(opts.OutputFcn, candidate, values, 'iter')
        x = candidate;
        state = candidateState;
        exitflag = -1;
        break
      end
    end
    starts = [x, starts(:, 1)];
    if exitflag < 0
      break
    end
  end
  [state, funcCount] = optimFminconState(data, x, opts);
  H = eye(length(x));
  exitflag = positiveExitflag(exitflag, state, opts);
  result = makeResult(data, opts, x, state, H, exitflag, totalIterations, totalFuncCount + funcCount, exitMessage(exitflag));
  optimOutputStop(opts.OutputFcn, x, makeValues(state, totalIterations, result.output.funcCount, 0), 'done');
  displayFinal(opts, result.output.message);
end
%=============================================================================
function searchOpts = penaltyOptions(opts, finalPass)
  if finalPass
    maxIter = opts.MaxIter;
    maxFunEvals = opts.MaxFunEvals;
  else
    maxIter = min(40, opts.MaxIter);
    maxFunEvals = min(400, opts.MaxFunEvals);
  end
  searchOpts = optimset('Display', 'off', 'MaxIter', maxIter, 'MaxFunEvals', maxFunEvals, ...
    'TolX', opts.StepTolerance, 'TolFun', opts.FunctionTolerance);
end
%=============================================================================
function result = makeResult(data, opts, x, state, H, exitflag, iterations, funcCount, message)
  lambda = estimateLambda(data, opts, state);
  result.x = x;
  result.fval = state.f;
  result.exitflag = exitflag;
  result.lambda = lambda;
  result.grad = state.grad;
  result.hessian = H;
  result.output = struct();
  result.output.iterations = iterations;
  result.output.funcCount = funcCount;
  result.output.constrviolation = state.violation;
  result.output.stepsize = norm(x - data.x0, Inf);
  result.output.algorithm = data.algorithm;
  result.output.firstorderopt = norm(state.grad, Inf);
  result.output.cgiterations = 0;
  result.output.message = message;
  result.output.bestfeasible = bestFeasible(state, x, opts);
end
%=============================================================================
function lambda = estimateLambda(data, opts, state)
  lambda = emptyLambda(data, state);
  active = find(state.cineq >= -opts.ConstraintTolerance);
  W = [state.Jeq; state.Jineq(active, :)];
  if isempty(W)
    return
  end
  multipliers = solveMultipliers(W, state.grad);
  neq = size(state.Jeq, 1);
  eqMultipliers = sliceVector(multipliers, 1, neq);
  ineqMultipliers = zeros(size(state.cineq));
  ineqMultipliers(active) = max(0, sliceVector(multipliers, neq + 1, length(multipliers)));
  nEqLin = size(data.Aeq, 1);
  nLin = size(data.A, 1);
  nNon = length(state.cnonlin);
  n = length(data.x0);
  lambda.eqlin = sliceVector(eqMultipliers, 1, nEqLin);
  lambda.eqnonlin = sliceVector(eqMultipliers, nEqLin + 1, length(eqMultipliers));
  lambda.ineqlin = sliceVector(ineqMultipliers, 1, nLin);
  lambda.ineqnonlin = sliceVector(ineqMultipliers, nLin + 1, nLin + nNon);
  lowerStart = nLin + nNon + 1;
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
function multipliers = solveMultipliers(W, grad)
  rhs = -grad(:);
  system = W * W';
  target = W * rhs;
  try
    multipliers = system \ target;
    if all(isfinite(multipliers))
      return
    end
  catch
  end
  multipliers = pinv(W') * rhs;
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
function best = bestFeasible(state, x, opts)
  if state.violation > opts.ConstraintTolerance
    best = [];
    return
  end
  best.x = x;
  best.fval = state.f;
  best.firstorderopt = norm(state.grad, Inf);
  best.constrviolation = state.violation;
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
function starts = startPoints(data)
  starts = data.x0(:);
  n = length(data.x0);
  finiteBoth = isfinite(data.lb) & isfinite(data.ub);
  if n <= 5 && any(finiteBoth)
    middle = starts(:, 1);
    middle(finiteBoth) = (data.lb(finiteBoth) + data.ub(finiteBoth)) / 2;
    lower = starts(:, 1);
    lower(isfinite(data.lb)) = data.lb(isfinite(data.lb));
    upper = starts(:, 1);
    upper(isfinite(data.ub)) = data.ub(isfinite(data.ub));
    starts = [starts, middle, lower, upper];
  end
end
%=============================================================================
function value = penaltyObjective(data, z, opts, penalty)
  z = min(max(z(:), data.lb), data.ub);
  f = optimCheckFunValue(optimCall(data.fun, z), opts);
  if ~isnumeric(f) || ~isscalar(f)
    error('Nelson:optimization:invalidObjectiveScalar', '%s must return a numeric scalar.', 'Objective function');
  end
  cineq = [data.A * z - data.b; data.lb - z; z - data.ub];
  ceq = data.Aeq * z - data.beq;
  if ~isempty(data.nonlcon)
    [c, ceqNonlin] = optimCall(data.nonlcon, z);
    cineq = [cineq; double(c(:))];
    ceq = [ceq; double(ceqNonlin(:))];
  end
  value = double(f) + penalty * (sum(max(0, cineq) .^ 2) + sum(ceq .^ 2));
end
%=============================================================================
function lambda = emptyLambda(data, state)
  n = length(data.x0);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.ineqlin = zeros(size(data.A, 1), 1);
  lambda.eqlin = zeros(size(data.Aeq, 1), 1);
  lambda.ineqnonlin = zeros(length(state.cnonlin), 1);
  lambda.eqnonlin = zeros(length(state.ceqnonlin), 1);
end
%=============================================================================
function flag = positiveExitflag(searchFlag, state, opts)
  if searchFlag < 0
    flag = searchFlag;
  elseif state.violation <= max(opts.ConstraintTolerance, 1e-5)
    flag = 1;
  else
    flag = searchFlag;
  end
end
%=============================================================================
function msg = exitMessage(exitflag)
  if exitflag > 0
    msg = _('Local minimum found that satisfies the constraints.');
  elseif exitflag < 0
    msg = _('Stopped by output function.');
  else
    msg = _('Maximum number of iterations or function evaluations reached.');
  end
end
%=============================================================================
function displayHeader(opts)
  if isIterDisplay(opts)
    disp(' Iteration   Func-count         Fval   Feasibility   First-order opt')
  end
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
