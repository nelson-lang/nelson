%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconTRR(data, opts)
  x = projectEquality(min(max(data.x0, data.lb), data.ub), data.Aeq, data.beq);
  radius = max(1, norm(x, Inf));
  H = eye(length(x));
  [state, funcCount] = optimFminconState(data, x, opts);
  displayHeader(opts);
  if optimOutputStop(opts.OutputFcn, x, makeValues(state, 0, funcCount, 0), 'init')
    result = makeResult(data, opts, x, state, -1, 0, funcCount, _('Stopped by output function.'), eye(length(x)), 0);
    return
  end
  exitflag = 0;
  message = _('Maximum number of iterations or function evaluations reached.');
  iter = 0;
  totalCgIterations = 0;
  pcgFlag = 0;
  pcgResidual = norm(state.grad);
  for iter = 1:opts.MaxIter
    H = hessianMatrix(data, opts, x, state, H);
    [direction, cgInfo] = projectedDirection(state.grad, x, data, H, radius, opts);
    cgIterations = cgInfo.iterations;
    pcgFlag = cgInfo.flag;
    pcgResidual = cgInfo.residual;
    totalCgIterations = totalCgIterations + cgIterations;
    if norm(direction, Inf) <= opts.OptimalityTolerance && state.violation <= opts.ConstraintTolerance
      exitflag = 1; message = _('Local minimum found that satisfies the constraints.'); break
    end
    [trial, trialState, accepted, stepNorm, count] = trustStep(data, opts, x, state, direction, radius);
    funcCount = funcCount + count;
    if accepted
      x = trial; state = trialState;
      radius = min(100 * max(1, norm(x, Inf)), max(2 * radius, stepNorm));
    else
      radius = radius / 2;
    end
    values = makeValues(state, iter, funcCount, stepNorm);
    displayIter(opts, values);
    if optimOutputStop(opts.OutputFcn, x, values, 'iter')
      exitflag = -1; message = _('Stopped by output function.'); break
    end
    if stepNorm <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    if funcCount >= opts.MaxFunEvals
      break
    end
  end
  result = makeResult(data, opts, x, state, exitflag, iter, funcCount, message, H, totalCgIterations);
  result.output.pcgflag = pcgFlag;
  result.output.pcgresidual = pcgResidual;
  result.output.trustregionradius = radius;
  optimOutputStop(opts.OutputFcn, x, makeValues(state, iter, funcCount, 0), 'done');
  displayFinal(opts, message);
end
%=============================================================================
function [direction, cgInfo] = projectedDirection(grad, x, data, H, radius, opts)
  g = projectNull(grad, data.Aeq);
  [direction, cgInfo] = truncatedPCG(H, g, radius, opts);
  atLower = abs(x - data.lb) <= sqrt(eps);
  atUpper = abs(x - data.ub) <= sqrt(eps);
  direction(atLower & direction < 0) = 0;
  direction(atUpper & direction > 0) = 0;
end
%=============================================================================
function H = hessianMatrix(data, opts, x, state, fallback)
  if ~isempty(opts.HessianFcn)
    H = feval(opts.HessianFcn, x, hessianLambda(data, x));
  elseif ~isempty(opts.HessFcn)
    H = feval(opts.HessFcn, x, hessianLambda(data, x));
  elseif ~isempty(opts.HessianMultiplyFcn)
    H = buildHessianFromMultiply(opts.HessianMultiplyFcn, x);
  elseif ~isempty(opts.HessMult)
    H = buildHessianFromMultiply(opts.HessMult, x);
  elseif isnumeric(opts.Hessian) && ~isempty(opts.Hessian)
    H = opts.Hessian;
  else
    H = fallback;
  end
  H = double(H);
  if ~isequal(size(H), [length(x), length(x)]) || any(any(~isfinite(H)))
    H = eye(length(x));
  end
  H = (H + H') / 2;
end
%=============================================================================
function H = buildHessianFromMultiply(fun, x)
  n = length(x);
  H = zeros(n, n);
  for k = 1:n
    basis = zeros(n, 1);
    basis(k) = 1;
    try
      column = feval(fun, x, basis);
    catch
      column = feval(fun, basis);
    end
    H(:, k) = double(column(:));
  end
end
%=============================================================================
function lambda = hessianLambda(data, x)
  n = length(x);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.ineqlin = zeros(0, 1);
  lambda.ineqnonlin = zeros(0, 1);
  lambda.eqnonlin = zeros(0, 1);
  lambda.eqlin = zeros(size(data.Aeq, 1), 1);
end
%=============================================================================
function [p, info] = truncatedPCG(H, g, radius, opts)
  maxCg = projectionCgLimit(opts);
  if isempty(maxCg)
    maxIter = max(20, 5 * length(g));
  else
    maxIter = maxCg;
  end
  p = zeros(length(g), 1);
  info.iterations = 0;
  info.flag = 0;
  info.residual = norm(g);
  if norm(g) == 0
    return
  end
  r = -g(:);
  z = preconditionResidual(H, r, opts);
  d = z;
  rz = r' * z;
  tol = max(opts.TolProjCGAbs, opts.TolProjCG * norm(g));
  for k = 1:maxIter
    info.iterations = k;
    Hd = H * d;
    dHd = d' * Hd;
    if dHd <= eps
      p = boundaryStep(p, d, radius);
      info.flag = 2;
      info.residual = norm(r);
      return
    end
    alpha = rz / dHd;
    candidate = p + alpha * d;
    if norm(candidate) >= radius
      p = boundaryStep(p, d, radius);
      info.flag = 3;
      info.residual = norm(r);
      return
    end
    p = candidate;
    r = r - alpha * Hd;
    if norm(r) <= tol
      info.flag = 0;
      info.residual = norm(r);
      return
    end
    z = preconditionResidual(H, r, opts);
    oldrz = rz;
    rz = r' * z;
    d = z + (rz / max(oldrz, eps)) * d;
  end
  info.flag = 1;
  info.residual = norm(r);
end
%=============================================================================
function z = preconditionResidual(H, r, opts)
  if isempty(opts.PrecondBandWidth) || opts.PrecondBandWidth < 0
    z = r;
    return
  end
  if isfinite(opts.PrecondBandWidth) && opts.PrecondBandWidth > 0
    width = min(size(H, 1) - 1, round(opts.PrecondBandWidth));
    M = bandMatrix(H, width);
    z = solveSystem(M, r);
    if all(isfinite(z))
      return
    end
  end
  diagonal = abs(diag(H));
  diagonal(diagonal < sqrt(eps)) = 1;
  z = r ./ diagonal;
end
%=============================================================================
function M = bandMatrix(H, width)
  M = zeros(size(H));
  for k = -width:width
    M = M + diag(diag(H, k), k);
  end
  d = abs(diag(M));
  d(d < sqrt(eps)) = 1;
  M(1:size(M, 1) + 1:end) = d;
end
%=============================================================================
function maxIter = projectionCgLimit(opts)
  if ~isempty(opts.MaxProjCGIter)
    maxIter = opts.MaxProjCGIter;
  else
    maxIter = opts.MaxPCGIter;
  end
end
%=============================================================================
function p = boundaryStep(p, d, radius)
  a = d' * d;
  b = 2 * (p' * d);
  c = p' * p - radius ^ 2;
  tau = (-b + sqrt(max(0, b ^ 2 - 4 * a * c))) / max(2 * a, eps);
  p = p + tau * d;
end
%=============================================================================
function [trial, trialState, accepted, stepNorm, count] = trustStep(data, opts, x, state, direction, radius)
  accepted = false; count = 0; trial = x; trialState = state; stepNorm = 0;
  if norm(direction) > radius
    direction = direction * (radius / norm(direction));
  end
  alpha = 1;
  for k = 1:25
    candidate = projectEquality(min(max(x + alpha * direction, data.lb), data.ub), data.Aeq, data.beq);
    candidate = min(max(candidate, data.lb), data.ub);
    [candidateState, added] = optimFminconState(data, candidate, opts);
    count = count + added;
    if candidateState.f <= state.f - 1e-4 * alpha * max(1, norm(direction) ^ 2) ...
        && candidateState.violation <= max(opts.ConstraintTolerance, state.violation)
      trial = candidate; trialState = candidateState; accepted = true;
      stepNorm = norm(trial - x, Inf);
      return
    end
    alpha = alpha / 2;
  end
end
%=============================================================================
function x = projectEquality(x, Aeq, beq)
  if isempty(Aeq)
    return
  end
  residual = Aeq * x - beq;
  x = x - Aeq' * solveSystem(Aeq * Aeq', residual);
end
%=============================================================================
function y = projectNull(x, Aeq)
  if isempty(Aeq)
    y = x;
    return
  end
  y = x - Aeq' * solveSystem(Aeq * Aeq', Aeq * x);
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
function result = makeResult(data, opts, x, state, exitflag, iter, funcCount, message, H, cgIterations)
  lambda = multipliers(data, opts, x, state);
  result.x = x; result.fval = state.f; result.exitflag = positiveExit(exitflag, state, opts);
  result.lambda = lambda; result.grad = state.grad; result.hessian = H;
  result.output = struct();
  result.output.iterations = iter; result.output.funcCount = funcCount;
  result.output.constrviolation = state.violation; result.output.stepsize = norm(x - data.x0, Inf);
  result.output.algorithm = data.algorithm; result.output.firstorderopt = norm(state.grad, Inf);
  result.output.cgiterations = cgIterations; result.output.message = exitMessage(result.exitflag, message);
  result.output.bestfeasible = bestFeasible(state, x, opts);
end
%=============================================================================
function lambda = multipliers(data, opts, x, state)
  n = length(x);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.ineqlin = zeros(0, 1);
  lambda.ineqnonlin = zeros(0, 1);
  lambda.eqnonlin = zeros(0, 1);
  lambda.eqlin = equalityMultiplier(data.Aeq, state.grad);
  lagGrad = state.grad + data.Aeq' * lambda.eqlin;
  lower = find(abs(x - data.lb) <= opts.ConstraintTolerance & isfinite(data.lb));
  upper = find(abs(x - data.ub) <= opts.ConstraintTolerance & isfinite(data.ub));
  lambda.lower(lower) = max(0, lagGrad(lower));
  lambda.upper(upper) = max(0, -lagGrad(upper));
end
%=============================================================================
function lambda = equalityMultiplier(Aeq, grad)
  if isempty(Aeq)
    lambda = zeros(0, 1);
    return
  end
  lambda = -solveSystem(Aeq * Aeq', Aeq * grad);
end
%=============================================================================
function flag = positiveExit(flag, state, opts)
  if flag == 0 && state.violation <= opts.ConstraintTolerance
    flag = 1;
  end
end
%=============================================================================
function msg = exitMessage(exitflag, defaultMessage)
  if exitflag > 0
    msg = _('Local minimum found that satisfies the constraints.');
  elseif exitflag < 0
    msg = _('Stopped by output function.');
  else
    msg = defaultMessage;
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
