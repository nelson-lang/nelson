%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimLsqTrustRegion(fun, x0, lb, ub, opts, ctx)
  % Reflective trust-region least-squares engine (Coleman-Li scaling with a
  % Gauss-Newton model) shared by lsqnonlin (default algorithm) and fsolve
  % ('trust-region'). Bounds are honored by strictly interior iterates.
  x = startPoint(double(x0(:)), lb, ub);
  [funcCount, initialResidual] = initialEvaluation(ctx, x, double(x0(:)));
  [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, initialResidual);
  funcCount = funcCount + added;
  resnorm = r' * r;
  g = J' * r;
  radius = max(1, norm(x));
  iterations = 0;
  stepNorm = 0;
  stop = struct('kind', 'maxiter', 'value', ctx.maxIter, 'tolerance', ctx.maxIter);
  optimLsqDisplay(opts, 'header', 'trr', []);
  values = iterValues(0, funcCount, resnorm, scaledOptimality(g, x, lb, ub), 0);
  optimLsqDisplay(opts, 'iter', 'trr', values);
  if optimOutputStop(opts.OutputFcn, x, values, 'init')
    stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
    result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, ...
      scaledOptimality(g, x, lb, ub));
    optimOutputStop(opts.OutputFcn, x, values, 'done');
    return
  end
  for iter = 1:ctx.maxIter
    firstorderopt = scaledOptimality(g, x, lb, ub);
    if firstorderopt <= opts.OptimalityTolerance
      stop = struct('kind', 'optimality', 'value', firstorderopt, ...
        'tolerance', opts.OptimalityTolerance);
      break
    end
    [v, dv] = colemanLiScaling(g, x, lb, ub);
    d = sqrt(abs(v));
    B = J' * J + diag(abs(g) .* dv);
    step = trustRegionStep(B, g, d, radius, x, lb, ub);
    model = g' * step + 0.5 * (step' * (B * step));
    trial = x + step;
    trialResidual = evaluateResidual(fun, trial);
    funcCount = funcCount + 1;
    trialResnorm = trialResidual' * trialResidual;
    predicted = max(-2 * model, eps);
    if ~isfinite(trialResnorm)
      ratio = -1;
    else
      ratio = (resnorm - trialResnorm) / predicted;
    end
    scaledStepNorm = norm(step ./ max(d, sqrt(eps)));
    if ratio > 1e-4
      stepNorm = norm(step);
      previousResnorm = resnorm;
      x = trial;
      [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, trialResidual);
      funcCount = funcCount + added;
      resnorm = r' * r;
      g = J' * r;
      iterations = iterations + 1;
      values = iterValues(iterations, funcCount, resnorm, ...
        scaledOptimality(g, x, lb, ub), stepNorm);
      optimLsqDisplay(opts, 'iter', 'trr', values);
      if optimOutputStop(opts.OutputFcn, x, values, 'iter')
        stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
        break
      end
      if abs(previousResnorm - resnorm) <= opts.FunctionTolerance * max(resnorm, eps)
        stop = struct('kind', 'funchange', ...
          'value', abs(previousResnorm - resnorm) / max(resnorm, eps), ...
          'tolerance', opts.FunctionTolerance);
        break
      end
      if stepNorm <= opts.StepTolerance * (sqrt(eps) + norm(x))
        stop = struct('kind', 'step', 'value', stepNorm, ...
          'tolerance', opts.StepTolerance);
        break
      end
    end
    if ratio < 0.25
      radius = 0.25 * min(radius, scaledStepNorm);
    elseif ratio > 0.75 && scaledStepNorm >= 0.9 * radius
      radius = min(2 * radius, 1e16);
    end
    if radius <= eps * (1 + norm(x))
      stop = struct('kind', 'step', 'value', radius, ...
        'tolerance', opts.StepTolerance);
      break
    end
    if funcCount >= ctx.maxFunEvals
      stop = struct('kind', 'maxfeval', 'value', funcCount, ...
        'tolerance', ctx.maxFunEvals);
      break
    end
  end
  if strcmp(stop.kind, 'maxiter')
    firstorderopt = scaledOptimality(g, x, lb, ub);
    if firstorderopt <= opts.OptimalityTolerance
      stop = struct('kind', 'optimality', 'value', firstorderopt, ...
        'tolerance', opts.OptimalityTolerance);
    end
  end
  result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, ...
    scaledOptimality(g, x, lb, ub));
  optimOutputStop(opts.OutputFcn, x, ...
    iterValues(iterations, funcCount, resnorm, result.firstorderopt, stepNorm), 'done');
end
%=============================================================================
function [funcCount, initialResidual] = initialEvaluation(ctx, x, xRaw)
  funcCount = 0;
  initialResidual = [];
  if isfield(ctx, 'initialResidual') && ~isempty(ctx.initialResidual) && isequal(x, xRaw)
    initialResidual = ctx.initialResidual;
    funcCount = 1;
  end
end
%=============================================================================
function x = startPoint(x, lb, ub)
  % Strictly interior starting point, as required by the reflective method.
  x = min(max(x, lb), ub);
  onLower = isfinite(lb) & (x <= lb);
  onUpper = isfinite(ub) & (x >= ub);
  room = ub - lb;
  for k = find(onLower(:))'
    x(k) = lb(k) + shiftAmount(room(k));
  end
  for k = find(onUpper(:))'
    x(k) = ub(k) - shiftAmount(room(k));
  end
end
%=============================================================================
function delta = shiftAmount(room)
  if isfinite(room)
    delta = min(1e-2, room / 2);
  else
    delta = 1e-2;
  end
end
%=============================================================================
function [v, dv] = colemanLiScaling(g, x, lb, ub)
  n = length(x);
  v = ones(n, 1);
  dv = zeros(n, 1);
  for k = 1:n
    if g(k) < 0 && isfinite(ub(k))
      v(k) = ub(k) - x(k);
      dv(k) = 1;
    elseif g(k) >= 0 && isfinite(lb(k))
      v(k) = x(k) - lb(k);
      dv(k) = 1;
    end
  end
end
%=============================================================================
function value = scaledOptimality(g, x, lb, ub)
  v = colemanLiScaling(g, x, lb, ub);
  value = norm(v .* g, Inf);
end
%=============================================================================
function step = trustRegionStep(B, g, d, radius, x, lb, ub)
  % Choose the best of the (possibly reflected) modified Newton step and the
  % scaled steepest descent step, clipped to the trust region and kept
  % strictly inside the bounds.
  D = diag(d);
  A = D * B * D;
  gs = d .* g;
  newtonHat = safeSolve(A, -gs);
  candidates = {};
  candidates{end + 1} = clipToRadius(newtonHat, radius);
  curvature = gs' * (A * gs);
  cauchyScale = (gs' * gs) / max(curvature, eps);
  cauchyHat = -min(cauchyScale, radius / max(norm(gs), eps)) * gs;
  candidates{end + 1} = cauchyHat;
  best = [];
  bestModel = Inf;
  for k = 1:length(candidates)
    unscaled = d .* candidates{k};
    variants = interiorVariants(unscaled, x, lb, ub);
    for j = 1:length(variants)
      s = variants{j};
      model = g' * s + 0.5 * (s' * (B * s));
      if model < bestModel
        bestModel = model;
        best = s;
      end
    end
  end
  if isempty(best)
    best = zeros(size(g));
  end
  step = best;
end
%=============================================================================
function s = clipToRadius(s, radius)
  stepNorm = norm(s);
  if stepNorm > radius
    s = (radius / stepNorm) * s;
  end
end
%=============================================================================
function variants = interiorVariants(s, x, lb, ub)
  % Truncate the step at the first bound hit; also propose a single
  % reflection of the crossing component.
  [alpha, index] = boundFraction(s, x, lb, ub);
  if alpha >= 1
    variants = {s};
    return
  end
  truncated = 0.995 * alpha * s;
  variants = {truncated};
  if index > 0 && alpha > 0
    reflected = s;
    reflected(index) = -reflected(index);
    remainder = 0.995 * (1 - alpha) * reflected;
    combined = truncated + remainder;
    beta = boundFraction(combined - truncated, x + truncated, lb, ub);
    combined = truncated + 0.995 * min(1, beta) * (combined - truncated);
    variants{end + 1} = combined;
  end
end
%=============================================================================
function [alpha, index] = boundFraction(s, x, lb, ub)
  alpha = Inf;
  index = 0;
  for k = 1:length(s)
    if s(k) > 0 && isfinite(ub(k))
      candidate = (ub(k) - x(k)) / s(k);
    elseif s(k) < 0 && isfinite(lb(k))
      candidate = (lb(k) - x(k)) / s(k);
    else
      candidate = Inf;
    end
    if candidate < alpha
      alpha = candidate;
      index = k;
    end
  end
  alpha = max(alpha, 0);
end
%=============================================================================
function residual = evaluateResidual(fun, x)
  residual = double(optimCall(fun, x));
  residual = residual(:);
end
%=============================================================================
function step = safeSolve(A, b)
  try
    step = A \ b;
    if all(isfinite(step))
      return
    end
  catch
  end
  step = pinv(A) * b;
end
%=============================================================================
function values = iterValues(iteration, funcCount, resnorm, firstorderopt, stepSize)
  values = struct();
  values.iteration = iteration;
  values.funccount = funcCount;
  values.fval = resnorm;
  values.resnorm = resnorm;
  values.firstorderopt = firstorderopt;
  values.stepsize = stepSize;
end
%=============================================================================
function result = makeResult(x, residual, resnorm, J, stop, iterations, funcCount, stepNorm, firstorderopt)
  result.x = x;
  result.residual = residual;
  result.resnorm = resnorm;
  result.J = J;
  result.stop = stop;
  result.iterations = iterations;
  result.funcCount = funcCount;
  result.stepsize = stepNorm;
  result.firstorderopt = firstorderopt;
  result.damping = [];
  result.cgiterations = 0;
end
%=============================================================================
