%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFsolveDogleg(fun, x0, opts, ctx)
  % Powell dogleg trust-region engine for square nonlinear systems (fsolve).
  x = double(x0(:));
  [funcCount, initialResidual] = initialEvaluation(ctx);
  [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, initialResidual);
  funcCount = funcCount + added;
  resnorm = r' * r;
  g = J' * r;
  radius = max(1, norm(x));
  iterations = 0;
  stepNorm = 0;
  stop = struct('kind', 'maxiter', 'value', ctx.maxIter, 'tolerance', ctx.maxIter);
  optimLsqDisplay(opts, 'header', 'dogleg', []);
  values = iterValues(0, funcCount, resnorm, norm(g, Inf), radius, 0);
  optimLsqDisplay(opts, 'iter', 'dogleg', values);
  if optimOutputStop(opts.OutputFcn, x, values, 'init')
    stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
    result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, norm(g, Inf));
    optimOutputStop(opts.OutputFcn, x, values, 'done');
    return
  end
  for iter = 1:ctx.maxIter
    if resnorm <= opts.FunctionTolerance
      stop = struct('kind', 'optimality', 'value', resnorm, ...
        'tolerance', opts.FunctionTolerance);
      break
    end
    % Stationary point of the squared residual that is not a root: the
    % gradient vanishes while the residual stays large.
    if norm(g, Inf) <= max(opts.OptimalityTolerance, sqrt(eps)) * max(1, norm(r))
      stop = struct('kind', 'stationary', 'value', norm(g, Inf), ...
        'tolerance', opts.OptimalityTolerance);
      break
    end
    newtonStep = safeSolve(J, -r);
    if norm(newtonStep) <= opts.StepTolerance * (sqrt(eps) + norm(x))
      stop = struct('kind', 'direction', 'value', norm(newtonStep), ...
        'tolerance', opts.StepTolerance);
      break
    end
    step = doglegStep(newtonStep, g, J, radius);
    trial = x + step;
    trialResidual = evaluateResidual(fun, trial);
    funcCount = funcCount + 1;
    trialResnorm = trialResidual' * trialResidual;
    predicted = resnorm - predictedResnorm(r, J, step);
    actual = resnorm - trialResnorm;
    if ~isfinite(trialResnorm)
      ratio = -1;
    else
      ratio = actual / max(predicted, eps);
    end
    if ratio > 1e-4
      stepNorm = norm(step);
      previousResnorm = resnorm;
      x = trial;
      [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, trialResidual);
      funcCount = funcCount + added;
      resnorm = r' * r;
      g = J' * r;
      iterations = iterations + 1;
      values = iterValues(iterations, funcCount, resnorm, norm(g, Inf), radius, stepNorm);
      optimLsqDisplay(opts, 'iter', 'dogleg', values);
      if optimOutputStop(opts.OutputFcn, x, values, 'iter')
        stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
        break
      end
      if stepNorm <= opts.StepTolerance * (sqrt(eps) + norm(x))
        stop = struct('kind', 'step', 'value', stepNorm, ...
          'tolerance', opts.StepTolerance);
        break
      end
      if abs(previousResnorm - resnorm) <= opts.FunctionTolerance * max(resnorm, eps)
        stop = struct('kind', 'funchange', ...
          'value', abs(previousResnorm - resnorm) / max(resnorm, eps), ...
          'tolerance', opts.FunctionTolerance);
        break
      end
    end
    if ratio < 0.25
      radius = 0.25 * norm(step);
    elseif ratio > 0.75 && norm(step) >= 0.9 * radius
      radius = min(2 * radius, 1e16);
    end
    if radius <= eps * (1 + norm(x))
      stop = struct('kind', 'regularization', 'value', radius, ...
        'tolerance', eps * (1 + norm(x)));
      break
    end
    if funcCount >= ctx.maxFunEvals
      stop = struct('kind', 'maxfeval', 'value', funcCount, ...
        'tolerance', ctx.maxFunEvals);
      break
    end
  end
  if strcmp(stop.kind, 'maxiter') && resnorm <= opts.FunctionTolerance
    stop = struct('kind', 'optimality', 'value', resnorm, ...
      'tolerance', opts.FunctionTolerance);
  end
  result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, norm(g, Inf));
  optimOutputStop(opts.OutputFcn, x, ...
    iterValues(iterations, funcCount, resnorm, norm(g, Inf), radius, stepNorm), 'done');
end
%=============================================================================
function [funcCount, initialResidual] = initialEvaluation(ctx)
  funcCount = 0;
  initialResidual = [];
  if isfield(ctx, 'initialResidual') && ~isempty(ctx.initialResidual)
    initialResidual = ctx.initialResidual;
    funcCount = 1;
  end
end
%=============================================================================
function step = doglegStep(newtonStep, g, J, radius)
  if norm(newtonStep) <= radius
    step = newtonStep;
    return
  end
  Jg = J * g;
  cauchyScale = (g' * g) / max(Jg' * Jg, eps);
  cauchyStep = -cauchyScale * g;
  normCauchy = norm(cauchyStep);
  if normCauchy >= radius
    step = -(radius / max(norm(g), eps)) * g;
    return
  end
  % Walk from the Cauchy point towards the Newton point until the radius.
  difference = newtonStep - cauchyStep;
  a = difference' * difference;
  b = 2 * (cauchyStep' * difference);
  c = (cauchyStep' * cauchyStep) - radius ^ 2;
  discriminant = max(b ^ 2 - 4 * a * c, 0);
  tau = (-b + sqrt(discriminant)) / max(2 * a, eps);
  tau = min(max(tau, 0), 1);
  step = cauchyStep + tau * difference;
end
%=============================================================================
function value = predictedResnorm(r, J, step)
  predicted = r + J * step;
  value = predicted' * predicted;
end
%=============================================================================
function residual = evaluateResidual(fun, x)
  residual = double(optimCall(fun, x));
  residual = residual(:);
end
%=============================================================================
function step = safeSolve(A, b)
  useDirect = false;
  if size(A, 1) == size(A, 2)
    useDirect = rcond(A) > eps;
  end
  if useDirect
    step = A \ b;
    if all(isfinite(step))
      return
    end
  end
  step = pinv(A) * b;
end
%=============================================================================
function values = iterValues(iteration, funcCount, resnorm, firstorderopt, radius, stepSize)
  values = struct();
  values.iteration = iteration;
  values.funccount = funcCount;
  values.fval = resnorm;
  values.resnorm = resnorm;
  values.firstorderopt = firstorderopt;
  values.radius = radius;
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
  result.cgiterations = [];
end
%=============================================================================
