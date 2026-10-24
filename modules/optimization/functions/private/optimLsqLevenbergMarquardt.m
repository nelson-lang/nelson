%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimLsqLevenbergMarquardt(fun, x0, lb, ub, opts, ctx)
  % Levenberg-Marquardt least-squares engine shared by lsqnonlin and fsolve.
  % Supports bound constraints by projection. Returns a result struct with a
  % stop kind that the caller maps to solver-specific exit flags.
  x = min(max(double(x0(:)), lb), ub);
  n = length(x);
  [funcCount, initialResidual] = initialEvaluation(ctx, x, double(x0(:)));
  damping = initialDamping(opts);
  [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, initialResidual);
  funcCount = funcCount + added;
  resnorm = r' * r;
  g = J' * r;
  iterations = 0;
  stepNorm = 0;
  stop = struct('kind', 'maxiter', 'value', ctx.maxIter, 'tolerance', ctx.maxIter);
  optimLsqDisplay(opts, 'header', 'lm', []);
  values = iterValues(0, funcCount, resnorm, firstOrder(g, x, lb, ub), damping, 0);
  optimLsqDisplay(opts, 'iter', 'lm', values);
  if optimOutputStop(opts.OutputFcn, x, values, 'init')
    stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
    result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, ...
      firstOrder(g, x, lb, ub), damping);
    optimOutputStop(opts.OutputFcn, x, values, 'done');
    return
  end
  for iter = 1:ctx.maxIter
    gradNorm = firstOrder(g, x, lb, ub);
    if gradNorm <= 1e-4 * opts.FunctionTolerance
      stop = struct('kind', 'optimality-relative', 'value', gradNorm, ...
        'tolerance', 1e-4 * opts.FunctionTolerance);
      break
    end
    JtJ = J' * J;
    scaling = max(diag(JtJ), sqrt(eps));
    accepted = false;
    trial = x;
    trialResidual = r;
    trialResnorm = resnorm;
    firstTry = true;
    while true
      step = safeSolve(JtJ + damping * diag(scaling), -g);
      % The 1e-2 factor keeps LM iterating on small but productive steps, as
      % the expected behavior does, so the gradient test can fire first.
      if firstTry && norm(step) <= 1e-2 * opts.StepTolerance * (sqrt(eps) + norm(x))
        stop = struct('kind', 'direction', 'value', norm(step), ...
          'tolerance', opts.StepTolerance);
        break
      end
      firstTry = false;
      candidate = min(max(x + step, lb), ub);
      candidateResidual = evaluateResidual(fun, candidate);
      funcCount = funcCount + 1;
      candidateResnorm = candidateResidual' * candidateResidual;
      if isfinite(candidateResnorm) && candidateResnorm < resnorm
        accepted = true;
        trial = candidate;
        trialResidual = candidateResidual;
        trialResnorm = candidateResnorm;
        damping = max(damping / 10, eps);
        break
      end
      damping = damping * 10;
      if damping > 1e16
        if firstOrder(g, x, lb, ub) <= 1e-6 * max(1, sqrt(resnorm))
          % Stationary point of the squared residual: no descent direction.
          stop = struct('kind', 'stationary', 'value', firstOrder(g, x, lb, ub), ...
            'tolerance', 1e-6 * max(1, sqrt(resnorm)));
        else
          stop = struct('kind', 'regularization', 'value', damping, 'tolerance', 1e16);
        end
        break
      end
      if funcCount >= ctx.maxFunEvals
        stop = struct('kind', 'maxfeval', 'value', funcCount, ...
          'tolerance', ctx.maxFunEvals);
        break
      end
    end
    if ~accepted
      break
    end
    stepNorm = norm(trial - x);
    previousResnorm = resnorm;
    x = trial;
    [J, r, added] = optimFiniteDiffJacobian(fun, x, opts, trialResidual);
    funcCount = funcCount + added;
    resnorm = r' * r;
    g = J' * r;
    iterations = iterations + 1;
    values = iterValues(iterations, funcCount, resnorm, ...
      firstOrder(g, x, lb, ub), damping, stepNorm);
    optimLsqDisplay(opts, 'iter', 'lm', values);
    if optimOutputStop(opts.OutputFcn, x, values, 'iter')
      stop = struct('kind', 'outputfcn', 'value', 0, 'tolerance', 0);
      break
    end
    if stepNorm <= 1e-2 * opts.StepTolerance * (sqrt(eps) + norm(x))
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
    if funcCount >= ctx.maxFunEvals
      stop = struct('kind', 'maxfeval', 'value', funcCount, ...
        'tolerance', ctx.maxFunEvals);
      break
    end
  end
  result = makeResult(x, r, resnorm, J, stop, iterations, funcCount, stepNorm, ...
    firstOrder(g, x, lb, ub), damping);
  optimOutputStop(opts.OutputFcn, x, ...
    iterValues(iterations, funcCount, resnorm, result.firstorderopt, damping, stepNorm), 'done');
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
function damping = initialDamping(opts)
  damping = 0.01;
  if isfield(opts, 'InitDamping') && ~isempty(opts.InitDamping)
    damping = double(opts.InitDamping);
  end
end
%=============================================================================
function value = firstOrder(g, x, lb, ub)
  % Gradient norm restricted to directions that stay feasible.
  free = true(size(g));
  atLower = isfinite(lb) & ((x - lb) <= sqrt(eps) * max(1, abs(x)));
  atUpper = isfinite(ub) & ((ub - x) <= sqrt(eps) * max(1, abs(x)));
  free(atLower & g > 0) = false;
  free(atUpper & g < 0) = false;
  if any(free)
    value = norm(g(free), Inf);
  else
    value = 0;
  end
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
function values = iterValues(iteration, funcCount, resnorm, firstorderopt, damping, stepSize)
  values = struct();
  values.iteration = iteration;
  values.funccount = funcCount;
  values.fval = resnorm;
  values.resnorm = resnorm;
  values.firstorderopt = firstorderopt;
  values.lambda = damping;
  values.stepsize = stepSize;
end
%=============================================================================
function result = makeResult(x, residual, resnorm, J, stop, iterations, funcCount, stepNorm, firstorderopt, damping)
  result.x = x;
  result.residual = residual;
  result.resnorm = resnorm;
  result.J = J;
  result.stop = stop;
  result.iterations = iterations;
  result.funcCount = funcCount;
  result.stepsize = stepNorm;
  result.firstorderopt = firstorderopt;
  result.damping = damping;
  result.cgiterations = [];
end
%=============================================================================
