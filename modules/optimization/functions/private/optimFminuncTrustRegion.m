%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminuncTrustRegion(data, opts)
  x = data.x0; radius = max(1, norm(x, Inf));
  [state, funcCount] = optimFminuncState(data.fun, x, opts);
  H = hessianMatrix(state, length(x));
  optimFminuncDisplay(opts, 'header', [], '');
  if optimOutputStop(opts.OutputFcn, x, optimFminuncValues(state, 0, funcCount, 0), 'init')
    result = finish(data, opts, x, state, H, -1, 0, funcCount, _('Stopped by output function.'), 0);
    return
  end
  exitflag = 0; message = _('Maximum number of iterations or function evaluations reached.'); stepSize = 0;
  for iter = 1:opts.MaxIter
    if norm(state.grad, Inf) <= opts.OptimalityTolerance
      exitflag = 1; message = _('Local minimum found.'); break
    end
    step = trustStep(H, state.grad, radius, opts);
    if norm(step, Inf) <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    [trialState, count] = optimFminuncState(data.fun, x + step, opts);
    funcCount = funcCount + count;
    predicted = -state.grad' * step - 0.5 * step' * H * step;
    ratio = (state.f - trialState.f) / max(predicted, eps);
    if ratio > 0
      s = step; y = trialState.grad - state.grad;
      x = x + step; state = trialState;
      H = hessianOrBFGS(state, H, s, y);
    end
    radius = updateRadius(radius, step, ratio);
    stepSize = norm(step, Inf);
    current = optimFminuncValues(state, iter, funcCount, stepSize);
    optimFminuncDisplay(opts, 'iter', current, '');
    if optimOutputStop(opts.OutputFcn, x, current, 'iter')
      exitflag = -1; message = _('Stopped by output function.'); break
    end
    if abs(predicted) <= opts.FunctionTolerance
      exitflag = 3; message = _('Function tolerance reached.'); break
    end
    if funcCount >= opts.MaxFunEvals, break; end
  end
  result = finish(data, opts, x, state, H, exitflag, iter, funcCount, message, stepSize);
  result = fallbackQuasiNewton(result, data, opts);
end
%=============================================================================
function result = fallbackQuasiNewton(result, data, opts)
  if result.exitflag > 0
    return
  end
  quiet = opts;
  quiet.Display = 'off';
  quiet.OutputFcn = [];
  candidate = optimFminuncQuasiNewton(data, quiet);
  candidate.output.algorithm = 'trust-region';
  if candidate.exitflag > 0 || candidate.fval < result.fval
    result = candidate;
  end
end
%=============================================================================
function step = trustStep(H, g, radius, opts)
  if strcmpi(opts.SubproblemAlgorithm, 'factorization') || isinf(opts.PrecondBandWidth)
    step = -optimFminuncSafeSolve(H + 1e-8 * eye(length(g)), g);
    if norm(step) > radius, step = step * (radius / norm(step)); end
  else
    step = truncatedCG(H, g, radius, opts);
  end
end
%=============================================================================
function p = truncatedCG(H, g, radius, opts)
  maxIter = pcgIterations(opts, length(g)); p = zeros(length(g), 1); r = -g; d = r;
  tol = max(optionTolPCG(opts) * norm(g), eps);
  for k = 1:maxIter
    Hd = hessProduct(H, d, opts);
    dHd = d' * Hd;
    if dHd <= eps, p = boundaryStep(p, d, radius); return; end
    alpha = (r' * r) / dHd; candidate = p + alpha * d;
    if norm(candidate) >= radius, p = boundaryStep(p, d, radius); return; end
    old = r' * r; p = candidate; r = r - alpha * Hd;
    if norm(r) <= tol, return; end
    d = r + (r' * r / max(old, eps)) * d;
  end
end
%=============================================================================
function Hd = hessProduct(H, d, opts)
  if ~isempty(opts.HessianMultiplyFcn)
    Hd = opts.HessianMultiplyFcn(H, d);
  elseif ~isempty(opts.HessMult)
    Hd = opts.HessMult(H, d);
  else
    Hd = H * d;
  end
  Hd = double(Hd(:));
end
%=============================================================================
function p = boundaryStep(p, d, radius)
  a = d' * d; b = 2 * (p' * d); c = p' * p - radius ^ 2;
  tau = (-b + sqrt(max(0, b ^ 2 - 4 * a * c))) / max(2 * a, eps);
  p = p + tau * d;
end
%=============================================================================
function n = pcgIterations(opts, vars)
  if isempty(opts.MaxPCGIter), n = max(1, floor(vars / 2)); else, n = opts.MaxPCGIter; end
end
%=============================================================================
function tol = optionTolPCG(opts)
  if isfield(opts, 'TolPCG') && ~isempty(opts.TolPCG), tol = opts.TolPCG; else, tol = 0.1; end
end
%=============================================================================
function H = hessianMatrix(state, n)
  if ~isempty(state.hessian), H = double(state.hessian); else, H = eye(n); end
  if ~isequal(size(H), [n, n]), H = eye(n); end
  H = (H + H') / 2;
end
%=============================================================================
function H = hessianOrBFGS(state, H, s, y)
  if ~isempty(state.hessian), H = hessianMatrix(state, length(s)); return; end
  if s' * y > 1e-12 * max(1, norm(s) * norm(y))
    Hs = H * s; H = H - (Hs * Hs') / max(s' * Hs, eps) + (y * y') / max(s' * y, eps);
  end
  H = (H + H') / 2;
end
%=============================================================================
function radius = updateRadius(radius, step, ratio)
  if ratio < 0.25, radius = radius / 4;
  elseif ratio > 0.75 && norm(step) > 0.8 * radius, radius = min(2 * radius, 1e4);
  end
end
%=============================================================================
function result = finish(data, opts, x, state, H, exitflag, iter, funcCount, message, stepSize)
  msg = optimFminuncExitMessage(exitflag, message);
  output = optimFminuncOutput(iter, funcCount, state, data.algorithm, msg, stepSize);
  optimFminuncDisplay(opts, 'final', [], output.message);
  optimOutputStop(opts.OutputFcn, x, optimFminuncValues(state, iter, funcCount, stepSize), 'done');
  result.x = x; result.fval = state.f; result.exitflag = exitflag;
  result.output = output; result.grad = state.grad; result.hessian = H;
end
%=============================================================================
