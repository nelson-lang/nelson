%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminuncQuasiNewton(data, opts)
  x = data.x0; H = eye(length(x)); memory = {};
  [state, funcCount] = optimFminuncState(data.fun, x, opts);
  optimFminuncDisplay(opts, 'header', [], '');
  if optimOutputStop(opts.OutputFcn, x, optimFminuncValues(state, 0, funcCount, 0), 'init')
    result = finish(data, opts, x, state, H, -1, 0, funcCount, _('Stopped by output function.'), 0);
    return
  end
  exitflag = 0; message = _('Maximum number of iterations or function evaluations reached.'); stepSize = 0;
  for iter = 1:opts.MaxIter
    if state.f <= opts.ObjectiveLimit
      exitflag = -3; message = _('Objective limit reached.'); break
    end
    if norm(state.grad, Inf) <= opts.OptimalityTolerance
      exitflag = 1; message = _('Local minimum found.'); break
    end
    direction = searchDirection(H, state.grad, memory, opts);
    [trial, trialState, accepted, stepSize, count] = lineSearch(data, opts, x, state, direction);
    funcCount = funcCount + count;
    if ~accepted
      exitflag = 0; message = 'Line search failed to find a lower objective value.'; break
    end
    s = trial - x; y = trialState.grad - state.grad;
    [H, memory] = updateHessian(H, memory, s, y, opts);
    if abs(state.f - trialState.f) <= opts.FunctionTolerance
      x = trial; state = trialState; exitflag = 3; message = _('Function tolerance reached.'); break
    end
    x = trial; state = trialState;
    current = optimFminuncValues(state, iter, funcCount, stepSize);
    optimFminuncDisplay(opts, 'iter', current, '');
    if optimOutputStop(opts.OutputFcn, x, current, 'iter')
      exitflag = -1; message = _('Stopped by output function.'); break
    end
    if stepSize <= opts.StepTolerance
      exitflag = 2; message = _('Step size tolerance reached.'); break
    end
    if funcCount >= opts.MaxFunEvals
      break
    end
  end
  result = finish(data, opts, x, state, H, exitflag, iter, funcCount, message, stepSize);
end
%=============================================================================
function d = searchDirection(H, g, memory, opts)
  if optimFminuncIsLBFGS(opts)
    d = -optimFminuncLBFGSMultiply(memory, g);
  elseif optionEquals(opts.HessianApproximation, 'steepdesc') || optionEquals(opts.HessUpdate, 'steepdesc')
    d = -g;
  else
    d = -optimFminuncSafeSolve(H, g);
  end
  if any(~isfinite(d)) || norm(d) == 0, d = -g; end
end
%=============================================================================
function [trial, trialState, accepted, stepSize, count] = lineSearch(data, opts, x, state, direction)
  alpha = 1; accepted = false; trial = x; trialState = state; count = 0; stepSize = 0;
  slope = state.grad' * direction;
  if slope >= 0, direction = -state.grad; slope = -state.grad' * state.grad; end
  for k = 1:30
    candidate = x + alpha * direction;
    [candidateState, added] = optimFminuncState(data.fun, candidate, opts);
    count = count + added;
    if candidateState.f <= state.f + 1e-4 * alpha * slope
      trial = candidate; trialState = candidateState; stepSize = norm(alpha * direction, Inf);
      accepted = true; return
    end
    alpha = alpha / 2;
  end
end
%=============================================================================
function [H, memory] = updateHessian(H, memory, s, y, opts)
  if s' * y <= 1e-12 * max(1, norm(s) * norm(y)), return; end
  if optimFminuncIsLBFGS(opts)
    memory = optimFminuncLBFGSUpdate(memory, s, y, optimFminuncLBFGSMemory(opts)); return
  end
  if optionEquals(opts.HessUpdate, 'dfp') || optionEquals(opts.HessianApproximation, 'dfp')
    H = dfpUpdate(H, s, y);
  elseif ~optionEquals(opts.HessianApproximation, 'steepdesc')
    H = bfgsUpdate(H, s, y);
  end
  H = (H + H') / 2;
end
%=============================================================================
function tf = optionEquals(value, expected)
  tf = false;
  if iscell(value)
    if isempty(value), return; end
    value = value{1};
  end
  if ischar(value) || isstring(value)
    tf = strcmpi(char(value), expected);
  end
end
%=============================================================================
function H = bfgsUpdate(H, s, y)
  Hs = H * s;
  H = H - (Hs * Hs') / max(s' * Hs, eps) + (y * y') / max(s' * y, eps);
end
%=============================================================================
function H = dfpUpdate(H, s, y)
  Hy = H * y;
  H = H + (s * s') / max(s' * y, eps) - (Hy * Hy') / max(y' * Hy, eps);
end
%=============================================================================
function result = finish(data, opts, x, state, H, exitflag, iter, funcCount, message, stepSize)
  output = optimFminuncOutput(iter, funcCount, state, data.algorithm, optimFminuncExitMessage(exitflag, message), stepSize);
  optimFminuncDisplay(opts, 'final', [], output.message);
  optimOutputStop(opts.OutputFcn, x, optimFminuncValues(state, iter, funcCount, stepSize), 'done');
  result.x = x; result.fval = state.f; result.exitflag = exitflag;
  result.output = output; result.grad = state.grad; result.hessian = H;
end
%=============================================================================
