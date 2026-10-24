%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [state, count] = optimFminuncState(fun, x, opts)
  if useHessianOutput(opts)
    [state.f, state.grad, state.hessian] = optimCall(fun, x);
    state.f = scalarValue(state.f, opts);
    state.grad = double(state.grad(:));
    state.hessian = double(state.hessian);
    count = 1;
    return
  end
  if useGradient(opts)
    [state.f, state.grad] = optimCall(fun, x);
    state.f = scalarValue(state.f, opts);
    state.grad = double(state.grad(:));
    state.hessian = [];
    count = 1;
    return
  end
  state.f = scalarValue(optimCall(fun, x), opts);
  state.grad = finiteGradient(fun, x, state.f, opts);
  state.hessian = [];
  count = 1 + finiteCount(x, opts);
end
%=============================================================================
function tf = useGradient(opts)
  tf = opts.SpecifyObjectiveGradient || strcmpi(opts.GradObj, 'on');
end
%=============================================================================
function tf = useHessianOutput(opts)
  tf = useGradient(opts) && (ischar(opts.HessianFcn) || isstring(opts.HessianFcn)) ...
    && strcmpi(char(opts.HessianFcn), 'objective');
end
%=============================================================================
function f = scalarValue(value, opts)
  value = optimCheckFunValue(value, opts);
  if ~isnumeric(value) || ~isscalar(value)
    error('Nelson:optimization:invalidObjectiveScalar', '%s must return a numeric scalar.', 'Objective function');
  end
  f = double(value);
end
%=============================================================================
function g = finiteGradient(fun, x, f0, opts)
  step = finiteStep(x, opts);
  g = zeros(length(x), 1);
  useCentral = strcmpi(opts.FiniteDifferenceType, 'central');
  for k = 1:length(x)
    if useCentral
      xp = x; xm = x;
      xp(k) = xp(k) + step(k); xm(k) = xm(k) - step(k);
      fp = scalarValue(optimCall(fun, xp), opts);
      fm = scalarValue(optimCall(fun, xm), opts);
      g(k) = (fp - fm) / (2 * step(k));
    else
      xp = x; xp(k) = xp(k) + step(k);
      fp = scalarValue(optimCall(fun, xp), opts);
      g(k) = (fp - f0) / step(k);
    end
  end
end
%=============================================================================
function count = finiteCount(x, opts)
  if strcmpi(opts.FiniteDifferenceType, 'central')
    count = 2 * length(x);
  else
    count = length(x);
  end
end
%=============================================================================
function step = finiteStep(x, opts)
  scale = finiteScale(x, opts);
  if isempty(opts.FiniteDifferenceStepSize)
    if strcmpi(opts.FiniteDifferenceType, 'central')
      base = eps ^ (1 / 3);
    else
      base = sqrt(eps);
    end
    step = base * scale;
  elseif isscalar(opts.FiniteDifferenceStepSize)
    step = double(opts.FiniteDifferenceStepSize) * scale;
  else
    step = double(opts.FiniteDifferenceStepSize(:)) .* scale;
  end
  step(step == 0) = sqrt(eps);
end
%=============================================================================
function scale = finiteScale(x, opts)
  scale = abs(x(:));
  t = typicalX(opts, length(scale));
  t = t(:);
  mask = scale < t;
  scale(mask) = t(mask);
end
%=============================================================================
function t = typicalX(opts, n)
  if isempty(opts.TypicalX)
    t = ones(n, 1);
  elseif isscalar(opts.TypicalX)
    t = double(opts.TypicalX) * ones(n, 1);
  else
    t = double(opts.TypicalX(:));
  end
end
%=============================================================================
