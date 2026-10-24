%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [state, count] = optimFminconState(data, x, opts)
  state.x = x(:);
  [state.f, state.grad, objectiveCount] = objectiveState(data.fun, x, opts);
  [c, ceq, Jc, Jceq, constraintCount] = constraintState(data.nonlcon, x, opts);
  state.cnonlin = c;
  state.ceqnonlin = ceq;
  state.Jcnonlin = Jc;
  state.Jceqnonlin = Jceq;
  lower = data.lb(data.lowerIndex) - x(data.lowerIndex); lower = lower(:);
  upper = x(data.upperIndex) - data.ub(data.upperIndex); upper = upper(:);
  state.cineq = [data.A * x - data.b; c; lower; upper];
  state.Jineq = [data.A; Jc; data.Jlower; data.Jupper];
  state.ceq = [data.Aeq * x - data.beq; ceq];
  state.Jeq = [data.Aeq; Jceq];
  state.violation = max([0; state.cineq(:); abs(state.ceq(:))]);
  state.merit = state.f + 10 * (sum(max(0, state.cineq)) + sum(abs(state.ceq)));
  count = objectiveCount + constraintCount;
end
%=============================================================================
function [f, g, count] = objectiveState(fun, x, opts)
  if opts.SpecifyObjectiveGradient || strcmpi(opts.GradObj, 'on')
    [f, g] = optimCall(fun, x);
    f = scalarValue(f, opts);
    g = double(g(:));
    count = 1;
    return
  end
  f = scalarValue(optimCall(fun, x), opts);
  g = zeros(length(x), 1);
  step = finiteStep(x, opts);
  count = 1;
  useCentral = strcmpi(opts.FiniteDifferenceType, 'central');
  for k = 1:length(x)
    if useCentral
      xp = x; xm = x;
      xp(k) = xp(k) + step(k); xm(k) = xm(k) - step(k);
      fp = scalarValue(optimCall(fun, xp), opts);
      fm = scalarValue(optimCall(fun, xm), opts);
      g(k) = (fp - fm) / (2 * step(k));
      count = count + 2;
    else
      xp = x; xp(k) = xp(k) + step(k);
      fp = scalarValue(optimCall(fun, xp), opts);
      g(k) = (fp - f) / step(k);
      count = count + 1;
    end
  end
end
%=============================================================================
function [c, ceq, Jc, Jceq, count] = constraintState(nonlcon, x, opts)
  if isempty(nonlcon)
    c = zeros(0, 1); ceq = zeros(0, 1);
    Jc = zeros(0, length(x)); Jceq = zeros(0, length(x));
    count = 0;
    return
  end
  if opts.SpecifyConstraintGradient || strcmpi(opts.GradConstr, 'on')
    [c, ceq, gc, gceq] = optimCall(nonlcon, x);
    [c, ceq] = constraintValues(c, ceq);
    Jc = double(gc)'; Jceq = double(gceq)';
    validateJacobian(Jc, length(c), length(x), sprintf('Nonlinear %s gradient has invalid dimensions.', 'inequality'));
    validateJacobian(Jceq, length(ceq), length(x), sprintf('Nonlinear %s gradient has invalid dimensions.', 'equality'));
    count = 1;
    return
  end
  [c, ceq] = optimCall(nonlcon, x);
  [c, ceq] = constraintValues(c, ceq);
  mIneq = length(c); mEq = length(ceq);
  Jc = zeros(mIneq, length(x)); Jceq = zeros(mEq, length(x));
  step = finiteStep(x, opts);
  count = 1;
  useCentral = strcmpi(opts.FiniteDifferenceType, 'central');
  for k = 1:length(x)
    if useCentral
      xp = x; xm = x;
      xp(k) = xp(k) + step(k); xm(k) = xm(k) - step(k);
      [cp, ceqp] = optimCall(nonlcon, xp);
      [cm, ceqm] = optimCall(nonlcon, xm);
      if mIneq > 0
        Jc(:, k) = (double(cp(:)) - double(cm(:))) / (2 * step(k));
      end
      if mEq > 0
        Jceq(:, k) = (double(ceqp(:)) - double(ceqm(:))) / (2 * step(k));
      end
      count = count + 2;
    else
      xp = x; xp(k) = xp(k) + step(k);
      [cp, ceqp] = optimCall(nonlcon, xp);
      if mIneq > 0
        Jc(:, k) = (double(cp(:)) - c) / step(k);
      end
      if mEq > 0
        Jceq(:, k) = (double(ceqp(:)) - ceq) / step(k);
      end
      count = count + 1;
    end
  end
end
%=============================================================================
function [c, ceq] = constraintValues(c, ceq)
  if isempty(c)
    c = zeros(0, 1);
  elseif isnumeric(c) && isreal(c)
    c = double(c(:));
  else
    error('Nelson:optimization:invalidConstraintValue', 'Nonlinear constraint function must return real numeric values.');
  end
  if isempty(ceq)
    ceq = zeros(0, 1);
  elseif isnumeric(ceq) && isreal(ceq)
    ceq = double(ceq(:));
  else
    error('Nelson:optimization:invalidConstraintValue', 'Nonlinear constraint function must return real numeric values.');
  end
end
%=============================================================================
function validateJacobian(J, rows, cols, message)
  if ~isequal(size(J), [rows, cols])
    error('Nelson:optimization:sizeMismatch', message);
  end
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
function step = finiteStep(x, opts)
  n = length(x);
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
