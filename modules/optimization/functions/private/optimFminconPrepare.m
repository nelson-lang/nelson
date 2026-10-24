%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = optimFminconPrepare(fun, x0, A, b, Aeq, beq, lb, ub, nonlcon, opts)
  if isempty(x0)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Initial point');
  end
  if ~isnumeric(x0) || ~isreal(x0)
    error('Nelson:optimization:invalidInitialPoint', '%s must be a real numeric array.', 'Initial point');
  end
  if ~(isa(fun, 'function_handle') || ischar(fun) || isstring(fun))
    error('Nelson:optimization:invalidObjective', 'Objective must be a function handle or a function name.');
  end
  x = double(x0(:));
  n = length(x);
  [A, b] = normalizeLinear(A, b, n, 'A');
  [Aeq, beq] = normalizeLinear(Aeq, beq, n, 'Aeq');
  lb = normalizeBound(lb, n, -Inf);
  ub = normalizeBound(ub, n, Inf);
  if any(lb > ub)
    error('Nelson:optimization:boundsInconsistent', 'Lower bounds must not exceed upper bounds.');
  end
  algorithm = lower(char(opts.Algorithm));
  valid = {'interior-point', 'sqp', 'sqp-legacy', 'active-set', 'trust-region-reflective'};
  if ~any(strcmp(algorithm, valid))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Algorithm');
  end
  validateTextChoice(opts.Display, {'off', 'none', 'final', 'final-detailed', 'notify', ...
    'notify-detailed', 'iter', 'iter-detailed'}, sprintf(_('Invalid value for "%s".'), 'Display'));
  validateTextChoice(opts.Diagnostics, {'on', 'off'}, sprintf(_('Invalid value for "%s".'), 'Diagnostics'));
  validateTextChoice(opts.FiniteDifferenceType, {'forward', 'central'}, sprintf(_('Invalid value for "%s".'), 'FiniteDifferenceType'));
  if strcmp(algorithm, 'trust-region-reflective')
    if ~isempty(nonlcon) || ~isempty(A)
      error('Nelson:fmincon:trustRegionConstraints', 'Trust-region-reflective requires bound constraints or linear equalities only.');
    end
    if ~opts.SpecifyObjectiveGradient
      error('Nelson:fmincon:trustRegionGradient', 'Trust-region-reflective requires SpecifyObjectiveGradient set to true.');
    end
  end
  data.fun = fun;
  data.x0 = prepareInitialPoint(x, lb, ub, algorithm);
  data.xShape = size(x0);
  data.A = A;
  data.b = b;
  data.Aeq = Aeq;
  data.beq = beq;
  data.lb = lb;
  data.ub = ub;
  data.lowerIndex = find(isfinite(lb));
  data.upperIndex = find(isfinite(ub));
  data.Jlower = boundJacobian(data.lowerIndex, n, -1);
  data.Jupper = boundJacobian(data.upperIndex, n, 1);
  data.nonlcon = nonlcon;
  data.algorithm = algorithm;
end
%=============================================================================
function validateTextChoice(value, choices, message)
  value = lower(char(value));
  if ~any(strcmp(value, choices))
    error('Nelson:optimization:invalidValue', message);
  end
end
%=============================================================================
function [A, b] = normalizeLinear(A, b, n, name)
  if isempty(A)
    A = zeros(0, n);
    b = zeros(0, 1);
    return
  end
  A = double(A);
  b = double(b(:));
  if size(A, 2) ~= n
    error('Nelson:optimization:columnPerVariable', '%s must have one column for each variable.', name);
  end
  if size(A, 1) ~= length(b)
    error('Nelson:optimization:sizeMismatch', '%s and right hand side dimensions must agree.', name);
  end
end
%=============================================================================
function bound = normalizeBound(bound, n, defaultValue)
  if isempty(bound)
    bound = defaultValue * ones(n, 1);
  elseif isscalar(bound)
    bound = double(bound) * ones(n, 1);
  else
    bound = double(bound(:));
  end
  if length(bound) ~= n
    error('Nelson:dimagree:boundsNumberElementsX0', 'Bounds must have the same number of elements as x0.');
  end
end
%=============================================================================
function x = prepareInitialPoint(x, lb, ub, algorithm)
  x = min(max(x, lb), ub);
  if strcmp(algorithm, 'interior-point')
    both = find(isfinite(lb) & isfinite(ub) & (ub > lb));
    for k = both(:)'
      delta = 0.5 * min(1, ub(k) - lb(k));
      if x(k) <= lb(k)
        x(k) = lb(k) + delta;
      elseif x(k) >= ub(k)
        x(k) = ub(k) - delta;
      end
    end
  end
end
%=============================================================================
function J = boundJacobian(indices, n, signValue)
  m = length(indices);
  J = zeros(m, n);
  if m > 0
    rows = (1:m)';
    cols = indices(:);
    J(rows + (cols - 1) * m) = signValue;
  end
end
%=============================================================================
