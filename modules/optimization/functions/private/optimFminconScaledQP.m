%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [step, lambda] = optimFminconScaledQP(H, state, opts, maxIter)
  n = length(state.grad);
  qpopts = opts;
  qpopts.MaxIter = maxIter;
  H = regularizedHessian(H);
  scale = variableScale(H, state, opts);
  Hs = diagScale(H, scale);
  gs = scale(:) .* state.grad(:);
  Jineq = shapedJacobian(state.Jineq, n);
  Jeq = shapedJacobian(state.Jeq, n);
  JsIneq = Jineq * diag(scale);
  JsEq = Jeq * diag(scale);
  [scaledStep, ~, ~, lambda] = optimActiveSetQP(Hs, gs, JsIneq, -state.cineq, ...
    JsEq, -state.ceq, [], [], zeros(n, 1), qpopts);
  step = scale(:) .* scaledStep(:);
  if any(~isfinite(step))
    step = -state.grad;
  end
end
%=============================================================================
function J = shapedJacobian(J, n)
  if isempty(J)
    J = zeros(0, n);
  end
end
%=============================================================================
function H = regularizedHessian(H)
  H = (H + H') / 2;
  diagonal = abs(diag(H));
  diagonal(diagonal < sqrt(eps)) = 1;
  shift = 0;
  for k = 1:6
    try
      chol(H + shift * eye(size(H)));
      H = H + shift * eye(size(H));
      return
    catch
      shift = max(1e-10, 10 ^ (k - 10) * max(diagonal));
    end
  end
  H = H + shift * eye(size(H));
end
%=============================================================================
function scale = variableScale(H, state, opts)
  n = length(state.grad);
  scale = typicalScale(opts, state.x, n);
  if strcmpi(opts.ScaleProblem, 'none') && conditionEstimate(H, state) < 1e8
    scale(:) = 1;
    return
  end
  colNorms = sqrt(columnEnergy(state.Jineq, n) + columnEnergy(state.Jeq, n))';
  curvature = sqrt(abs(diag(H)));
  weight = max([ones(n, 1), colNorms, curvature], [], 2);
  scale = max(scale(:), 1 ./ weight);
  scale(~isfinite(scale) | scale <= 0) = 1;
end
%=============================================================================
function e = columnEnergy(J, n)
  % Sum of squares per column, always returned as a 1-by-n row so that an
  % empty Jacobian (which Nelson collapses to 0-by-0 on concatenation) keeps
  % one entry per variable instead of folding to a scalar.
  if isempty(J)
    e = zeros(1, n);
  else
    e = sum(J .^ 2, 1);
  end
end
%=============================================================================
function value = conditionEstimate(H, state)
  n = length(state.grad);
  d = abs(diag(H));
  j = sqrt(columnEnergy(state.Jineq, n) + columnEnergy(state.Jeq, n))';
  values = [d(:); j(:)];
  values = values(isfinite(values) & values > 0);
  if isempty(values)
    value = 1;
  else
    value = max(values) / max(min(values), eps);
  end
end
%=============================================================================
function scale = typicalScale(opts, x, n)
  if isempty(opts.TypicalX)
    scale = max(1, abs(x(:)));
  elseif isscalar(opts.TypicalX)
    scale = max(1, abs(double(opts.TypicalX))) * ones(n, 1);
  else
    scale = max(1, abs(double(opts.TypicalX(:))));
  end
  if length(scale) ~= n
    scale = ones(n, 1);
  end
end
%=============================================================================
function Hs = diagScale(H, scale)
  s = scale(:);
  Hs = H .* (s * s');
  Hs = (Hs + Hs') / 2;
end
%=============================================================================
