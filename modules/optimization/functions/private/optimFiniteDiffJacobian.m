%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [J, f0, count] = optimFiniteDiffJacobian(fun, x, opts, knownF0)
  % knownF0 (optional): residual already evaluated at x, reused to avoid one
  % function evaluation when finite differencing.
  x = double(x(:));
  if strcmpi(opts.Jacobian, 'on') || opts.SpecifyObjectiveGradient
    [f0, J] = optimCall(fun, x);
    f0 = double(f0(:));
    J = double(J);
    count = 1;
    return
  end
  if nargin >= 4 && ~isempty(knownF0)
    f0 = double(knownF0(:));
    count = 0;
  else
    f0 = optimCall(fun, x);
    f0 = double(f0(:));
    count = 1;
  end
  n = length(x);
  m = length(f0);
  J = zeros(m, n);
  step = opts.FiniteDifferenceStepSize;
  if isempty(step)
    scale = max(abs(x), 1);
    if ~isempty(opts.TypicalX)
      typicalX = double(opts.TypicalX(:));
      if isscalar(typicalX)
        typicalX = repmat(typicalX, n, 1);
      end
      scale = max(scale, abs(typicalX));
    end
    step = sqrt(eps) * scale;
  elseif isscalar(step)
    step = repmat(double(step), n, 1);
  else
    step = double(step(:));
  end
  for k = 1:n
    xp = x;
    if strcmpi(opts.FiniteDifferenceType, 'central')
      xp(k) = xp(k) + step(k);
      xm = x;
      xm(k) = xm(k) - step(k);
      fp = double(optimCall(fun, xp)); fp = fp(:);
      fm = double(optimCall(fun, xm)); fm = fm(:);
      J(:, k) = (fp - fm) ./ (2 * step(k));
      count = count + 2;
    else
      xp(k) = xp(k) + step(k);
      fp = double(optimCall(fun, xp)); fp = fp(:);
      J(:, k) = (fp - f0) ./ step(k);
      count = count + 1;
    end
  end
end
%=============================================================================
