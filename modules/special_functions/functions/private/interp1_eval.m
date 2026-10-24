%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vq = interp1_eval(x, v, xq, method, extrapMode, extrapValue)
  if nargin < 4 || isempty(method)
    method = 'linear';
  end
  if nargin < 5 || isempty(extrapMode)
    extrapMode = 'default';
  end
  if nargin < 6
    extrapValue = NaN;
  end

  x = full(x(:));
  if ~isnumeric(x) || ~isnumeric(v) || ~isnumeric(xq)
    error('Nelson:interp1:numericExpected', 'Numeric input arguments expected.');
  end
  if issparse(x) || issparse(v) || issparse(xq)
    error('Nelson:special_functions:denseExpected', 'dense type for all input arguments expected.');
  end
  if ~isreal(x) || ~isreal(xq)
    error('Nelson:interp1:mustBeReal', 'Sample points and query points must be real.');
  end
  n = numel(x);
  if n < 2
    error('Nelson:special_functions:notEnoughSamplePoints', 'At least two sample points expected.');
  end
  dx = diff(x);
  if any(dx == 0) || ~(all(dx > 0) || all(dx < 0))
    error('Nelson:interp1:samplePointsNotMonotonic', 'Sample points must be strictly monotonic.');
  end

  originalVector = isvector(v);
  if originalVector
    if numel(v) ~= n
      error('Nelson:dimagree:xVLength', 'X and V must be of the same length.');
    end
    vMatrix = full(v(:));
    extraDims = [];
  else
    if size(v, 1) ~= n
      error('Nelson:dimagree:lENGTHXSIZEV1', 'LENGTH(X) and SIZE(V,1) must be the same.');
    end
    extraDims = size(v);
    extraDims = extraDims(2:end);
    vMatrix = reshape(full(v), n, []);
  end

  if dx(1) < 0
    x = flipud(x);
    vMatrix = flipud(vMatrix);
  end

  qSize = size(xq);
  q = full(xq(:));
  out = zeros(numel(q), size(vMatrix, 2)) + 0 * vMatrix(1, 1);
  for col = 1:size(vMatrix, 2)
    out(:, col) = interp1_eval_column(x, vMatrix(:, col), q, method, extrapMode, extrapValue);
  end

  if originalVector
    vq = reshape(out(:, 1), qSize);
  else
    if isvector(xq)
      vq = reshape(out, [numel(q), extraDims]);
    else
      vq = reshape(out, [qSize, extraDims]);
    end
  end
end
%=============================================================================
function yq = interp1_eval_column(x, y, q, method, extrapMode, extrapValue)
  if strcmp(method, 'spline')
    % Use the dedicated not-a-knot spline; interp1 extrapolates for 'spline'
    % by default, matching spline().
    yq = spline(x, y, q);
    if strcmp(extrapMode, 'constant')
      outside = (q < min(x)) | (q > max(x));
      yq(outside) = extrapValue;
    end
    return
  end
  if strcmp(method, 'cubic') || strcmp(method, 'v5cubic')
    % 'cubic'/'v5cubic' use cubic convolution on a uniformly spaced grid; on a
    % non-uniform grid the reference warns and falls back to 'spline'.
    if strcmp(extrapMode, 'extrap')
      % 'cubic'/'v5cubic' do not extrapolate; the reference warns and returns
      % NaN outside the sample range.
      warning('Nelson:interp1:noExtrap', 'The ''cubic'' and ''v5cubic'' methods do not support extrapolation.');
      extrapMode = 'default';
      extrapValue = NaN;
    end
    dh = diff(x);
    if all(abs(dh - dh(1)) <= 1e-10 * max(abs(dh(1)), 1))
      yq = interp1_cubic_convolution(x, y, q, extrapMode, extrapValue);
    else
      warning('Nelson:interp1:cubicNonUniform', 'The ''cubic'' method requires the grid to have a uniform spacing. Switching the method from ''cubic'' to ''spline'' because this condition is not met.');
      yq = spline(x, y, q);
      if strcmp(extrapMode, 'constant')
        outside = (q < min(x)) | (q > max(x));
        yq(outside) = extrapValue;
      end
    end
    return
  end
  % 'makima' is evaluated by the pure Nelson path (interp1_vector); the native
  % __interp1__ does not implement modified Akima interpolation.
  nativeMethods = {'linear', 'nearest', 'previous', 'next', 'pchip', 'spline'};
  if any(strcmp(method, nativeMethods))
    try
      if strcmp(extrapMode, 'extrap')
        yq = __interp1__(x, y, q, method, 'extrap');
      elseif strcmp(extrapMode, 'constant')
        yq = __interp1__(x, y, q, method, extrapValue);
      elseif any(strcmp(method, {'pchip', 'spline'}))
        yq = __interp1__(x, y, q, method, 'extrap');
      else
        yq = __interp1__(x, y, q, method);
      end
      return
    catch
    end
  end
  yq = interp1_vector(x, y, q, method, extrapMode, extrapValue);
end
%=============================================================================
function yq = interp1_cubic_convolution(x, y, q, extrapMode, extrapValue)
  n = numel(x);
  x = x(:);
  y = y(:);
  q = q(:);
  h = x(2) - x(1);
  yq = zeros(numel(q), 1);
  lo = x(1);
  hi = x(n);
  for k = 1:numel(q)
    qi = q(k);
    if (qi < lo || qi > hi) && ~strcmp(extrapMode, 'extrap')
      if strcmp(extrapMode, 'constant')
        yq(k) = extrapValue;
      else
        yq(k) = NaN;
      end
      continue
    end
    idx = floor((qi - lo) / h) + 1;
    if idx < 1
      idx = 1;
    elseif idx > n - 1
      idx = n - 1;
    end
    t = (qi - x(idx)) / h;
    yi = y(idx);
    yp1 = y(idx + 1);
    if idx - 1 >= 1
      ym1 = y(idx - 1);
    else
      ym1 = 3 * y(1) - 3 * y(2) + y(3);
    end
    if idx + 2 <= n
      yp2 = y(idx + 2);
    else
      yp2 = 3 * y(n) - 3 * y(n - 1) + y(n - 2);
    end
    yq(k) = yi + 0.5 * t * ((yp1 - ym1) ...
      + t * ((2 * ym1 - 5 * yi + 4 * yp1 - yp2) ...
      + t * (-ym1 + 3 * yi - 3 * yp1 + yp2)));
  end
end
%=============================================================================
