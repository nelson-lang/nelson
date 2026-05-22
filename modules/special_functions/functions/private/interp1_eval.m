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
    error(_('Numeric input arguments expected.'));
  end
  if issparse(x) || issparse(v) || issparse(xq)
    error(_('dense type for all input arguments expected.'));
  end
  if ~isreal(x) || ~isreal(xq)
    error(_('Sample points and query points must be real.'));
  end
  n = numel(x);
  if n < 2
    error(_('At least two sample points expected.'));
  end
  dx = diff(x);
  if any(dx == 0) || ~(all(dx > 0) || all(dx < 0))
    error(_('Sample points must be strictly monotonic.'));
  end

  originalVector = isvector(v);
  if originalVector
    if numel(v) ~= n
      error(_('X and V must be of the same length.'));
    end
    vMatrix = full(v(:));
    extraDims = [];
  else
    if size(v, 1) ~= n
      error(_('LENGTH(X) and SIZE(V,1) must be the same.'));
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
  nativeMethods = {'linear', 'nearest', 'previous', 'next', 'pchip', 'cubic', 'makima', 'spline'};
  if any(strcmp(method, nativeMethods))
    try
      if strcmp(extrapMode, 'extrap')
        yq = __interp1__(x, y, q, method, 'extrap');
      elseif strcmp(extrapMode, 'constant')
        yq = __interp1__(x, y, q, method, extrapValue);
      elseif any(strcmp(method, {'pchip', 'makima', 'spline'}))
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
