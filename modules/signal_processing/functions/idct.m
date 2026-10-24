%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = idct(x, n, dim)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if nargin() < 3
    dim = firstNonSingletonDimension(x);
  end
  if nargin() < 2 || isempty(n)
    n = size(x, dim);
  end
  y = applyDctMatrix(x, n, dim, true);
end
%=============================================================================
function y = applyDctMatrix(x, n, dim, inverse)
  validateTransformLength(n);
  x = resizeAlongDimension(x, n, dim);
  order = [dim, 1:dim - 1, dim + 1:ndims(x)];
  xp = permute(x, order);
  originalSize = size(xp);
  columns = reshape(xp, n, []);
  matrix = dctMatrix(n);
  if inverse
    columns = matrix.' * columns;
  else
    columns = matrix * columns;
  end
  yp = reshape(columns, originalSize);
  y = ipermute(yp, order);
end
%=============================================================================
function matrix = dctMatrix(n)
  row = (0:n - 1).';
  column = 0:n - 1;
  matrix = sqrt(2 / n) * cos((pi / (2 * n)) * (row * (2 * column + 1)));
  matrix(1, :) = 1 / sqrt(n);
end
%=============================================================================
function y = resizeAlongDimension(x, n, dim)
  current = size(x, dim);
  if current == n
    y = x;
    return
  end
  sizes = size(x);
  if dim > numel(sizes)
    sizes(numel(sizes) + 1:dim) = 1;
  end
  sizes(dim) = n;
  y = zeros(sizes);
  count = min(current, n);
  source = repmat({':'}, 1, ndims(y));
  target = source;
  source{dim} = 1:count;
  target{dim} = 1:count;
  y(target{:}) = x(source{:});
end
%=============================================================================
function dim = firstNonSingletonDimension(x)
  sizes = size(x);
  dim = find(sizes ~= 1, 1);
  if isempty(dim)
    dim = 1;
  end
end
%=============================================================================
function validateTransformLength(n)
  if ~isnumeric(n) || ~isscalar(n) || ~isreal(n) || n < 0 || fix(n) ~= n
    error('Nelson:signal:invalidTransformLength', '%s must be a nonnegative integer scalar.', 'Transform length');
  end
end
%=============================================================================
