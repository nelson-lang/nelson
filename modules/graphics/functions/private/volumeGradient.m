%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [gx, gy, gz] = volumeGradient(V, x, y, z)
  gx = derivativeAlongDimension(V, x, 2);
  gy = derivativeAlongDimension(V, y, 1);
  gz = derivativeAlongDimension(V, z, 3);
end
%=============================================================================
function d = derivativeAlongDimension(V, coordinates, dim)
  d = zeros(size(V));
  n = size(V, dim);
  for k = 1:n
    [first, second] = derivativeStencil(k, n);
    d = assignDerivativeSlice(d, V, coordinates, dim, k, first, second);
  end
end
%=============================================================================
function [first, second] = derivativeStencil(k, n)
  if k == 1
    first = 1;
    second = 2;
  elseif k == n
    first = n - 1;
    second = n;
  else
    first = k - 1;
    second = k + 1;
  end
end
%=============================================================================
function d = assignDerivativeSlice(d, V, coordinates, dim, k, first, second)
  target = repmat({':'}, 1, ndims(V));
  lower = target;
  upper = target;
  if dim == 1
    target{1} = k;
    lower{1} = first;
    upper{1} = second;
  elseif dim == 2
    target{2} = k;
    lower{2} = first;
    upper{2} = second;
  else
    target{3} = k;
    lower{3} = first;
    upper{3} = second;
  end
  denominator = coordinates(second) - coordinates(first);
  d(target{:}) = (V(upper{:}) - V(lower{:})) / denominator;
end
%=============================================================================
