%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function a = polyarea(x, y, dim)
  narginchk(2, 3);
  if ~isequal(size(x), size(y))
    error('Nelson:polyarea:sizeMismatch', 'X and Y must have the same size.');
  end
  if ~isnumeric(x) || ~isnumeric(y)
    error('Nelson:polyarea:inputsMustBeNumeric', 'X and Y must be numeric.');
  end
  if nargin < 3
    dim = find(size(x) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  else
    if ~(isscalar(dim) && isreal(dim) && dim > 0 && dim == floor(dim))
      error('Nelson:polyarea:invalidDimension', 'DIM must be a positive integer.');
    end
  end
  % Shoelace formula: A = 1/2 |sum_i (x_i * y_{i+1} - x_{i+1} * y_i)|.
  xShift = circshift(x, -1, dim);
  yShift = circshift(y, -1, dim);
  a = 0.5 * abs(sum(x .* yShift - xShift .* y, dim));
end
%=============================================================================
