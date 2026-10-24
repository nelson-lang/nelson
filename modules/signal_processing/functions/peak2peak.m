%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function y = peak2peak(x, dim)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin < 2
    y = extremaDistance(x);
  elseif ischar(dim) || isstring(dim)
    option = lower(char(dim));
    if ~strcmp(option, 'all')
      error('Nelson:signal:unsupportedOption', 'Unsupported option.');
    end
    y = extremaDistance(x, [], 'all');
  elseif isscalar(dim)
    y = extremaDistance(x, [], dim);
  else
    dims = validateDimensions(dim);
    yMax = x;
    yMin = x;
    for k = dims
      yMax = max(yMax, [], k);
      yMin = min(yMin, [], k);
    end
    y = trimTrailingSingletonDimensions(yMax - yMin);
  end
end
%=============================================================================
function y = extremaDistance(x, varargin)
  y = max(x, varargin{:}) - min(x, varargin{:});
end
%=============================================================================
function dims = validateDimensions(dims)
  if isempty(dims) || ~isnumeric(dims) || ~isvector(dims)
    error('Nelson:signal:dimensionScalarOrVector', 'Dimension must be a positive integer scalar or vector.');
  end
  if any(~isfinite(dims(:))) || any(dims(:) <= 0) || any(dims(:) ~= fix(dims(:)))
    error('Nelson:signal:dimensionScalarOrVector', 'Dimension must be a positive integer scalar or vector.');
  end
  dims = unique(sort(dims(:).'));
end
%=============================================================================
function y = trimTrailingSingletonDimensions(y)
  shape = size(y);
  while length(shape) > 2 && shape(end) == 1
    shape(end) = [];
  end
  y = reshape(y, shape);
end
%=============================================================================
