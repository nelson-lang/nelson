%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = resize(A, sz, varargin)
  % Resize an array by truncating or padding.
  %   B = resize(A, m)          resize a vector A to length m
  %   B = resize(A, [m n ...])  resize A to the given size
  %   B = resize(..., 'FillValue', v)  value used for padded elements
  % Elements beyond the original data are filled with a fill value (0 for a
  % numeric array by default).
  narginchk(2, inf);
  if ~isnumeric(sz) || ~isreal(sz) || isempty(sz) || any(sz < 0) || any(sz ~= floor(sz))
    error('Nelson:resize:invalidSize', 'The size must be a vector of nonnegative integers.');
  end
  fillValue = defaultFillValue(A);
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:resize:nameValuePairs', 'Name-value arguments must come in pairs.');
  end
  for k = 1:2:numel(varargin)
    name = varargin{k};
    if (ischar(name) || isStringScalar(name)) && strcmpi(name, 'FillValue')
      fillValue = varargin{k + 1};
    else
      error('Nelson:resize:unknownOption', 'Unknown option.');
    end
  end
  if isscalar(sz)
    if ~isvector(A) && ~isempty(A)
      error('Nelson:resize:scalarSizeVectorOnly', 'A scalar size requires a vector input.');
    end
    if iscolumn(A) && ~isrow(A)
      targetSize = [sz, 1];
    else
      targetSize = [1, sz];
    end
  else
    targetSize = double(sz(:)');
  end
  B = resizeToSize(A, targetSize, fillValue);
end
%=============================================================================
function value = defaultFillValue(A)
  if isnumeric(A)
    value = zeros(1, 1, 'like', A);
  elseif islogical(A)
    value = false;
  elseif iscell(A)
    value = {[]};
  elseif isstring(A)
    value = string(missing);
  else
    value = missing;
  end
end
%=============================================================================
function B = resizeToSize(A, targetSize, fillValue)
  sourceSize = size(A);
  nd = max(numel(targetSize), numel(sourceSize));
  targetSize(end + 1:nd) = 1;
  sourceSize(end + 1:nd) = 1;
  if iscell(A)
    B = repmat(fillValue, targetSize);
  else
    B = repmat(cast(fillValue, 'like', A), targetSize);
  end
  copySize = min(targetSize, sourceSize);
  if any(copySize == 0)
    return
  end
  srcIdx = arrayfun(@(n) 1:n, copySize, 'UniformOutput', false);
  B(srcIdx{:}) = A(srcIdx{:});
end
%=============================================================================
