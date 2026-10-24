%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = issorted(varargin)
  % TF = issorted(A)
  % TF = issorted(A, dim)
  % TF = issorted(A, direction)
  % TF = issorted(A, dim, direction)
  % TF = issorted(A, 'rows')
  % direction: 'ascend' (default), 'descend', 'monotonic',
  %            'strictascend', 'strictdescend', 'strictmonotonic'
  narginchk(1, 3);
  A = varargin{1};
  dim = [];
  direction = 'ascend';
  if nargin == 2
    numOrString = varargin{2};
    if isstring(numOrString) && isscalar(numOrString)
      numOrString = char(numOrString);
    end
    if ischar(numOrString)
      if strcmpi(numOrString, 'rows')
        tf = all(all(diff(A, 1, 1) >= 0, 1), 'all');
        return
      end
      direction = validateDirectionLocal(numOrString);
    elseif isnumeric(numOrString) && isscalar(numOrString)
      dim = numOrString;
    else
      error('Nelson:data_analysis:invalidArgument2ScalarInteger', 'Wrong type for argument #%d: %s expected.', 2, 'scalar integer value');
    end
  elseif nargin == 3
    dim = varargin{2};
    if ~(isnumeric(dim) && isscalar(dim))
      error('Nelson:data_analysis:invalidArgument2ScalarInteger', 'Wrong type for argument #%d: %s expected.', 2, 'scalar integer value');
    end
    direction = varargin{3};
    if isstring(direction) && isscalar(direction)
      direction = char(direction);
    end
    direction = validateDirectionLocal(direction);
  end
  if isempty(dim)
    dim = firstNonsingletonLocal(size(A));
  end
  % NaN handling (aligned with expected behavior): for 'ascend' NaN elements must be
  % placed last, for 'descend' they must be placed first, and any NaN
  % makes the strict directions false.
  if isfloat(A)
    nanMask = isnan(A);
    anyNaN = any(nanMask(:));
  else
    nanMask = false(size(A));
    anyNaN = false;
  end
  B = A;
  if anyNaN
    B(nanMask) = Inf;
  end
  d = diff(B, 1, dim);
  m = diff(nanMask, 1, dim);
  dOk = isnan(d);
  switch direction
    case 'ascend'
      tf = all(m(:) >= 0) && all(d(:) >= 0 | dOk(:));
    case 'descend'
      tf = all(m(:) <= 0) && all(d(:) <= 0 | dOk(:));
    case 'strictascend'
      tf = ~anyNaN && all(d(:) > 0);
    case 'strictdescend'
      tf = ~anyNaN && all(d(:) < 0);
    case 'monotonic'
      tf = (all(m(:) >= 0) && all(d(:) >= 0 | dOk(:))) || (all(m(:) <= 0) && all(d(:) <= 0 | dOk(:)));
    case 'strictmonotonic'
      tf = ~anyNaN && (all(d(:) > 0) || all(d(:) < 0));
  end
end
%=============================================================================
function direction = validateDirectionLocal(direction)
  if ~ischar(direction)
    error('Nelson:data_analysis:invalidArgumentAscendDescendMonotonic', '%s expected.', 'ascend, descend, monotonic, strictascend, strictdescend or strictmonotonic');
  end
  direction = lower(direction);
  supported = {'ascend', 'descend', 'monotonic', 'strictascend', 'strictdescend', 'strictmonotonic'};
  if ~any(strcmp(supported, direction))
    error('Nelson:data_analysis:invalidArgumentAscendDescendMonotonic', '%s expected.', 'ascend, descend, monotonic, strictascend, strictdescend or strictmonotonic');
  end
end
%=============================================================================
function dim = firstNonsingletonLocal(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
