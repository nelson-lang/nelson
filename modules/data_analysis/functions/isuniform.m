%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, step] = isuniform(v)
  % true when the vector v is evenly spaced up to round-off; step is the spacing or NaN
  narginchk(1, 1);
  if ~(isnumeric(v) || islogical(v)) || ~isreal(v)
    error('Nelson:isuniform:notNumericReal', 'Input must be numeric and real.');
  end
  if ~isvector(v) && ~isempty(v)
    error('Nelson:isuniform:notVector', 'Input must be a vector.');
  end
  if isfloat(v) && ~isempty(v)
    v = v(:);
  else
    v = double(v(:));
  end
  tf = false;
  step = NaN('like', v);
  n = numel(v);
  if n < 2 || ~all(isfinite(v))
    return
  end
  candidate = (v(n) - v(1)) / (n - 1);
  tolerance = 4 * eps(max(abs(v(1)), abs(v(n))));
  tf = all(abs(diff(v) - candidate) <= tolerance);
  if tf
    step = candidate;
  end
  tf = logical(gather(tf));
end
%=============================================================================
