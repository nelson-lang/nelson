%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = cummax(x, varargin)
  % y = cummax(x)
  % y = cummax(x, dim)
  % y = cummax(___, direction)  with 'forward' (default) | 'reverse'
  % y = cummax(___, nanflag)    with 'omitnan' (default) | 'includenan'
  narginchk(1, 4);
  [dim, reverse, includeNaN] = parseCumulativeOptionsLocal(size(x), varargin{:});
  y = cumulativeLocal(x, dim, @max, reverse, includeNaN);
end
%=============================================================================
function [dim, reverse, includeNaN] = parseCumulativeOptionsLocal(szx, varargin)
  dim = firstNonsingletonLocal(szx);
  reverse = false;
  includeNaN = false;
  for k = 1:length(varargin)
    arg = varargin{k};
    if isstring(arg) && isscalar(arg)
      arg = char(arg);
    end
    if isnumeric(arg) && isscalar(arg)
      dim = arg;
    elseif ischar(arg) && strcmpi(arg, 'reverse')
      reverse = true;
    elseif ischar(arg) && strcmpi(arg, 'forward')
      reverse = false;
    elseif ischar(arg) && (strcmpi(arg, 'omitnan') || strcmpi(arg, 'omitmissing'))
      includeNaN = false;
    elseif ischar(arg) && (strcmpi(arg, 'includenan') || strcmpi(arg, 'includemissing'))
      includeNaN = true;
    else
      error('Nelson:data_analysis:unsupportedOption', 'Unsupported option.');
    end
  end
end
%=============================================================================
function y = cumulativeLocal(x, dim, fun, reverse, includeNaN)
  % Inclusive scan via Hillis-Steele doubling: log(n) vectorized steps instead
  % of a per-element loop (a sequential loop with dynamic subscripts is orders
  % of magnitude slower). fun is associative (max/min, omitnan by default), and
  % the NaN-propagating variant used for 'includenan' is associative too, so the
  % doubling result is identical to the sequential scan.
  y = x;
  n = size(x, dim);
  if n <= 1
    return
  end
  nd = ndims(x);
  checkNaN = includeNaN && isfloat(x);
  s = 1;
  while s < n
    if reverse
      hi = subscriptLocal(dim, 1:n - s, nd);
      lo = subscriptLocal(dim, s + 1:n, nd);
    else
      hi = subscriptLocal(dim, s + 1:n, nd);
      lo = subscriptLocal(dim, 1:n - s, nd);
    end
    a = y(hi{:});
    b = y(lo{:});
    combined = fun(a, b);
    if checkNaN
      combined(isnan(a) | isnan(b)) = NaN;
    end
    y(hi{:}) = combined;
    s = s * 2;
  end
end
%=============================================================================
function sub = subscriptLocal(dim, k, nDims)
  sub = repmat({':'}, 1, nDims);
  sub{dim} = k;
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
