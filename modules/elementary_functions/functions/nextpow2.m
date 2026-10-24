%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nextpow2(varargin)
  % next higher power of 2.
  narginchk(1, 1);
  nargoutchk(0, 1);
  X = varargin{1};
  if ~isnumeric(X) && ~islogical(X) && ~ischar(X)
    error('Nelson:validators:mustBeNumeric', 'Invalid input argument at position %d. Value must be numeric.', 1);
  end
  classname = class(X);
  if islogical(X) || ischar(X)
    classname = 'double';
  end
  wasSparse = issparse(X);
  a = abs(full(double(X)));
  [f, n] = log2(a);
  % exact powers of two: log2 gives f == 0.5 with n one too large
  n(f == 0.5) = n(f == 0.5) - 1;
  % zero, Inf and NaN propagate as they are
  k = (a == 0) | ~isfinite(a);
  n(k) = a(k);
  n = cast(n, classname);
  if wasSparse
    n = sparse(n);
  end
  varargout{1} = n;
end
%=============================================================================

