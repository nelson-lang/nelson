%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nextpow2 (varargin)
  % next higher power of 2.
  narginchk(1, 1);
  nargoutchk(0, 1);
  X = varargin{1};
  mustBeNumeric(X, 1);
  classname = class(X);
  [f, n] = log2(abs(double(X)));
  idx = (n == 0);
  if ~isempty(f(idx))
    n(idx) = f(idx);
  end
  k = (f == 0.5);
  if ~isempty(n(k))
    n(k) = n(k) - 1;
  end
  k = ~isfinite(f);
  if ~isempty(f(k))
    n(k) = f(k);
  end
  varargout{1} = cast(n, classname);
end
%=============================================================================

