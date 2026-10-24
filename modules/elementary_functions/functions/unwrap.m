%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function q = unwrap(p, tol, dim)
  narginchk(1, 3);
  if nargin < 2 || isempty(tol)
    tol = pi;
  end
  if nargin < 3 || isempty(dim)
    dim = find(size(p) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  q = p;
  if size(p, dim) < 2
    return
  end
  dp = diff(p, 1, dim);
  % wrap each difference into the interval (-pi, pi]
  wrapped = mod(dp + pi, 2 * pi) - pi;
  atBoundary = (wrapped == -pi) & (dp > 0);
  wrapped(atBoundary) = pi;
  correction = wrapped - dp;
  % only correct jumps whose magnitude exceeds the tolerance
  correction(abs(dp) < tol) = 0;
  cumulative = cumsum(correction, dim);
  colons = repmat({':'}, 1, ndims(p));
  colons{dim} = 2:size(p, dim);
  q(colons{:}) = p(colons{:}) + cumulative;
end
%=============================================================================
